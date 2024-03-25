module ColorWheel exposing
    ( Msg
    , State
    , getElementDimensions
    , initialState
    , subscriptions
    , trackMouseOutsideWheel
    , update
    , view
    , withColor
    )

import ABALA.Core exposing (PathSegment(..), PathSegmentString)
import Browser.Dom exposing (Element)
import Browser.Events
import Colors exposing (Color)
import Css
    exposing
        ( backgroundColor
        , backgroundImage
        , backgroundRepeat
        , backgroundSize
        , block
        , borderRadius
        , cover
        , display
        , hidden
        , inlineBlock
        , linearGradient2
        , overflow
        , paddingTop
        , pct
        , px
        , toRight
        , url
        )
import Html.Styled exposing (div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Json.Decode as Decode exposing (Decoder)
import Svg.Styled exposing (Svg, circle, defs, line, path, radialGradient, stop, svg)
import Svg.Styled.Attributes
    exposing
        ( cx
        , cy
        , d
        , fill
        , filter
        , gradientUnits
        , height
        , id
        , offset
        , opacity
        , pointerEvents
        , r
        , stopColor
        , stroke
        , strokeWidth
        , style
        , viewBox
        , width
        , x1
        , x2
        , y1
        , y2
        )
import Svg.Styled.Events exposing (on, onMouseDown, onMouseOut, onMouseUp)
import Svg.Styled.Lazy exposing (lazy2, lazy5, lazy6)
import Task


type alias Position =
    ( Float, Float )


type alias State =
    -- TODO make it opaque
    { id : String
    , elementDimensions : Maybe Element
    , mouseTracking : Bool
    , color : Color

    -- For Development
    , mousePosition : Position
    , numberOfSlices : Float
    , blur : Float
    , dynamic : Bool

    -- Config
    , trackMouseOutsideWheel : Bool
    , sameHeightAsWidth : Bool
    }


initialState : String -> Maybe Color -> State
initialState id_ maybeColor =
    { id = id_
    , elementDimensions = Nothing
    , mouseTracking = False
    , color = Maybe.withDefault (makeColor ( 0, 0 )) maybeColor

    -- For Development
    , mousePosition = ( 0, 0 )
    , dynamic = False

    -- Magic optimal value after profiling performance on FF75 and Chrome
    --, numberOfSlices = 97
    --, blur = 8
    , numberOfSlices = 100
    , blur = 2

    -- Config
    , trackMouseOutsideWheel = True
    , sameHeightAsWidth = True
    }


type Msg
    = GotElementDimensions (Result Browser.Dom.Error Element)
    | GotMousePosition Position
    | StartedMouseTracking
    | StoppedMouseTracking
    | SetOpacity Float
      -- For Development
    | SetNumberOfSlices Float
    | SetBlur Float
    | ToggledDynamic


update : Msg -> State -> ( State, Maybe Color, Cmd Msg )
update msg model =
    let
        getDimensionsIfNotSet =
            if model.elementDimensions == Nothing then
                getElementDimensions model

            else
                Cmd.none
    in
    case msg of
        SetOpacity opacity ->
            let
                { h, s } =
                    Colors.toHsva model.color

                color =
                    Colors.hsv h s opacity
            in
            ( { model | color = color }, Just color, Cmd.none )

        GotMousePosition relativePosition ->
            let
                color =
                    computeColor model relativePosition
            in
            ( { model
                | mousePosition = relativePosition
                , color = color
              }
            , Just color
            , getDimensionsIfNotSet
            )

        GotElementDimensions result ->
            ( { model | elementDimensions = Result.toMaybe result }, Nothing, Cmd.none )

        StartedMouseTracking ->
            ( { model | mouseTracking = True }, Nothing, Cmd.none )

        StoppedMouseTracking ->
            ( { model | mouseTracking = False }, Nothing, Cmd.none )

        --
        --
        --
        {--| For Development --}
        SetNumberOfSlices n ->
            ( { model | numberOfSlices = n }, Nothing, Cmd.none )

        SetBlur b ->
            ( { model | blur = b }, Nothing, Cmd.none )

        ToggledDynamic ->
            ( { model | dynamic = not model.dynamic }, Nothing, Cmd.none )


subscriptions : State -> Sub Msg
subscriptions model =
    if model.mouseTracking then
        Sub.batch
            [ Browser.Events.onMouseMove (Decode.map GotMousePosition mouseInfoDecoder)
            , Browser.Events.onMouseUp (Decode.succeed StoppedMouseTracking)
            ]

    else
        Sub.none



-- CONFIG


trackMouseOutsideWheel : Bool -> State -> State
trackMouseOutsideWheel shouldTrack model =
    { model | trackMouseOutsideWheel = shouldTrack }


withColor : Color -> State -> State
withColor color model =
    { model | color = color }



-- COLOR


computeColor : State -> Position -> Color
computeColor model ( x, y ) =
    case model.elementDimensions of
        Just dimensions ->
            let
                ( w, h ) =
                    ( dimensions.element.width, dimensions.element.height )

                clickedVec =
                    toPolar ( (x - w / 2) / (w / 2), (y - h / 2) / (h / 2) )
                        |> Tuple.mapFirst (clamp 0 1)
            in
            updateColor clickedVec model.color

        Nothing ->
            model.color


updateColor : Position -> Color -> Color
updateColor ( r, theta ) color =
    let
        { v } =
            Colors.toHsva color
    in
    Colors.hsv theta r v


makeColor : Position -> Color
makeColor ( r, theta ) =
    Colors.hsv theta r 1


toPolarPosition : Color -> Position
toPolarPosition color =
    let
        { h, s } =
            Colors.toHsva color

        ( theta, radius ) =
            ( h, s )
    in
    ( radius, theta )



-- VIEW


view : State -> Svg Msg
view model =
    let
        { v } =
            Colors.toHsva model.color

        view_ =
            if model.dynamic then
                viewDynamic

            else
                viewStatic
    in
    div
        [ css
            [ Css.height (pct 100)
            , Css.width (pct 100)
            ]
        ]
        [ view_ model
        , gradientSliderInput SetOpacity v 0.0001 1.0001 0.01 (Colors.rangeValue model.color)
        ]



-- viewForDevelopment : State -> Svg Msg
-- viewForDevelopment model =
--     let
--         { v } =
--             Colors.toHsva model.color
--         view_ =
--             if model.dynamic then
--                 viewDynamic
--             else
--                 viewStatic
--     in
--     div
--         [ css
--             [ Css.height (pct 100)
--             , Css.width (pct 100)
--             ]
--         ]
--         [ view_ model
--         , Html.Styled.text (String.fromFloat v)
--         , gradientSliderInput SetOpacity v 0.0001 1.0001 0.01 (Colors.rangeValue model.color)
--         , Html.Styled.text (String.fromFloat model.numberOfSlices)
--         , sliderInput SetNumberOfSlices model.numberOfSlices 1 180 1
--         , Html.Styled.text (String.fromFloat model.blur)
--         , sliderInput SetBlur model.blur 0 40 1
--         , Html.Styled.text (String.fromFloat v)
--         , sliderInput SetOpacity v 0.0001 1.0001 0.01
--         , Html.Styled.button [ Html.Styled.Events.onClick ToggledDynamic ]
--             [ Html.Styled.text
--                 (if model.dynamic then
--                     "Make static"
--                  else
--                     "Make dynamic"
--                 )
--             ]
--         ]


viewStatic : State -> Svg Msg
viewStatic model =
    lazy5 viewStaticEager model.id model.mouseTracking model.color model.trackMouseOutsideWheel model.sameHeightAsWidth


viewStaticEager : String -> Bool -> Color -> Bool -> Bool -> Svg Msg
viewStaticEager id_ mouseTracking color mouseTrackingOutsideWheel sameHeightAsWidth =
    let
        value =
            .v (Colors.toHsva color)

        outerWidthAndHeight =
            if sameHeightAsWidth then
                Css.batch []

            else
                Css.batch
                    [ Css.height (pct 100)
                    , Css.width (pct 100)
                    ]

        outerDivAttributes =
            [ css
                [ outerWidthAndHeight
                , borderRadius (pct 50)
                , overflow hidden
                , backgroundColor (Colors.toCssColor Colors.black)
                , Css.position Css.relative
                , Css.border3 (px 1) Css.solid (Colors.toCssColor Colors.black)
                ]
            , id id_
            ]

        innerHeight =
            Css.batch <|
                if sameHeightAsWidth then
                    -- N.B. Padding in percentage is computed based on width value, so we force the height to be
                    -- the same as the width. This *would* affect its children's positions, but there are none here.
                    [ paddingTop (pct 100) ]

                else
                    [ Css.height (pct 100) ]

        innerDivAttributes =
            [ css
                [ Css.width (pct 100)
                , innerHeight
                , backgroundColor (Colors.toCssColor Colors.white)
                , backgroundRepeat Css.round
                , backgroundSize cover
                , backgroundImage (url colorWheelDataImage)
                , Css.borderRadius (pct 50)
                , Css.opacity (Css.num value)
                ]
            , id (id_ ++ "static")
            ]

        innerDivEventListeners =
            -- TODO I'm not sure why I was using `stopPropagationOn` but I'm leaving as a comment in case a bug appears.
            --[ ( True, stopPropagationOn "click" (alwaysStopPropagation (Decode.map GotMousePosition mouseInfoDecoder)) )
            [ ( True, on "click" (Decode.map GotMousePosition mouseInfoDecoder) )
            , ( True, onMouseDown StartedMouseTracking )
            , ( True, onMouseUp StoppedMouseTracking )
            , ( not mouseTrackingOutsideWheel, onMouseOut StoppedMouseTracking )
            , ( mouseTracking, on "mousemove" (Decode.map GotMousePosition mouseInfoDecoder) )
            ]
                |> List.filter Tuple.first
                |> List.map Tuple.second

        ( x, y ) =
            fromPolar (toPolarPosition color)
    in
    div outerDivAttributes
        [ div (innerDivAttributes ++ innerDivEventListeners) []
        , crosshair x y value
        ]


crosshair : Float -> Float -> Float -> Svg msg
crosshair x y opacity =
    let
        strokeWidth_ =
            String.fromInt 8

        color =
            if opacity > 0.5 then
                Colors.black

            else
                Colors.white

        strokeColor =
            Colors.toString <| Colors.updateAlpha 0.52 color

        finalSize =
            15

        size =
            90

        viewBox_ =
            "0 0 " ++ String.fromInt size ++ " " ++ String.fromInt size

        center =
            String.fromInt (size // 2)

        radius =
            String.fromInt (size // 3)

        sizeStr =
            String.fromInt size
    in
    div
        [ css
            [ Css.position Css.absolute
            , Css.left (Css.calc (pct (x * 50 + 50)) Css.minus (px (finalSize / 2)))
            , Css.top (Css.calc (pct (y * 50 + 50)) Css.minus (px (finalSize / 2)))
            , Css.height (px finalSize)
            , Css.width (px finalSize)
            , Css.pointerEvents Css.none
            ]
        ]
        [ svg [ viewBox viewBox_ ]
            [ circle
                [ cx center
                , cy center
                , r radius
                , fill "none"
                , stroke strokeColor
                , strokeWidth strokeWidth_
                ]
                []
            , line
                [ x1 "0"
                , x2 sizeStr
                , y1 center
                , y2 center
                , stroke strokeColor
                , strokeWidth strokeWidth_
                ]
                []
            , line
                [ x1 center
                , x2 center
                , y1 "0"
                , y2 sizeStr
                , stroke strokeColor
                , strokeWidth strokeWidth_
                ]
                []
            ]
        ]


gradientSliderInput : (Float -> msg) -> Float -> Float -> Float -> Float -> Colors.Range -> Svg msg
gradientSliderInput inputToMsg oldValue min_ max_ step_ colorRange =
    div
        [ css
            [ display inlineBlock
            , Css.width (pct 100)
            , Css.height (px 30)
            , backgroundImage <|
                linearGradient2 toRight
                    (Css.stop <| Colors.toCssColor colorRange.start)
                    (Css.stop <| Colors.toCssColor colorRange.end)
                    []
            , Css.borderColor <| Colors.toCssColor Colors.gray
            , Css.borderRadius (px 3)
            , Css.marginTop (px 10)
            , Css.padding3 (px 1) (px 4) Css.zero
            , Css.boxSizing Css.borderBox
            , Css.border3 (px 1) Css.solid (Colors.toCssColor Colors.black)
            ]
        ]
        -- put 30 px on slider height
        [ sliderInput inputToMsg oldValue min_ max_ step_ ]


sliderInput : (Float -> msg) -> Float -> Float -> Float -> Float -> Svg msg
sliderInput msg oldValue min_ max_ step_ =
    let
        onInputCallback stringValue =
            case String.toFloat stringValue of
                Just newValue ->
                    msg newValue

                Nothing ->
                    msg oldValue
    in
    Html.Styled.div
        [ css [ Css.width (pct 97), overflow hidden, Css.margin2 Css.zero Css.auto ] ]
        [ Html.Styled.input
            [ Html.Styled.Attributes.type_ "range"
            , Html.Styled.Attributes.min (String.fromFloat min_)
            , Html.Styled.Attributes.max (String.fromFloat max_)
            , Html.Styled.Attributes.step (String.fromFloat step_)
            , Html.Styled.Attributes.value <| String.fromFloat oldValue
            , Html.Styled.Events.onInput onInputCallback

            -- TODO Change height to a variable based on the wrappig div height.
            , css [ display block, Css.width (pct 100), Css.height (px 27) ]
            ]
            []
        ]



-- CMD
-- TASK AND
-- MOUSE
-- DECODER


getElementDimensions : State -> Cmd Msg
getElementDimensions model =
    Task.attempt GotElementDimensions (Browser.Dom.getElement model.id)


mouseInfoDecoder : Decoder Position
mouseInfoDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)



-- HELPERS


strTruncateFromFloat : Int -> Float -> String
strTruncateFromFloat precision float =
    let
        tens =
            toFloat (10 ^ precision)

        truncatedFloat =
            -- Prevent bugs where string representation is "7.1234134e-15" turned into "7.1234"
            toFloat (floor (float * tens)) / tens

        floatString =
            String.fromFloat truncatedFloat

        truncate string =
            if String.length string <= precision then
                string

            else
                String.dropRight (String.length string - precision) string
    in
    case String.split "." floatString of
        integer :: [] ->
            integer

        integer :: [ decimal ] ->
            integer ++ "." ++ truncate decimal

        _ ->
            floatString



-- DYNAMIC VIEW (FOR DEVELOPMENT)
-- VIEW


viewDynamic : State -> Svg Msg
viewDynamic model =
    lazy6 viewDynamicEager
        model.id
        (floor model.numberOfSlices)
        model.mouseTracking
        model.blur
        model.color
        model.trackMouseOutsideWheel



-- alwaysStopPropagation : Decoder a -> Decoder ( a, Bool )
-- alwaysStopPropagation =
--     Decode.map (\msg -> ( msg, True ))


viewDynamicEager : String -> Int -> Bool -> Float -> Color -> Bool -> Svg Msg
viewDynamicEager id_ numberOfSlices mouseTracking blur color mouseTrackingOutsideWheel =
    let
        scale =
            2

        value =
            .v (Colors.toHsva color)

        divAttributes =
            [ css
                [ borderRadius (pct 50)
                , overflow hidden
                , backgroundColor (Colors.toCssColor Colors.black)
                , Css.position Css.relative

                --, Css.height (pct 100)
                --, Css.width (pct 100)
                ]
            , id id_
            ]

        svgAttributes =
            -- TODO add xmlns="http://www.w3.org/2000/svg"
            [ viewBox "-1 -1 2 2"
            , height "100%"
            , width "100%"
            , opacity (String.fromFloat value)
            , filter ("blur(" ++ String.fromFloat blur ++ "px)")
            , style <|
                ""
                    ++ "display: block;"
                    ++ "background-color: white;"
                    ++ "boder-radius: 50%;"
                    ++ ("transform: scale(" ++ String.fromInt scale ++ ");")
                    -- N.B. We need that the scaled element do *not* react to mouse events
                    -- otherwise it would affect `offsetX` and `offsetY` from which we are
                    -- calculating the mouse position and therefore the selected color!
                    --
                    -- Test here: https://jsfiddle.net/dn9jkr7a/1/
                    ++ "pointer-events: none;"

            -- TODO change this name or remove it entirely
            , id "MainSVG"
            ]

        divEventListeners =
            -- TODO I'm not sure why I was using `stopPropagationOn` but I'm leaving as a comment in case a bug appears.
            --[ ( True, stopPropagationOn "click" (alwaysStopPropagation (Decode.map GotMousePosition mouseInfoDecoder)) )
            [ ( True, on "click" (Decode.map GotMousePosition mouseInfoDecoder) )
            , ( True, onMouseDown StartedMouseTracking )
            , ( True, onMouseUp StoppedMouseTracking )
            , ( not mouseTrackingOutsideWheel, onMouseOut StoppedMouseTracking )
            , ( mouseTracking, on "mousemove" (Decode.map GotMousePosition mouseInfoDecoder) )
            ]
                |> List.filter Tuple.first
                |> List.map Tuple.second

        ( x, y ) =
            fromPolar (toPolarPosition color)
    in
    div (divAttributes ++ divEventListeners)
        [ div [ css [ Css.height (pct 100), Css.width (pct 100) ] ]
            [ svg svgAttributes [ lazy2 pizza numberOfSlices scale ]
            , crosshair x y value
            ]
        ]


pizza : Int -> Float -> Svg msg
pizza numberOfSlices scale =
    Svg.Styled.g []
        (List.range 0 (numberOfSlices - 1)
            |> List.concatMap (pizzaSlice numberOfSlices scale)
        )


pizzaSlice : Int -> Float -> Int -> List (Svg msg)
pizzaSlice totalNumberOfSlices scale sliceNumber =
    let
        sizeAngle =
            (2 * pi) / toFloat totalNumberOfSlices

        rotationAngle =
            toFloat sliceNumber * sizeAngle

        endColor =
            Colors.toString <| makeColor ( 1, rotationAngle )

        startColor =
            Colors.toString <| makeColor ( 0, rotationAngle )

        id_ =
            "colorId_" ++ String.fromInt sliceNumber
    in
    [ toppings startColor endColor id_ scale
    , path
        [ d (betterDough sizeAngle rotationAngle)
        , fill <| "url(#" ++ id_ ++ ")"

        --, Svg.Styled.Attributes.strokeWidth "0.001"
        --, Svg.Styled.Attributes.stroke "black"
        -- N.B. This is important to allow the svg being always the target of
        --      the click, so that offsetX and offsetY are meaningful.
        --      (Otherwise any pizza slice could be the target and the basis
        --      for the offset calculation; not what we want.)
        , pointerEvents "none"
        ]
        []
    ]


betterDough : Float -> Float -> PathSegmentString
betterDough sizeAngle rotationAngle =
    let
        _ =
            ( sizeAngle, rotationAngle )

        radius =
            -- Greater than the pizza radius of 1: it'll be trimmed via border-radius 50%.
            1

        sliceSize =
            -- More than it's supposed size  because otherwise we would see white segments
            -- dividing the slices. My guess is it's a floating point precision thing.
            sizeAngle + 0.02

        ( x1, y1 ) =
            ( radius, rotationAngle )
                |> fromPolar

        ( x2, y2 ) =
            ( radius, rotationAngle + sliceSize )
                |> fromPolar

        moveTo =
            "M 0 0"

        lineTo1 =
            "L" ++ strTruncateFromFloat 4 x1 ++ " " ++ strTruncateFromFloat 4 y1

        ( ctrlX, ctrlY ) =
            ( radius, radius * tan (sliceSize / 2) )
                |> toPolar
                |> Tuple.mapSecond ((+) rotationAngle)
                |> fromPolar

        quadraticBezier =
            "Q"
                ++ (strTruncateFromFloat 4 ctrlX ++ " " ++ strTruncateFromFloat 4 ctrlY)
                ++ " "
                ++ (strTruncateFromFloat 4 x2 ++ " " ++ strTruncateFromFloat 4 y2)
    in
    moveTo ++ lineTo1 ++ quadraticBezier ++ "Z"


toppings : String -> String -> String -> Float -> Svg msg
toppings startColor endColor id_ scale =
    defs []
        [ radialGradient [ id id_, cx "0", cy "0", r (String.fromFloat (1 / scale)), gradientUnits "userSpaceOnUse" ]
            [ stop [ offset "0%", stopColor startColor ] []
            , stop [ offset "100%", stopColor endColor ] []
            ]
        ]



{--

                COLOR SLIDERS

--}
-- rgbSliders : (Color -> msg) -> Color -> Html msg
-- rgbSliders toMsg color =
--     let
--         { red, green, blue } =
--             Colors.toRgba color
--     in
--     div []
--         [ colorSlider (\input -> toMsg (Colors.updateRed input color)) red (Colors.rangeRed color)
--         , colorSlider (\input -> toMsg (Colors.updateGreen input color)) green (Colors.rangeGreen color)
--         , colorSlider (\input -> toMsg (Colors.updateBlue input color)) blue (Colors.rangeBlue color)
--         ]
-- hslSliders : (Color -> msg) -> Color -> Html msg
-- hslSliders toMsg color =
--     let
--         { hue, saturation, lightness } =
--             Colors.toHsla color
--     in
--     div []
--         [ colorSlider (\input -> toMsg (Colors.updateHue input color)) hue (Colors.rangeHue color)
--         , colorSlider (\input -> toMsg (Colors.updateSaturation input color)) saturation (Colors.rangeSaturation color)
--         , colorSlider (\input -> toMsg (Colors.updateLightness input color)) lightness (Colors.rangeLightness color)
--         ]


{-| TODO move this into Colors.elm?
-}



-- colorSlider : (Float -> msg) -> Float -> Colors.Range -> Html msg
-- colorSlider inputToMsg oldValue colorRange =
--     div []
--         [ div
--             [ css
--                 [ display inlineBlock
--                 , Css.width (pct 90)
--                 , Css.height (px 30)
--                 , backgroundImage <|
--                     linearGradient2 toRight
--                         (Css.stop <| Colors.toCssColor colorRange.start)
--                         (Css.stop <| Colors.toCssColor colorRange.end)
--                         []
--                 ]
--             ]
--             -- put 30 px on slider height
--             [ sliderInput inputToMsg oldValue 0 1 0.0001 ]
--         ]


colorWheelDataImage : String
colorWheelDataImage =
    "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgMCAgMDAwMEAwMEBQgFBQQEBQoHBwYIDAoMDAsKCwsNDhIQDQ4RDgsLEBYQ"
        ++ "ERMUFRUVDA8XGBYUGBIUFRT/2wBDAQMEBAUEBQkFBQkUDQsNFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBT/wAAR"
        ++ "CAEDAQMDAREAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKB"
        ++ "kaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWm"
        ++ "p6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREA"
        ++ "AgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpj"
        ++ "ZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oA"
        ++ "DAMBAAIRAxEAPwD5imir/RiMj+fIyKUsVbxkdMZFKeKuiLOmMijNFXRFnVGRnzxda6Is64yKE8VdMWdcZFGVOa6EzthIrFcGtbndCQ5RzSZ3wZaiHNYs9Kmy"
        ++ "7CtYSZ6VNl+GLOK5pM9Kmy/BD0rnlI9OnIvwwH0rmlI9Omy/Db1zykelTZditvaueUz1KbLkVt7Vi5np02W47b2rFyPUpssx2+O1ZOR6tNlmO39qxcj1KbLE"
        ++ "cFZuR6lMsJDWTkerTRYjjxWbZ6dNFhErJs9KmidFxWMmehBEoHFc0mdkUOzXJKR0JCE4rinI0SGE1xTkUMJxXHORQ0muOUihhOK5ZSKG5rn5ijz6WKv6DjI/"
        ++ "xIjIpzRVvGR0xkUZoa6IyOmMijPFXRGR1RkUJoq6IyOyMjOnirqjI64SKE0PWuiMjthIqvEa2TO6EgSLmhs9CnItQQ81jKR6VORpW8GccVyykelTkadvbH0r"
        ++ "llM9KnI07e19q5JTPTps0YLT2rllM9OmzQgs+nFcspnp05F+Gz6cVzymepTZcis/asHUPUpstx2ftWTmepTZYSz9qycz1aTJ0s/as3M9akyZbTHas3M9akyV"
        ++ "bbHapcz1qRKtv7VDkerSJFhIrNyPUpkgTFYykejAXbXLKR1xGniuOcjdIaTXDORYwmuKcihhNcU5FIYTXJKRQ0muSUihuTXO5DscdLDX9DKR/h7GRSmhreMj"
        ++ "pjIpTQ9a6IyOqMihNDXRGR1RkUJ4faumMjrjIoTQdeK6IyOyEilLbZ7V0KR3QkVmtfatVM7oMVLQ+lJzPRgy3Bac9KxlM9GmzTtrTOOK5JzPTps1raz6cVxz"
        ++ "menTZrW1n04rjnM9OmzTt7LpxXJKoenTZowWXTiuWVQ9Omy/DZe1c0qh6lNl2Ky9qwdQ9OmyzHZcdKydQ9Wkywll7Vk6h6tJk62XtUOoetSZItl7VHtD16TH"
        ++ "iz9qXOevSYotcdqTmevSYG3x2rNzPWpsY0OKylM9GDInTFcspnZEhYYrjnM3RGxxXDORaIya4ZzLQwmuOcyhpNccpFDCa5JSKG5Nc7kM5+WGv6KUj/DGMilN"
        ++ "D7VvGR0xkUpoa6IyOmMijNBXRGR1xkUZret4yOuEinJa57VupnbCRXezz2rVVDvhIjNhntVe0PQgxy2HtSdQ9Cmy1DY+1YyqHo02adtZdOK5Z1D06bNa1s+n"
        ++ "FcU6h6dNmtbWXTiuOcz06bNS3svauSVQ9OnI0oLL2rllUPTpsvw2XTiuaVQ9Smy7FZe1YOoepSZajsvasXUPVpMnSy9qzdQ9WnIlWy9qh1D1qTJBZe1Tznr0"
        ++ "mL9j9qXtD16TA2ntS9oexSZE9rjtUOoerTkV5YMVjKoejCRTlixmuaVQ7YsqSpiuOczqiys/Fcc5myImOK4pzLIya4pzLGk1yTmUNJrklMqw3JrncxlCWH2r"
        ++ "+jlI/wAJIyKcsFbRkdMZFKaD2rojI6YzKUtvW6kdcJFWS19q1UzthIhNlntWntDuhIb9gz2qvaHoQYf2d7Ue1PRpscune1T7U9CmyxFp+O1ZuoejTZft7Hkc"
        ++ "VzSqHp02alrZdOK5J1D06bNe2sunFcU6h6dNmpb2XTiuSVQ9KmzSgsvauSVQ9OmzQhsuOlc8qh6lNlyKy9q53UPUpstJZe1ZOoerSZOtlntWbqHq02SrZe1R"
        ++ "7Q9akx4s/ap5z2KTFNn7UvaHr0mMa09ql1D16UivLbe1ZuoepTkUZ7frWMqh6EJFCeGuaVQ7oSM6eLFcsqh2RkUZVxmuWVQ6YsrPXHKZuiJjiuOczRDCa45T"
        ++ "KsMJrllMoTJrn5x2JJYPav6SUj/BeMynLBWykdEZlSW39q3UjqjIqva57VqpnXCRGbLPar5zvhIBYZ7UvaHoU5Dhp3tR7U9Gmxw072qfano02PGne1L2p6NN"
        ++ "kqaf7VDqno02W4bDGOKxlUPRps0ray6cVySqHp02atrZ9OK451D06bNW3sunFckqh6dNmnBZdOK5JVD06bNCCy9q5pVD06bLkVljtWDqHqU2Wo7L2rF1D1aT"
        ++ "J1svaodQ9akyQWXtUe0PWpMd9j9qXtD2KTENp7VLqHr0pEMlr7VDqHq05FOa29qydQ9KEjPuLfrxXPKod8JGZcQda55VDuhIzLiHrXNKod0JGbPH1rmlUO2L"
        ++ "KMq4Ncsqh1RZWeuWUzdERNccplpDCa5ZTKEzWDmOxsSwe1f0qpH+BMZlSW39q2UjojMrPa57VqpnXCRGbPPar5zthIUWOe1L2h6FORIun+1Q6h6NORKun+1S"
        ++ "6p6NNkg072qfanpU2PGne1T7U9GmyRdO9ql1T0abJ4tPx2rN1D0abL8FjjHFc0qh6VNmnbWXTiuSdQ9OnI1bay6cVySqHp02akFl7VySqHp02aENl04rmlUP"
        ++ "UpsuR2XtWDqHp02WksvasXUPVpMmWy9qh1D1qTJBZ+1Tznr0mBs/ap9oevSYx7THaodQ9anIqy23tWbqHp05FGe39qwlVPRhIzLiDrxXPKqd8JmXcw9a55VD"
        ++ "vhIybmHrxXPKod8JGXcRVzyqHdCRmTpjNc0qh3QZRlGK5pVDqiV3Nc0pmqIy1ckpljcisOcqx10lv7V/TKkf8/MZld7b2rZTOmMyI2me1VznZCQosvaj2h3w"
        ++ "kSpYe1Q6h6NORMlh7Vm6h6VOROun+1Q6p6VNkq6d7VDqnpU2SLp2e1T7U9GmyQad7VHtT0qbJU0/2qXVPQpstQ2GO1YyqHpU2aNvZYxxXLKoenTZqW1l7VyS"
        ++ "melTZqW9l04rklUPTps0ILLpxXLKoepTZdisvasHUPUpyLKWXtWTqHq0mTLZe1Q6h61Jj/sftU+0PXpMa1p7VDqHr0mQSWvtUOoepTkU57f2rGVQ9GEjNuIO"
        ++ "vFc8qh6EJGXcwdeK55VDvhIybmHrXPKod8JGTdQ9awlUPQpyMi6i61zyqHoQkZNzHjNYSqHoQZmzriueVQ7YspScVzSqHSiFjXNKZohu6sPaFWPRXtq/p5SP"
        ++ "+eqMyE2ue1XznVGYCz9qOc7YSJUsfaodQ9GnIsJY+1ZuoelTkWEsPasnUPTpyJ0sPas3UPSpyJ00/wBqzdU9KnIlXTvaodU9KnIkXTvapdU9KmyRdO9ql1T0"
        ++ "abLEen47Vm6h6NNl2Cx6cVzyqHpU2adtZdOK5JVD06cjTt7LgcVyyqHp02aMFl7VyyqHp02XY7L2rB1D1Kciyll7Vk6h6tJkosvaodQ9ekxTZ+1Q6h69KRG9"
        ++ "rjtUOoerTkVJrasnUPSpyM+4t6wlUPQhIy7mDrXPKoehCRlXMPWueVQ74SMi6h61hKoehCRkXUXWsHUPRpyMa6i61hKoejTkY91H1rnlUPRpsyblOtYSqHoQ"
        ++ "ZnzDFYSqHZEqua5pVDdEefesfaF2PXmtfav6iUz/AJ2ozGi0z2p851QmSJZ89Kl1DvpyLEdlntWTqHp05FmOxz2rJ1D06cizHY+1ZOoenTkWI7D2rJ1D06ci"
        ++ "wlh7Vm6h6dNk6af7Vm6h6VNkq6f7VDqnpU2Srp3tUuqejTZKmn47VDqno02WorDnpWMqh6VNmhb2PTiuaVQ9KmzTt7L2rklUPTps0YLLpxXNKoenTZdisunF"
        ++ "YOoerTZYSy9qydQ9WkyT7H7VDqHrUmNa0x2qHUPWpyK8ttjtWTqHp05FGe39qxlUPShIzbmDrXPKoehCRk3MPWsJVDvhIyLqLrXPKoehTkZF1F1rCVQ9GnIx"
        ++ "ruLrWDqHpU5GNdx9awlUPSpsxbuPrWDqHpU2Y90nWsXUPRpsyrgYzXPKod8ClJ1rCVQ6kRGsecs93NrntX9Tc5/zlxmKtpntSczshMnjs+elZuoejTkWYrL2"
        ++ "rJ1D06ci3HZe1YuoepTkWo7HPasnUPUpyLMdj7Vi6h6dORZjsM9qydQ9OnIsJYe1ZuoenTkTpp/tWbqHpU2Srp/tUOqelTZKmne1Q6p6VNliPT/as3UPRpsu"
        ++ "QWOCOK55VD0qbNG3sunFc0qh6VNmjDZe1csqh6lNl2Oy9qwdQ9SnInWz9qzdQ9Wkx32T2rN1D16UiKS19qzdQ9SnIqTW+M8Vk6h6VORm3EHWueVQ9GnIyrmH"
        ++ "rxWEqh3wkZF1F1rCVQ9GnIx7qLrXPKoejTkY93H1rB1D0qbMW7j61g6h6VNmLdp1rF1D0qbMS8TrWDqHp02Yt2nWsXUPTpsx7kYzWEqh6MGZ01YOodkSA9ay"
        ++ "9obH0kLX2r+qec/5uozJEs/apczuhMsxWftWTmelTkWorP2rF1D1Kci5FZe1YuoepTkWYrL2rF1D1Kci3HY+1YuoepTkWo7H2rJ1D1KcizHY+1YuoenTkWEs"
        ++ "Pas3UPTpyJ0sPas3UPSpsmTT/aodU9OmydNP9qzdQ9Gmy1Dp+O1YyqHpU2X4LHpxXPKoelTZoQWPtXLKoenTZdjsuOlYOoepTZKLPA6Vm6h6tKQjWg9KzdQ9"
        ++ "anIqzW2O1ZOoenTkZ9xB1rCVQ9GnIy7mHrWEqh6MJGRdRdawdQ9CnIx7uLrWDqHo05GLdx9awdQ9OmzGu4+tYSqHpU2Yl2nWsHUPTpsxLtetYuoenTZiXida"
        ++ "wdQ9SmzDvF61jKoenTZjXQ61g6h6dMy5+prF1DugVSeay9obn1YLX2r+ruc/5q4TJ47TPas3M9CnMtR2ntWLmenTkW4rP2rGUz1Kcy5HZ+1YOZ6tORais/as"
        ++ "ZVD1Kci3HZcdKxdQ9SnItxWXTisXUPUpyLUdl7Vi6h6lORajsfasnUPUpyLKWPtWTqHp05E6WHtWbqHp05FiPT/asnVPSpssxaf7Vm6p6VNlyGw6cVzyqHpU"
        ++ "2X4bH2rnlUPTpyLcdj7Vg6h6lNjzZ+1ZOoepTkRSWuO1ZuoepTkULi3wDxWUqh6VORl3UOM1zyqHpU5GPdRdawlUPRpyMe7j61g6h6VNmLdp1rB1D0qbMS8T"
        ++ "rWLqHp02Yl4vWsJVD06bMS8XrWDqHqU2Yd4vWsXUPUpsw7xetYOoenTZh3g61i6h6tMxLsdawdQ9OmZFx1NYuoehAqE81n7Q6D7HW09q/rJzP+ZyEyxHae1Z"
        ++ "OZ6NOZbitPasXM9SnMtxWntWLmepSmXIrP2rBzPVpzLkVn7Vg5nqU5luKzz2rFzPWpyLcVn7Vi5nqU5luOz9qxcz1Kci3HZe1YOoepTkWo7LPasnUPUpyLUV"
        ++ "j7Vi6h6lORZjsOnFZOoenTkWY7DpxWTqHp02W4rD2rF1D0qbLkVj7VzyqHpU2Wls+OlYuoepTkI9rgdKydQ9OnIqT2/FZOoenTkZd1D1rGVQ9KnIx7uLrXPK"
        ++ "oelTkYt2nWsHUPTpsxbtOtYuoenTZiXa9awdQ9OmzDvF61i6h6lNmHeL1rB1D1KbMO8HWsHUPUpsw7wdaxdQ9SmYV4OtYOoerTZh3g61i6h6dMw7zvWDqHq0"
        ++ "zGuTyaxdQ9GBSJ5NZ+0OpH3Alt7V/W7mf8xkJlmO19qycz0acy3Fa+1YymerTkXIrb2rBzPVpSLkVr7VhKZ6tORcitfasHM9WlIuRWvtWDmerTmXIrX2rFzP"
        ++ "UpyLcVp04rFzPVpyLkVp7Vg5nq0pFqO09qwdQ9SlIuR2fTisXUPVpyLcVn7Vi6h6lORbisvasXUPUpyLUdl7Vk6h6VNlqOzx2rCVQ9OmyUWgA6Vi6h6VORDL"
        ++ "b4zxWLqHp05Gfcw4zWTqHpU5GPdx9awdQ9OmzEvE61i6h6lNmFeL1rnlUPVpsxLxetYuoenTZh3g61g6h6lMwrwdaxdQ9WmzDvB1rCVQ9SmYV4OtYOoerSMK"
        ++ "871i6h6tMwrzvWDqHp0zCvO9YuoerSMK871g6h6tMxrnvWLqHpUyiTzWftDqPvRIa/rp1D/mAhIsxQj0rJzPSpyLkUNYuoepSZcih6Vg5nq05FyKCsJTPVpy"
        ++ "LsUFYSmerTkXIoPasXM9WnIuRQCsHM9WlIuRQCsHUPVpSLkUArGVQ9WnItxQcjisHUPVpSLsUHtWDqHq05FyK3FYuoerSkXYrb2rB1D1Kci3HbDjisXUPUps"
        ++ "spbCsXUPSpsVoBisXUPSpsqTw9axdQ9OmzKuosZrF1D06bMS9j61i6h6lJmDep1rCVQ9WkzCvV61hKoerSZhXi9awdQ9SmzCvF61i6h6tMwr1etYOoerTMK9"
        ++ "XrWLqHq0mYV4OtYSqHqU2YV4OtYOoerTMG9HWsXUPVpGFeDrWDqHq0zCvB1rF1D1aZi3Q61i6h6VMoMDk1n7Q6z79jWv68dQ/wCXmDLUSisXUPRpsuxKKwdQ"
        ++ "9Wmy5EtYuoerTZciWsJVD1aTLsQrGVQ9Wky5EK53UPVpsuRIKxlUPVpsuxKKwdQ9Wmy5EtYSqHqU2W4l6Vi6h6tNl6JRWDqHq02XIVrB1D1abL0QrF1D1KbL"
        ++ "cSisXUPUpssKoIrF1D0qbBk4rF1D0qbKk6cGsXUPTpsybuPisXUPTpsw72POaxdQ9SkzBvYutYSqHrUpGDex1hKoerSkYN7H1rB1D1aUjCvIzzWDqHq02YV7"
        ++ "F1rFzPUpMwb2PrWDqHrUmYV7H1rFzPVpMwb2M81hKoerTkYV5GeawdQ9WnIwryPrxWEqh6tORg3kfWsXUPUpSMW6j5NYuoenCRQZME8VHOdaZ98Ia/rt1D/l"
        ++ "8gW4jWDqHo0y3EaxdQ9SkXYjWDqHq0y5EfesZVD1KbLsTdKwdQ9WmXImrB1D1aZdiasJVD1KZciasXUPVpsuRGsHUPVpstxN0rF1D1KbLsTVhKoepTZciasH"
        ++ "UPUpsuRP0rF1D1KbLkT1i6h6dNllHzWLqHp05Dyc1k6h6VNledcisHUPSpszLqPOaxdQ9OnIxbyLrxWLqHqU5GFexdaxcz1aUjCvYetYOoerSkYV5D1rFzPU"
        ++ "pyMK8g61g5nq05GFeQdeKxcz1KcjCvYOvFYOZ6tKZhXsHWsXM9WnIwb2DrWEpnq0pGFewdeKwcz1aUjBvYOtYOZ6tKZhXkHXisXM9WnMxbqDk8Vi5npwmZ7Q"
        ++ "cmo5zq5z7nRq/rh1D/mHgWYnrB1D0KZciesXUPUplyJ+lYOoerTLsT1i6h6lMuRPWDqHqUy5E9YSqHq02XInrF1D1KbLkUlYOoepTZcikrB1D06bLcUlYyqH"
        ++ "qU2XIpOlYSqHp02W45awlUPTpyLcUvvWDqHp05FuKWsnUPSpyLcclZOoenTkWA9YyqHp02MfkVhKoenTkU7hcg1i6h6VORkXcfWsnUPTpyMS8i61i6h6dORh"
        ++ "XkOc1i6h6tORiXkPWsnUPUpyMO8g61g5nqU5GFeQdeKxcz1KczCvIOvFYuZ6tORhXsHXisHM9SnIwry368Vi5nq05mFe2/XisHM9WlMwr2DrWDmerTmYV5b9"
        ++ "eKwcz1KczFurfrWTmelTmUGtuTxU851qZ9nI9f1k6h/zKxLMb1k6h30y3E9YOoenTZcikrB1D1KZcikrF1D1KbLkUlYOoepTZcikrCVQ9Smy3FJWDqHp02XI"
        ++ "pKxdQ9Smy3FLWEqh6dNluOWsJVD0qbLUU1YSqHpU5FuOasJVD0qci1FNWLqHo05FyKWsnUPTpyLkUtZuoepTkWklzWMqh6lORIWyK55VD06bIJeQawdU9Kmz"
        ++ "NuUzWTqHp02Y15H1rJ1D06cjFu4utZOoenTkYl3F1rJ1D06cjEvIetZOoenTkYd5D14rB1D1KcjDvIetYuoepTkYd5B1rFzPUpyMK8g68Vi5nq05mFeQdaxc"
        ++ "z1KczDvYOvFYOZ6tOZhXlv14rFzPUpzMe5t+TxWTmelCZRNvz0qec6lM+s1ev6udQ/5pIk8b1hKodsC3FJWLqHpU2W4pKwdQ9Omy5FJWLqHqU2XIpKwdQ9Om"
        ++ "y3FJWDqHqU2W4pawlUPTpstxy1jKoelTZbjlrB1D0qci1HLXPKoelTkWY5qwlUPQhIsxzVg6h6NORbim96ydQ9KnIuwy9KzdQ9SlIvQy1m6h6tORbjk4rGVQ"
        ++ "9SnInEmRXNKoepTkIxyK55VD0qbKc44NYuqelTZlXUfWsnVPTpsx7uPrUOoelTZi3cXWs3UPSpyMW7i61k6h6dORiXkPWsXUPUpyMO8h61i6h6dORiXkPXis"
        ++ "nUPUpyMK8h61i6h6lORh3kPWsHUPUpyMO8g68Vi5nqU5GHdwdaxcz1KcjIuLfk8VDmehCZSa256Uuc6ec+mg9f1Q6h/zaRJkesHUOqBZjkrB1D0abLcUlYuo"
        ++ "elTZbikrF1D06bLkUlYSqHp02W4pawlUPTpstRy1hKoelTkW45qwlUPSpyLMc1YSqHo05FmOaueVQ9CEizHNWEqh6EJFqKasXUPRpyLkMtZOoenSkXoZah1D"
        ++ "1aUi9DLWbqHq05FyKWsZVD1aciyklc0qh6lOQ/dkVyyqHp02QyjNYOoelTZn3CdaydU9KmzJuo+tQ6p6VNmNdx9ah1D0qcjGu4utZuoelTkYl3F1rJ1D06cj"
        ++ "FvIutYuoenTkYl5F1rJ1D1KcjDvIetYuoenTkYl5D1rJ1D1KcjDvIevFYuoepTkYt1BnNZOZ6VORlT29RznfCZUNvz0p850c59Abq/qJ1D/nDQ9XrF1DoiWI"
        ++ "5KwlUO6DLUUtYSqHo02W4pawdQ9Omy1HLWLqHpU2W45awlUPSpyLUc1YSqHpU5FmOaueVQ9CnIsxzVhKoejCRZjmrnlUO+EizHNWDqHoQkWopaydQ9KnIvQy"
        ++ "9KzdQ9SlIvwy9Kh1D1qTL0MlZuoetSZcilrGVQ9Smy3HJmuaVQ9SmyZXzXLKoenTYjHIrmlUPTpsqzDNYOoelTZmXKdah1T0qbMi7j61LqnpU2Y13H1rN1D0"
        ++ "abMW7j61m6h6VNmLdxdazdQ9OnIxbuLrWTqHp05GJeRdaydQ9OnIxLyLrWLqHp05GJdw9aydQ9SnIx7mDk8VHOejCRmTW/tU852xmVTbc9KrmN+c9o3V/T0q"
        ++ "h/zpIVXrCVQ2iTJJXPKodcGWI5awdQ9Cmy1HLWEqh6NORajlrCVQ9KnItRzVg6h6NORajmrCVQ9GEixHNXPKoehCRZSb3rnlUO+EixHN71hKod8JFqKasHUP"
        ++ "RpyLsMtZuoepTkXoJKl1D1qTL8MtQ6h61Jl6GWs3UPVpyLsUlYyqHq02W45a5pVD1KbLCSVySqHp02Sbs1yyqHp05EUnNc7qHpU2ULhc5rJ1T0qbMq6TrUe1"
        ++ "PSpsx7uPrU+1PRpsxruPrUOoelTZi3cfWs3UPSpsxbuPrWbqHpU5GLdxdaydQ9OnIxbuLrWTqHp05GNdQ9azdQ9KnIybiDOeKnnO+EjPmt/aqUzsjMqm356V"
        ++ "XMb856gWr+mpTP8AnhSDdXNKoaIesmK55VDoiydJcVzyqHbBliOWsHUPQhItRze9c8qh6FORZjmrCVQ9GEiyk3vXPKoehCRYjmrnlUO+Eiwk1YSqHfCRYjmr"
        ++ "ndQ9CnIuRS5rN1D0qci9BL0qPaHq0maEEtT7Q9eky/DJ0qHUPWpMvQyVm6h61NlyKWsZVD1KbLcUtc0qh6lNlqOSuScz06bJleuSdQ9OmwY1yyqHp02Vphms"
        ++ "HVPSpszblOtQ6p6NNmRdR9an2p6VNmPdR9an2p6NNmNdx9ah1D0qbMa7jrN1D0qcjFu4+tZuoelTkY11F1rN1D0qcjHuYc5qec9GEjMng60KZ3QmUZbetFI6"
        ++ "ozKxt+elXzG3OdyWr+lZTP8AnxsJurmlMtAHxXLKoaxJFkrnlUOqDJ0l965pVDthIsRzVzyqHfCRZjmrCVQ9CEiwk/vXPKod8JFhJ655VDvhMsRzVzuod8JF"
        ++ "qKasXUPRpyLsMtZ+0PUpSNCCSl7Q9eky/DLU+0PXpMvwy1DqHrUmXoZazdQ9WnIuRS1hKoerTZbilrnlUPTpstxy1ySqHp02WEkrklUPTpsk3ZFccqh6lORH"
        ++ "JzXPKoelTZRuFzWTqnpU2Zd0nWo9qejTZj3UfWk6p6NNmPdx9ah1T0qbMa7j61DqHo05GNdx9ah1D0qcjHuoutT7Q9GEjKuIetHOd8JGfNB14q1M7IzKUtv7"
        ++ "VspHTGZWMHNa8xtznSE1/SEpn+ANhpauWUxibq5JTNEAeuWVQ2iyVZcVyyqHVBkyTVzSqHbCROk1c8qh3QkWEn9655VDvhInSaud1TuhIsxTVg6h6FORchlr"
        ++ "N1D06ci/BL0qHUPXpM0YJOlL2h69Jl+CWpdQ9aky/DJUOoevSZdhkrJ1D1KbLsUtYyqHq05FuKWuaVQ9SnItRyVyyqHp02WY5K5J1D06bJ1euOdQ9OnIUtkV"
        ++ "ySqHp02V5hmsHVPRpszrlOtR7U9KmzIuk61PtT0abMe6j60nVPRpsx7qPrU+0PSpsyLqPrU+0PRhIybmHrRznfCRmTwdeK0UzthIozQda1UjrjIpSwVupHTG"
        ++ "ZXMHPStOY25y+TX9Gymf4HjSa5ZTKGFq45zKQm6uOUy0KHxXLKobxY9Za5JVDqjImWauWVQ7ISJkn9655VTthIsRzVzuod8JFuKasnUPSpyL0EvSo9oerSZo"
        ++ "QSdKn2h7FJmjBJS9oevSZfhkqfaHrUmXoZah1D1qUi7DLWTqHq05F2KWsJVD1KbLcUtc8qh6lNlqOSuWVQ9Omy1HJXJOZ6dNllJOK4p1D06bJA+RXJKoepTY"
        ++ "xzxXLKoelTZTnXOaydU9GmzLukzml7U9KmzIuo+tL2p6NNmRdR9aPaHoU2ZNzF1pqZ3wkZlxD1q1M7oSM6aCtlI7IyKM0FbxkdUZFOWCt1I6YyKxg56VrzG3"
        ++ "MRk1/RMpn+DowmuSUyrDSa5JzKSGE1xTmUhN1cc5miASVxzmbRY9Za5JVDpiyVJfeuWVQ7ISLMc1YOod9ORchlrN1D1KUjQgkqfaHr0maEEnSl7Q9ekzQgk6"
        ++ "VLqHsUmX4ZelS6h61Jl6GSs3UPVpMvRSVk6h6tNlyKWsZVD1KbLcUtc0qh6tNluOSuaVQ9Omy1HLXJOZ6dNlmOSuOcz06bJ1krinUPTpyHFq5JVD06bIJhnN"
        ++ "YOoejTZn3Cdan2h6VNmVcx9aPaHoQZk3MVWpnoQkZdxF1rRTO6EjOnhrdSOyEihNB14reMjrjIozQ10RkdUZFKWCuiMjpjIrGDnpWnMbcxkk4r+hZTP8KhpN"
        ++ "csplWGE1xzmUMJrjnMoYWrinMpCFq4ZzNEIHrinM2iyRZK5JTOqDLMUlYOZ302XYZOlRznq0maEEnSlznr0maEEnSp5z16TNCCSk6h69Jl+GWodQ9aky9DLW"
        ++ "bqHrUpF2GWsnUPVpyLkUtYyqHqU2XIpa5pVD06bLUctc0qh6lNlqOSuSVQ9Omy1HLXHOoenTZYSSuKcz06bJg+a5JTPTpsa3IrnlM9KmypMuaj2h6NNmdcR5"
        ++ "zVKZ6EJGXcRVtGR3QkZ08PWuiMjthIzpoa6IyOyMijND1rojI6oyKM0NdEZHVGRSlh9q3jI6YyKxh56VrzG/McuTiv6BlM/w2sNJrllMoYTXHKRQwmuOchpD"
        ++ "Ca4pyLSGk4rhnIpCZNcU5GqHKc1ySkdMCzEelc7kd9MuQtWbkepSL8L9KnnPWpMvwvUuZ61Jl+GSocz1qTL8MlQ6h61Nl2GSs3UPVpMuxSVi6h6tNlyKWsXU"
        ++ "PUpsuRS1zyqHqU2W45K5pTPTpstRyVyymepTZajkrjnM9OmyzHJXJKZ6dORYR65JSPSpyH5zWDkenTZHIuajmPQhIpTR5q1I74SM+eKuiMjuhIzp4evFdMZH"
        ++ "ZCRnzw9a6YyOuMihNDXRGR1xkUpoq6IyOmMilLFW8ZHTGRWMPPStuY35jhycV++ykf4hDCa5JSKGk1yykNIYTXFORQwmuOcihpNcM5FpCA5rilI1iiRBXLJn"
        ++ "VBFiIVg2d9NFuGsmz0qaLsJrNyPUpl2Fqzcj1aRehas3I9Wmy7C9ZuZ6tNl2J+lZOZ6lNl2J6ycz1KbLkUlYOZ6lNluKSsJTPUpstxP0rmlM9Omy3E9c0pHq"
        ++ "U2Wo3rmlI9Kmy1G9c0menTZajeuaTPSpsnU5rBs9KnICM1Nz0ISIZUyKtM74SKM8VbxZ2wkZ88XWumLOyEihNF1rpizrjIoTRda6Is64yKMsVdEZHTGRTmir"
        ++ "eMjpjIqmLmteY35jzmv32R/iaNPWuWQDD1rjkyxh61yTKQw9a4pspDW61xTNEKK5JGsSZOtc0jsgWI6xZ3wLUXasWejTLcVZM9OmXIqyZ6VMuxVkz06Rci6V"
        ++ "i2erTLkVZM9OmXIj0rGR6tMuRHpWDZ6dMuRViz06ZbiNc7PTplqI1zyPTpluM1zyPSplqM1hI9SmWoz0rBno0yynasWejTJKg9CAx+9UjvgVZRW0TtgUJwK6"
        ++ "InXEoTgYNdMTsiUJwK6InVEpTAc1vE6olKUCuiJ0RKxAzWpvc//Z"
