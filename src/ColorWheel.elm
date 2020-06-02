module ColorWheel exposing
    ( Model
    , Msg
    , MsgType(..)
    , getElementDimensions
    , initialModel
    , subscriptions
    , trackMouseOutsideWheel
    , update
    , view
    , withColor
    )

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
import LSystem.Image exposing (PathSegment(..), PathSegmentString)
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
import Svg.Styled.Events exposing (on, onMouseDown, onMouseOut, onMouseUp, stopPropagationOn)
import Svg.Styled.Lazy exposing (lazy2, lazy5, lazy6)
import Task


type alias Position =
    ( Float, Float )


type alias Model =
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


initialModel : String -> Model
initialModel id_ =
    { id = id_
    , elementDimensions = Nothing
    , mouseTracking = False
    , color = makeColor ( 0, 0 )

    -- For Development
    , mousePosition = ( 0, 0 )
    , dynamic = False

    -- Magic optimal value after profiling performance on FF75 and Chrome
    --, numberOfSlices = 97
    --, blur = 8
    , numberOfSlices = 100
    , blur = 2

    -- Config
    , trackMouseOutsideWheel = False
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


type MsgType
    = ColorChanged
    | SameColor


update : Msg -> Model -> ( Model, Cmd Msg, MsgType )
update msg model =
    case msg of
        GotMousePosition relativePosition ->
            ( { model
                | mousePosition = relativePosition
                , color = computeColor model relativePosition
              }
            , Cmd.none
            , ColorChanged
            )

        GotElementDimensions result ->
            ( { model | elementDimensions = Result.toMaybe result }, Cmd.none, SameColor )

        StartedMouseTracking ->
            ( { model | mouseTracking = True }, Cmd.none, SameColor )

        StoppedMouseTracking ->
            ( { model | mouseTracking = False }, Cmd.none, SameColor )

        SetOpacity opacity ->
            let
                { h, s } =
                    Colors.toHsva model.color

                color =
                    Colors.hsv h s opacity
            in
            ( { model | color = color }, Cmd.none, ColorChanged )

        --
        --
        --
        {--| For Development --}
        SetNumberOfSlices n ->
            ( { model | numberOfSlices = n }, Cmd.none, SameColor )

        SetBlur b ->
            ( { model | blur = b }, Cmd.none, SameColor )

        ToggledDynamic ->
            ( { model | dynamic = not model.dynamic }, Cmd.none, SameColor )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.mouseTracking then
        Sub.batch
            [ Browser.Events.onMouseMove (Decode.map GotMousePosition mouseInfoDecoder)
            , Browser.Events.onMouseUp (Decode.succeed StoppedMouseTracking)
            ]

    else
        Sub.none



-- CONFIG


trackMouseOutsideWheel : Bool -> Model -> Model
trackMouseOutsideWheel shouldTrack model =
    { model | trackMouseOutsideWheel = shouldTrack }


withColor : Color -> Model -> Model
withColor color model =
    { model | color = color }



-- COLOR


computeColor : Model -> Position -> Color
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


view : Model -> Svg Msg
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


viewForDevelopment : Model -> Svg Msg
viewForDevelopment model =
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
        , Html.Styled.text (String.fromFloat v)
        , gradientSliderInput SetOpacity v 0.0001 1.0001 0.01 (Colors.rangeValue model.color)
        , Html.Styled.text (String.fromFloat model.numberOfSlices)
        , sliderInput SetNumberOfSlices model.numberOfSlices 1 180 1
        , Html.Styled.text (String.fromFloat model.blur)
        , sliderInput SetBlur model.blur 0 40 1
        , Html.Styled.text (String.fromFloat v)
        , sliderInput SetOpacity v 0.0001 1.0001 0.01
        , Html.Styled.button [ Html.Styled.Events.onClick ToggledDynamic ]
            [ Html.Styled.text
                (if model.dynamic then
                    "Make static"

                 else
                    "Make dynamic"
                )
            ]
        ]


viewStatic : Model -> Svg Msg
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
                ]
            , id id_
            ]

        innerDivEventListeners =
            [ ( True, stopPropagationOn "click" (alwaysStopPropagation (Decode.map GotMousePosition mouseInfoDecoder)) )
            , ( True, onMouseDown StartedMouseTracking )
            , ( True, onMouseUp StoppedMouseTracking )
            , ( not mouseTrackingOutsideWheel, onMouseOut StoppedMouseTracking )
            , ( mouseTracking, on "mousemove" (Decode.map GotMousePosition mouseInfoDecoder) )
            ]
                |> List.filter Tuple.first
                |> List.map Tuple.second

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
                , backgroundImage (url "dist/colorwheel.jpeg")
                , Css.borderRadius (pct 50)
                , Css.opacity (Css.num value)
                ]
            , id (id_ ++ "static")
            ]

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


getElementDimensions : Model -> Cmd Msg
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


viewDynamic : Model -> Svg Msg
viewDynamic model =
    lazy6 viewDynamicEager
        model.id
        (floor model.numberOfSlices)
        model.mouseTracking
        model.blur
        model.color
        model.trackMouseOutsideWheel


alwaysStopPropagation : Decoder a -> Decoder ( a, Bool )
alwaysStopPropagation =
    Decode.map (\msg -> ( msg, True ))


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
            [ ( True, stopPropagationOn "click" (alwaysStopPropagation (Decode.map GotMousePosition mouseInfoDecoder)) )
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
