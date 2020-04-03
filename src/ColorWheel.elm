module ColorWheel exposing (Model, Msg, getElementDimensions, initialModel, update, view)

import Browser.Dom exposing (Element)
import Colors exposing (Color)
import Css exposing (backgroundColor, backgroundImage, block, borderRadius, display, hidden, overflow, pct, url)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Json.Decode as Decode exposing (Decoder)
import LSystem.Image exposing (PathSegment(..), PathSegmentString, segmentToString, toAbsoluteValue)
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
        , pointerEvents
        , r
        , scale
        , stopColor
        , stroke
        , strokeLinecap
        , strokeWidth
        , style
        , viewBox
        , width
        , x1
        , x2
        , y1
        , y2
        )
import Svg.Styled.Events exposing (on, onMouseDown, onMouseUp, stopPropagationOn)
import Svg.Styled.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5, lazy6)
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
    , precision : Float
    , blur : Float
    , dynamic : Bool
    }


type Msg
    = GotElementDimensions (Result Browser.Dom.Error Element)
    | GotMousePosition Position
    | StartedMouseTracking
    | StoppedMouseTracking
    | SetOpacity Float
      -- For Development
    | SetPrecision Float
    | SetBlur Float
    | ToggledDynamic


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
    , precision = 0.27
    , blur = 8
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMousePosition relativePosition ->
            ( { model | mousePosition = relativePosition, color = computeColor model relativePosition }, Cmd.none )

        GotElementDimensions result ->
            ( { model | elementDimensions = Result.toMaybe result }, Cmd.none )

        StartedMouseTracking ->
            ( { model | mouseTracking = True }, Cmd.none )

        StoppedMouseTracking ->
            ( { model | mouseTracking = False }, Cmd.none )

        SetOpacity opacity ->
            let
                { h, s } =
                    Colors.toHsva model.color

                color =
                    Colors.hsv h s opacity
            in
            ( { model | color = color }, Cmd.none )

        --
        --
        --
        {--| For Development --}
        SetPrecision p ->
            ( { model | precision = p }, Cmd.none )

        SetBlur b ->
            ( { model | blur = b }, Cmd.none )

        ToggledDynamic ->
            ( { model | dynamic = not model.dynamic }, Cmd.none )



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
        , Html.Styled.text (String.fromFloat model.precision)
        , sliderInput SetPrecision model.precision 0.01 1 0.01
        , Html.Styled.text (String.fromFloat model.blur)
        , sliderInput SetBlur model.blur 0 40 1
        , Html.Styled.text (String.fromFloat v)
        , sliderInput SetOpacity v 0 1 0.01
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
    lazy3 viewStaticEager model.id model.mouseTracking model.color


viewStaticEager : String -> Bool -> Color -> Svg Msg
viewStaticEager id_ mouseTracking color =
    let
        tracking =
            if mouseTracking then
                [ on "mousemove" (Decode.map GotMousePosition mouseInfoDecoder) ]

            else
                []

        opacity =
            .v (Colors.toHsva color)

        ( x, y ) =
            fromPolar (toPolarPosition color)

        scale =
            1.01
    in
    div
        [ css
            [ Css.height (pct 100)
            , Css.width (pct 100)
            , borderRadius (pct 50)
            , overflow hidden
            , backgroundColor (Colors.toCssColor Colors.black)
            ]
        ]
        [ div
            ([ css
                [ Css.height (pct 100)
                , Css.width (pct 100)
                , backgroundColor (Colors.toCssColor Colors.white)
                , backgroundImage (url "dist/colorwheel.svg")
                , Css.transform (Css.scale scale)
                , Css.borderRadius (pct 50)
                , Css.opacity (Css.num opacity)
                ]
             , id id_
             , stopPropagationOn "click"
                (Decode.map (\msg -> ( msg, True ))
                    (Decode.map GotMousePosition mouseInfoDecoder)
                )
             , onMouseDown StartedMouseTracking
             , onMouseUp StoppedMouseTracking
             ]
                ++ tracking
            )
            [ crosshair (scale * x) (scale * y) 0.05 ]
        ]


crosshair : Float -> Float -> Float -> Svg msg
crosshair x y radius =
    let
        fStr =
            String.fromFloat

        strokeWidth_ =
            radius / 10
    in
    svg [ viewBox "-1 -1 2 2", height "100%", width "100%", pointerEvents "none" ]
        [ circle
            [ cx (fStr x)
            , cy (fStr y)
            , r (fStr radius)
            , fill "none"
            , stroke (Colors.toString <| Colors.updateAlpha 0.9 Colors.black)
            , strokeWidth (fStr strokeWidth_)
            ]
            []
        , line
            [ x1 (fStr (x - 2 * radius))
            , x2 (fStr (x + 2 * radius))
            , y1 (fStr y)
            , y2 (fStr y)
            , stroke (Colors.toString <| Colors.updateAlpha 0.9 Colors.black)
            , strokeWidth (fStr strokeWidth_)
            ]
            []
        , line
            [ x1 (fStr x)
            , x2 (fStr x)
            , y1 (fStr (y - 2 * radius))
            , y2 (fStr (y + 2 * radius))
            , stroke (Colors.toString <| Colors.updateAlpha 0.9 Colors.black)
            , strokeWidth (fStr strokeWidth_)
            ]
            []
        ]


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
    Html.Styled.input
        [ Html.Styled.Attributes.type_ "range"
        , Html.Styled.Attributes.min (String.fromFloat min_)
        , Html.Styled.Attributes.max (String.fromFloat max_)
        , Html.Styled.Attributes.step (String.fromFloat step_)
        , Html.Styled.Attributes.value <| String.fromFloat oldValue
        , Html.Styled.Events.onInput onInputCallback
        , css [ display block, Css.width (pct 100) ]
        ]
        []



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
        floatString =
            String.fromFloat float

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
    lazy5 viewDynamicEager model.id model.precision model.mouseTracking model.blur model.color


viewDynamicEager : String -> Float -> Bool -> Float -> Color -> Svg Msg
viewDynamicEager id_ precision mouseTracking blur color =
    let
        tracking =
            if mouseTracking then
                [ on "mousemove" (Decode.map GotMousePosition mouseInfoDecoder) ]

            else
                []

        opacity =
            .v (Colors.toHsva color)

        ( x, y ) =
            fromPolar (toPolarPosition color)

        scale =
            1.01
    in
    div
        [ css
            [ borderRadius (pct 50)
            , overflow hidden
            , backgroundColor (Colors.toCssColor Colors.black)
            , Css.position Css.relative
            ]
        ]
        [ svg
            -- TODO add xmlns="http://www.w3.org/2000/svg"
            ([ viewBox "-1 -1 2 2"
             , height "100%"
             , width "100%"
             , style <|
                "display: block;"
                    ++ "background-color: white;"
                    ++ "transform: scale(1.01);"
                    ++ "boder-radius: 50%;"
                    -- TODO change opacity to simulate Value change (the V in HSV)
                    ++ ("opacity: " ++ String.fromFloat opacity ++ ";")
             , filter <| "blur(" ++ String.fromFloat blur ++ "px)"
             , id id_
             , stopPropagationOn "click"
                (Decode.map (\msg -> ( msg, True ))
                    (Decode.map GotMousePosition mouseInfoDecoder)
                )
             , onMouseDown StartedMouseTracking
             , onMouseUp StoppedMouseTracking
             ]
                ++ tracking
            )
            [ lazy pizza precision ]
        , div
            [ css
                [ Css.position Css.absolute
                , Css.top Css.zero
                , Css.left Css.zero
                , Css.height (pct 100)
                , Css.width (pct 100)
                , Css.pointerEvents Css.none
                ]
            ]
            [ crosshair (scale * x) (scale * y) 0.05 ]
        ]



-- TODO precision dictate number of slices and precisely size them


pizza : Float -> Svg msg
pizza precision =
    Svg.Styled.g []
        (List.range 1 (ceiling <| 360 * precision)
            |> List.map (toFloat >> (\theta -> theta / precision) >> degrees)
            |> List.concatMap (pizzaSlice precision)
        )


pizzaSlice : Float -> Float -> List (Svg msg)
pizzaSlice precision radians_ =
    let
        endColor =
            Colors.toString <| makeColor ( 1, radians_ )

        startColor =
            Colors.toString <| makeColor ( 0, radians_ )

        id_ =
            "colorId_" ++ strTruncateFromFloat 2 radians_
    in
    [ toppings startColor endColor id_
    , path
        [ d (betterDough precision radians_)
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
betterDough precision radians_ =
    let
        radius =
            -- Greater than the pizza radius of 1: it'll be trimmed via border-radius 50%.
            3

        sliceSize =
            -- More than 1 degree because otherwise we would see white segments dividing the slices.
            -- My guess is it's a floating point precision thing.
            degrees (1.1 * (1 / precision))

        ( x1, y1 ) =
            fromPolar ( radius, radians_ )

        ( x2, y2 ) =
            fromPolar ( radius, radians_ + sliceSize )

        moveTo =
            "M 0 0"

        lineTo1 =
            "L" ++ strTruncateFromFloat 4 x1 ++ " " ++ strTruncateFromFloat 4 y1

        lineTo2 =
            "L" ++ strTruncateFromFloat 4 x2 ++ " " ++ strTruncateFromFloat 4 y2
    in
    moveTo ++ lineTo1 ++ lineTo2 ++ "Z"


toppings : String -> String -> String -> Svg msg
toppings startColor endColor id_ =
    defs []
        [ radialGradient [ id id_, cx "0", cy "0", r "1", gradientUnits "userSpaceOnUse" ]
            [ stop [ offset "0%", stopColor startColor ] []
            , stop [ offset "100%", stopColor endColor ] []
            ]
        ]
