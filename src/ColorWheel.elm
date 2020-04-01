module ColorWheel exposing (Model, Msg, getElementDimensions, initialModel, update, view, withPrecision)

import Browser.Dom exposing (Element)
import Colors exposing (Color)
import Json.Decode as Decode exposing (Decoder)
import LSystem.Image exposing (PathSegment(..), PathSegmentString, segmentToString, toAbsoluteValue)
import Svg.Styled exposing (Svg, defs, path, radialGradient, stop, svg)
import Svg.Styled.Attributes
    exposing
        ( cx
        , cy
        , d
        , fill
        , gradientUnits
        , height
        , id
        , offset
        , pointerEvents
        , r
        , stopColor
        , style
        , viewBox
        , width
        )
import Svg.Styled.Events exposing (on)
import Svg.Styled.Lazy exposing (lazy)
import Task


type alias Position =
    ( Float, Float )


type alias Model =
    -- TODO make it opaque
    { id : String
    , mousePosition : Position
    , color : Color
    , elementDimensions : Maybe Element
    , precision : Float
    }


initialModel : String -> Model
initialModel id_ =
    Model id_ ( 0, 0 ) (makeColor ( 0, 0 )) Nothing 2


withPrecision : Float -> Model -> Model
withPrecision precision model =
    { model | precision = precision }


type Msg
    = MouseClicked Position
    | GotElementDimensions (Result Browser.Dom.Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseClicked relativePosition ->
            ( { model | mousePosition = relativePosition, color = computeColor model relativePosition }, Cmd.none )

        GotElementDimensions result ->
            ( { model | elementDimensions = Result.toMaybe result }, Cmd.none )


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
            makeColor clickedVec

        Nothing ->
            makeColor ( 0, 0 )


mouseInfoDecoder : Decoder Position
mouseInfoDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


getElementDimensions : Model -> Cmd Msg
getElementDimensions model =
    Task.attempt GotElementDimensions (Browser.Dom.getElement model.id)



-- COLOR


makeColor : Position -> Color
makeColor ( r, theta ) =
    Colors.hsl (theta / (pi * 2)) 1 ((1 - r) * 0.5 + 0.5)



-- VIEW


view : Model -> Svg Msg
view model =
    lazy viewEager model.id


viewEager : String -> Svg Msg
viewEager id_ =
    svg
        -- TODO add xmlns="http://www.w3.org/2000/svg"
        [ viewBox "-1 -1 2 2"
        , height "100%"
        , width "100%"
        , style <|
            "display: block;"
                ++ "border-radius: 50%;"
                ++ "background-color: white;"
        , id id_

        -- TODO add stopPropagation
        , on "mousedown" (Decode.map MouseClicked mouseInfoDecoder)
        ]
        pizza


pizza : List (Svg msg)
pizza =
    let
        precision =
            1 / 4
    in
    List.range 1 (round <| 360 * precision)
        |> List.map (toFloat >> (\theta -> theta / precision) >> degrees)
        |> List.concatMap (pizzaSlice precision)


pizzaSlice : Float -> Float -> List (Svg msg)
pizzaSlice precision radians_ =
    let
        endColor =
            Colors.toString <| makeColor ( 1, radians_ )

        startColor =
            Colors.toString <| makeColor ( 0, radians_ )

        id_ =
            "colorId_" ++ String.fromFloat radians_
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
            1.9

        sliceSize =
            -- More than 1 degree because otherwise we would see white segments dividing the slices.
            -- My guess is it's a floating point precision thing.
            degrees (2 / precision)

        ( x1, y1 ) =
            fromPolar ( radius, radians_ )

        ( x2, y2 ) =
            fromPolar ( radius, radians_ + sliceSize )

        moveTo =
            "M 0 0"

        lineTo1 =
            "L" ++ String.fromFloat x1 ++ " " ++ String.fromFloat y1

        lineTo2 =
            "L" ++ String.fromFloat x2 ++ " " ++ String.fromFloat y2
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
