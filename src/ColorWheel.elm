module ColorWheel exposing (Model, Msg, update, view)

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
        , r
        , stopColor
        , style
        , viewBox
        , width
        )
import Svg.Styled.Events exposing (on)


type alias Model =
    ( Float, Float )


type Msg
    = MouseClicked ( Float, Float )


update : Msg -> ( Float, Float )
update msg =
    case msg of
        MouseClicked position ->
            position



-- VIEW


view : Svg Msg
view =
    svg
        -- TODO add xmlns="http://www.w3.org/2000/svg"
        [ viewBox "-1 -1 2 2"
        , height "100%"
        , width "100%"
        , style <|
            "display: block; "
                -- TODO remove those?
                ++ "height: 100%; "
                ++ "width: 100%; "
        , on "click" (Decode.map MouseClicked clickPositionDecoder)
        ]
        pizza


pizza : List (Svg msg)
pizza =
    List.range 0 359
        |> List.concatMap (toFloat >> degrees >> pizzaSlice)


pizzaSlice : Float -> List (Svg msg)
pizzaSlice radians_ =
    let
        ( x0, y0 ) =
            fromPolar ( 1, radians_ )

        line =
            -- Reverse y-coordinate to compensate SVG top-to-bottom axis direction
            "l" ++ String.fromFloat x0 ++ " " ++ String.fromFloat y0

        endColor =
            Colors.toString <| Colors.hsl (radians_ / (pi * 2)) 1 0.5

        startColor =
            Colors.toString <| Colors.hsl (radians_ / (pi * 2)) 1 1

        id_ =
            "colorId_" ++ String.fromFloat radians_
    in
    [ toppings startColor endColor id_
    , path
        [ d ("M 0 0" ++ line ++ dough radians_ ++ "z")
        , fill <| "url(#" ++ id_ ++ ")"
        ]
        []
    ]


dough : Float -> PathSegmentString
dough radians_ =
    let
        sliceSize =
            1.5

        controlPoint =
            toPolar ( 1, tan (degrees (sliceSize / 2)) )

        destination =
            ( 1, degrees sliceSize )

        position ( r, theta ) =
            fromPolar ( r, theta + radians_ )
    in
    toAbsoluteValue <| segmentToString <| Q (position controlPoint) (position destination)


toppings : String -> String -> String -> Svg msg
toppings startColor endColor id_ =
    defs []
        [ radialGradient [ id id_, cx "0", cy "0", r "1", gradientUnits "userSpaceOnUse" ]
            [ stop [ offset "0%", stopColor startColor ] []
            , stop [ offset "100%", stopColor endColor ] []
            ]
        ]



-- EVENTS
-- DECODER


clickPositionDecoder : Decoder ( Float, Float )
clickPositionDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
