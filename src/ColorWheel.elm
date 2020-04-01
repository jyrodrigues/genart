module ColorWheel exposing (colorWheel)

import Colors exposing (Color)
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



-- TODO add xmlns="http://www.w3.org/2000/svg"


colorWheel : Svg msg
colorWheel =
    svg
        [ viewBox "-1 -1 2 2"
        , height "100%"
        , width "100%"
        , style <|
            "display: block; "
                ++ "height: 100%; "
                ++ "width: 100%; "
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
    [ customGradient startColor endColor id_
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


customGradient : String -> String -> String -> Svg msg
customGradient startColor endColor id_ =
    defs []
        [ radialGradient [ id id_, cx "0", cy "0", r "1", gradientUnits "userSpaceOnUse" ]
            [ stop [ offset "0%", stopColor startColor ] []
            , stop [ offset "100%", stopColor endColor ] []
            ]
        ]


gradients : Color -> Color -> Svg msg
gradients strokeColor backgroundColor =
    defs []
        [ radialGradient [ id "RadialGradient1" ]
            [ stop [ offset "30%", stopColor (Colors.toString backgroundColor) ] []
            , stop [ offset "100%", stopColor (Colors.toString strokeColor) ] []
            ]
        ]
