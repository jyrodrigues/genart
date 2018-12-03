module Colors exposing
    ( darkBlue
    , darkGray
    , gray
    , green_
    , lightBlue
    , offWhite
    , red_
    , toString
    )

import Element as El exposing (Color)


toString : Color -> String
toString color =
    let
        { red, green, blue, alpha } =
            El.toRgb color
    in
    "rgba("
        ++ String.fromInt (round (red * 255))
        ++ ("," ++ String.fromInt (round (green * 255)))
        ++ ("," ++ String.fromInt (round (blue * 255)))
        ++ ("," ++ String.fromFloat alpha ++ ")")


gray =
    El.rgb 170 170 170


lightBlue =
    El.rgba255 10 80 180 0.5


darkBlue =
    El.rgba255 15 15 60 0.95


darkGray =
    El.rgba 0 0 0 0.8


offWhite =
    El.rgb 200 200 200


red_ =
    El.rgb 240 0 0


green_ =
    El.rgb 0 240 0
