module Colors exposing
    ( allColors
    , darkBlue
    , darkGray
    , defaultGreen
    , gray
    , green_
    , lightBlue
    , offWhite
    , red_
    , toElmCssColor
    , toString
    )

import Css exposing (rgba)
import Element as El exposing (Color, toRgb)


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


defaultGreen =
    El.rgba255 0 180 110 0.7


allColors =
    [ gray
    , lightBlue
    , darkBlue
    , darkGray
    , offWhite
    , red_
    , green_

    -- Get more on https://coolors.co
    -- 1
    , El.rgba255 11 19 43 1
    , El.rgba255 28 37 65 1
    , El.rgba255 58 80 107 1
    , El.rgba255 91 192 190 1
    , El.rgba255 111 255 233 1

    -- 2
    , El.rgba255 244 247 190 1
    , El.rgba255 229 247 125 1
    , El.rgba255 222 186 111 1
    , El.rgba255 130 48 56 1
    , El.rgba255 30 0 14 1

    -- 3
    , El.rgba255 72 74 71 1
    , El.rgba255 92 109 112 1
    , El.rgba255 163 119 116 1
    , El.rgba255 232 136 115 1

    -- 4
    , El.rgba255 8 61 119 1
    , El.rgba255 235 235 211 1
    , El.rgba255 218 65 103 1
    , El.rgba255 244 211 94 1
    , El.rgba255 247 135 100 1
    , El.rgba255 224 172 157 1

    -- 5
    , El.rgba255 229 193 189 1
    , El.rgba255 210 208 186 1
    , El.rgba255 182 190 156 1
    , El.rgba255 123 158 135 1
    , El.rgba255 94 116 127 1
    ]


toElmCssColor color =
    let
        rgb =
            toRgb color

        fix value =
            if rgb.red < 1.0 || rgb.green < 1.0 || rgb.blue < 1.0 then
                round (value * 255)

            else
                round value
    in
    rgba (fix rgb.red) (fix rgb.green) (fix rgb.blue) rgb.alpha
