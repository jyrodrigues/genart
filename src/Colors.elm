module Colors exposing
    ( Color(..)
    , allColors
    , darkBlue
    , darkGray
    , defaultGreen
    , fromHexString
    , gray
    , green_
    , lightBlue
    , offWhite
    , red_
    , toCssColor
    , toHexString
    , toString
    )

import Css exposing (Color, hex, rgba)


{-|

    This type only exists because Elm-css doesn't expose something like
    colorToString and svg.stroke only takes a string as argument. Therefore
    we keep this type to allow for `toString` methods.

-}
type Color
    = Color Int Int Int Float
    | Hex String


toCssColor : Color -> Css.Color
toCssColor color =
    case color of
        Color r g b a ->
            rgba r g b a

        Hex str ->
            hex str


toString : Color -> String
toString color =
    case color of
        Color r g b a ->
            let
                rgb =
                    [ r, g, b ]
                        |> List.map String.fromInt
                        |> String.join ","

                innerString =
                    rgb ++ "," ++ String.fromFloat a
            in
            "rgba(" ++ innerString ++ ")"

        Hex str ->
            str


toHexString : Color -> String
toHexString color =
    case color of
        Color r g b a ->
            "#aabbcc"

        Hex str ->
            str


fromHexString : String -> Color
fromHexString color =
    Hex color


gray : Color
gray =
    Color 170 170 170 1


lightBlue : Color
lightBlue =
    Color 10 80 180 0.5


darkBlue : Color
darkBlue =
    Color 15 15 60 0.95


darkGray : Color
darkGray =
    -- Color 0 0 0 0.8
    Hex "#333333"


offWhite : Color
offWhite =
    Color 200 200 200 1


red_ : Color
red_ =
    Color 240 0 0 1


green_ : Color
green_ =
    Color 0 240 0 1


defaultGreen : Color
defaultGreen =
    --Color 0 180 110 0.7
    Hex "#00b46e"


allColors : List Color
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
    , Color 11 19 43 1
    , Color 28 37 65 1
    , Color 58 80 107 1
    , Color 91 192 190 1
    , Color 111 255 233 1

    -- 2
    , Color 244 247 190 1
    , Color 229 247 125 1
    , Color 222 186 111 1
    , Color 130 48 56 1
    , Color 30 0 14 1

    -- 3
    , Color 72 74 71 1
    , Color 92 109 112 1
    , Color 163 119 116 1
    , Color 232 136 115 1

    -- 4
    , Color 8 61 119 1
    , Color 235 235 211 1
    , Color 218 65 103 1
    , Color 244 211 94 1
    , Color 247 135 100 1
    , Color 224 172 157 1

    -- 5
    , Color 229 193 189 1
    , Color 210 208 186 1
    , Color 182 190 156 1
    , Color 123 158 135 1
    , Color 94 116 127 1
    ]
