module Colors exposing
    ( Color
    , allColors
    , black
    , darkBlue
    , darkGray
    , decoder
    , defaultGreen
    , encode
    , fromHexString
    , gray
    , green_
    , lightBlue
    , lightGray
    , offWhite
    , red_
    , toCssColor
    , toHexString
    , toString
    , updateAlpha
    , updateBlue
    , updateGreen
    , updateRed
    )

import Color exposing (Color, fromHsla, fromRgba, hsl, hsla, rgb255, rgba, toCssString, toHsla, toRgba)
import Css exposing (hex)
import Hex
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- TYPE


{-|

    This type only exists because Elm-css doesn't expose something like
    colorToString and svg.stroke only takes a string as argument. Therefore
    we keep this type to allow for `toString` methods.

-}
type alias Color =
    Color.Color



-- TODO remove the Hex variant use rtfeldman/hex to convert from or to hex notation
-- CONVERTERS


to255 : Float -> Int
to255 f =
    round (clamp 0 1 f * 255)


toCssColor : Color -> Css.Color
toCssColor color =
    let
        { red, green, blue, alpha } =
            toRgba color
    in
    Css.rgba (to255 red) (to255 green) (to255 blue) alpha


toString : Color -> String
toString =
    toCssString


toHexString : Color -> String
toHexString color =
    let
        { red, green, blue, alpha } =
            toRgba color

        pad hexString =
            if String.length hexString < 2 then
                "0" ++ hexString

            else
                hexString
    in
    "#"
        ++ (pad <| Hex.toString <| to255 red)
        ++ (pad <| Hex.toString <| to255 green)
        ++ (pad <| Hex.toString <| to255 blue)



-- START OF COPY-PASTE FROM RTFELDMAN/ELM-CSS IMPLEMENTATION OF `HEX`


fromHexString : String -> Color
fromHexString str =
    let
        withoutHash =
            if String.startsWith "#" str then
                String.dropLeft 1 str

            else
                str
    in
    case String.toList withoutHash of
        [ r, g, b ] ->
            validHex str ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

        [ r, g, b, a ] ->
            validHex str ( r, r ) ( g, g ) ( b, b ) ( a, a )

        [ r1, r2, g1, g2, b1, b2 ] ->
            validHex str ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

        [ r1, r2, g1, g2, b1, b2, a1, a2 ] ->
            validHex str ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

        _ ->
            erroneousHex str


validHex : String -> ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> Color
validHex str ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 ) =
    let
        toResult =
            String.fromList >> String.toLower >> Hex.fromString

        results =
            ( ( toResult [ r1, r2 ]
              , toResult [ g1, g2 ]
              )
            , ( toResult [ b1, b2 ]
              , toResult [ a1, a2 ]
              )
            )
    in
    case results of
        ( ( Ok red, Ok green ), ( Ok blue, Ok alpha ) ) ->
            rgba255 red green blue (toFloat alpha / 255.0)

        _ ->
            erroneousHex str


{-| Not to be confused with Thelonious Monk or Hieronymus Bosch.
-}
erroneousHex : String -> Color
erroneousHex str =
    let
        _ =
            Debug.log "erroneousHex string" str
    in
    rgba 0 0 0 1



-- END OF COPY-PASTE
-- TODO check float values are aligned with what is expected from Color library


updateRed : Float -> Color -> Color
updateRed amount color =
    let
        { green, blue, alpha } =
            toRgba color
    in
    rgba amount green blue alpha


updateGreen : Float -> Color -> Color
updateGreen amount color =
    let
        { red, blue, alpha } =
            toRgba color
    in
    rgba red amount blue alpha


updateBlue : Float -> Color -> Color
updateBlue amount color =
    let
        { red, green, alpha } =
            toRgba color
    in
    rgba red green amount alpha


updateAlpha : Float -> Color -> Color
updateAlpha amount color =
    let
        { red, green, blue } =
            toRgba color
    in
    rgba red green blue amount



-- JSON


encode : Color -> Encode.Value
encode =
    Encode.string << toHexString


decoder : Decoder Color
decoder =
    Decode.map fromHexString Decode.string



-- Aux


rgba255 : Int -> Int -> Int -> Float -> Color
rgba255 r g b a =
    let
        { red, green, blue } =
            toRgba (rgb255 r g b)
    in
    rgba red green blue a



-- CONSTANTS


gray : Color
gray =
    rgb255 170 170 170


lightBlue : Color
lightBlue =
    rgba255 10 80 180 0.5


darkBlue : Color
darkBlue =
    rgba255 15 15 60 0.95


lightGray : Color
lightGray =
    fromHexString "#ABAAAA"


darkGray : Color
darkGray =
    -- Color 0 0 0 0.8
    fromHexString "#333333"


black : Color
black =
    rgba 0 0 0 1


offWhite : Color
offWhite =
    rgb255 200 200 200


red_ : Color
red_ =
    rgb255 240 0 0


green_ : Color
green_ =
    rgb255 0 240 0


defaultGreen : Color
defaultGreen =
    --Color 0 180 110 0.7
    fromHexString "#00b46e"


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
    , rgb255 11 19 43
    , rgb255 28 37 65
    , rgb255 58 80 107
    , rgb255 91 192 190
    , rgb255 111 255 233

    -- 2
    , rgb255 244 247 190
    , rgb255 229 247 125
    , rgb255 222 186 111
    , rgb255 130 48 56
    , rgb255 30 0 14

    -- 3
    , rgb255 72 74 71
    , rgb255 92 109 112
    , rgb255 163 119 116
    , rgb255 232 136 115

    -- 4
    , rgb255 8 61 119
    , rgb255 235 235 211
    , rgb255 218 65 103
    , rgb255 244 211 94
    , rgb255 247 135 100
    , rgb255 224 172 157

    -- 5
    , rgb255 229 193 189
    , rgb255 210 208 186
    , rgb255 182 190 156
    , rgb255 123 158 135
    , rgb255 94 116 127
    ]
