module Colors exposing
    ( Color
    , Range
    , activeElementGray
    , allColors
    , black
    , blackShadow
    , darkBlue
    , darkGray
    , decoder
    , defaultGreen
    , encode
    , fromHexString
    , gray
    , green_
    , hsl
    , hsv
    , hsva
    , lightBlue
    , lightGray
    , offWhite
    , pureBlue
    , pureGreen
    , pureRed
    , random
    , rangeBlue
    , rangeGreen
    , rangeHue
    , rangeLightness
    , rangeRed
    , rangeSaturation
    , rangeValue
    , red_
    , toCssColor
    , toHexString
    , toHsla
    , toHsva
    , toRgba
    , toString
    , updateAlpha
    , updateBlue
    , updateGreen
    , updateHue
    , updateLightness
    , updateRed
    , updateSaturation
    , updateValue
    , white
    )

import Color exposing (Color, hsla, rgb255, rgba, toCssString, toHsla, toRgba)
import Css
import Hex
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random
import Utils exposing (floatModBy)



-- TYPE


{-|

    This type only exists because Elm-css doesn't expose something like
    colorToString and svg.stroke only takes a string as argument. Therefore
    we keep this type to allow for `toString` methods.

-}
type alias Color =
    Color.Color



-- RE-EXPORTS


hsl : Float -> Float -> Float -> Color
hsl =
    Color.hsl


toRgba : Color -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgba =
    Color.toRgba


toHsla : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsla =
    Color.toHsla


toString : Color -> String
toString =
    toCssString



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


toHexString : Color -> String
toHexString color =
    let
        { red, green, blue } =
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



-- HSV


hsv : Float -> Float -> Float -> Color
hsv h s v =
    hsva h s v 1


{-| hue radians, saturation [0~1], value [0~1], alpha [0~1]

-- From <https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB>

-}
hsva : Float -> Float -> Float -> Float -> Color
hsva hue_ s v a =
    let
        hue =
            inDegrees (floatModBy (2 * pi) hue_)

        c =
            v * s

        h_ =
            hue / 60

        x =
            c * (1 - abs (floatModBy 2 h_ - 1))

        ( r1, g1, b1 ) =
            if 0 <= h_ && h_ <= 1 then
                ( c, x, 0 )

            else if 1 < h_ && h_ <= 2 then
                ( x, c, 0 )

            else if 2 < h_ && h_ <= 3 then
                ( 0, c, x )

            else if 3 < h_ && h_ <= 4 then
                ( 0, x, c )

            else if 4 < h_ && h_ <= 5 then
                ( x, 0, c )

            else if 5 < h_ && h_ <= 6 then
                ( c, 0, x )

            else
                ( 0, 0, 0 )

        m =
            v - c

        ( r, g, b ) =
            ( r1 + m, g1 + m, b1 + m )
    in
    Color.rgba r g b a


inDegrees : Float -> Float
inDegrees radians =
    radians / degrees 1


{-| -- from <https://en.wikipedia.org/wiki/HSL_and_HSV#General_approach>
-}
toHsva : Color -> { h : Float, s : Float, v : Float, a : Float }
toHsva color =
    let
        { red, green, blue, alpha } =
            toRgba color

        max_ =
            max red (max green blue)

        min_ =
            min red (min green blue)

        c =
            max_ - min_

        h_ =
            if c == 0 then
                0

            else if max_ == red then
                let
                    almost =
                        (green - blue) / c
                in
                if almost < 0 then
                    almost + 6

                else
                    almost

            else if max_ == green then
                (blue - red) / c + 2

            else if max_ == blue then
                (red - green) / c + 4

            else
                -- Will never happen.
                0

        h =
            degrees (h_ * 60)

        v =
            max_

        s =
            if v == 0 then
                0

            else
                c / v
    in
    { h = h, s = s, v = v, a = alpha }



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
    rgba 0 0 0 1



-- END OF COPY-PASTE
-- TODO check float values are aligned with what is expected from Color library


updateRed : Float -> Color -> Color
updateRed amount color =
    let
        { green, blue, alpha } =
            toRgba color
    in
    rgba (clamp 0 1 amount) green blue alpha


updateGreen : Float -> Color -> Color
updateGreen amount color =
    let
        { red, blue, alpha } =
            toRgba color
    in
    rgba red (clamp 0 1 amount) blue alpha


updateBlue : Float -> Color -> Color
updateBlue amount color =
    let
        { red, green, alpha } =
            toRgba color
    in
    rgba red green (clamp 0 1 amount) alpha


type alias Range =
    { start : Color
    , end : Color
    }


rangeRed : Color -> Range
rangeRed color =
    { start = updateRed 0 color
    , end = updateRed 1 color
    }


rangeGreen : Color -> Range
rangeGreen color =
    { start = updateGreen 0 color
    , end = updateGreen 1 color
    }


rangeBlue : Color -> Range
rangeBlue color =
    { start = updateBlue 0 color
    , end = updateBlue 1 color
    }


pureRed : Float -> Color
pureRed amount =
    rgba (clamp 0 1 amount) 0 0 1


pureGreen : Float -> Color
pureGreen amount =
    rgba 0 (clamp 0 1 amount) 0 1


pureBlue : Float -> Color
pureBlue amount =
    rgba 0 0 (clamp 0 1 amount) 1


updateAlpha : Float -> Color -> Color
updateAlpha amount color =
    let
        { red, green, blue } =
            toRgba color
    in
    rgba red green blue (clamp 0 1 amount)


rangeHue : Color -> Range
rangeHue color =
    { start = updateHue 0 color
    , end = updateHue 1 color
    }


rangeSaturation : Color -> Range
rangeSaturation color =
    { start = updateSaturation 0 color
    , end = updateSaturation 1 color
    }


rangeLightness : Color -> Range
rangeLightness color =
    { start = updateLightness 0 color
    , end = updateLightness 1 color
    }


rangeValue : Color -> Range
rangeValue color =
    { start = updateValue 0 color
    , end = updateValue 1 color
    }


updateHue : Float -> Color -> Color
updateHue amount color =
    let
        { saturation, lightness, alpha } =
            toHsla color
    in
    hsla (clamp 0 1 amount) saturation lightness alpha


updateSaturation : Float -> Color -> Color
updateSaturation amount color =
    let
        { hue, lightness, alpha } =
            toHsla color
    in
    hsla hue (clamp 0 1 amount) lightness alpha


updateLightness : Float -> Color -> Color
updateLightness amount color =
    let
        { hue, saturation, alpha } =
            toHsla color
    in
    hsla hue saturation (clamp 0 1 amount) alpha


updateValue : Float -> Color -> Color
updateValue v color =
    let
        { h, s, a } =
            toHsva color
    in
    hsva h s v a



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


white : Color
white =
    rgba 1 1 1 1


offWhite : Color
offWhite =
    rgb255 200 200 200


red_ : Color
red_ =
    rgb255 240 0 0


green_ : Color
green_ =
    rgb255 0 240 0


blackShadow : Color
blackShadow =
    rgba 0 0 0 0.3


activeElementGray : Color
activeElementGray =
    rgb255 130 130 130


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



-- RANDOM


random : Random.Generator Color
random =
    Random.float 0 (2 * pi)
        |> Random.map (\hue -> hsv hue 1 1)
