module AppInit exposing (suite)

{--
    Note that Main.init cannot be tested directly because Navigation.Key can't be mocked.

    So we import everything from Main because anything other than `main` is exposed for testing.

    It seems that Elmer can circumvent this (https://elmer-test.cfapps.io/elmer/)

    Check for more info:
    - https://discourse.elm-lang.org/t/testing-update-for-browser-application-nav-key-gets-in-the-way/2101
    - https://github.com/elm-explorations/test/issues/24
    - https://elmer-test.cfapps.io/elmer/versions/6.0.0/module/Elmer.Navigation
--}

import Color exposing (rgba)
import Colors exposing (Color)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import LSystem.Core as LCore exposing (Composition, Step(..))
import LSystem.Image as Image
    exposing
        ( Image
        , PathCurve(..)
        , defaultImage
        )
import Main exposing (Route(..), encodeModel, modelDecoder, parseUrl)
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Url exposing (Protocol(..), Url)



-- BASE HELPERS


emptyUrl : Url
emptyUrl =
    { protocol = Https
    , host = "test.art"
    , port_ = Nothing
    , path = "/"
    , query = Nothing
    , fragment = Nothing
    }



-- FUZZERS


compositionFuzzer : Fuzzer Composition
compositionFuzzer =
    [ D, L, R, S ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
        -- To list of Steps, i.e. Block
        |> Fuzz.list
        -- To list of Blocks, i.e. almost a Composition
        |> Fuzz.list
        |> Fuzz.map LCore.fromList
        |> Fuzz.map nonEmptyComposition


{-| This helps preventing a bug: `Expect.equal` an empty composition
-}
nonEmptyComposition : Composition -> Composition
nonEmptyComposition composition =
    if LCore.length composition == 0 then
        LCore.fromList [ [ D ] ]

    else
        composition


colorFuzzer : Fuzzer Color
colorFuzzer =
    -- Forcing Color internal representation as hex since our URL scheme uses this representation.
    -- TODO change this when implementing Colors.toString always as hex (essentially it would be the same as encode?)
    Fuzz.map (Colors.toHexString >> Colors.fromHexString) <|
        Fuzz.map4
            rgba
            Fuzz.float
            Fuzz.float
            Fuzz.float
            Fuzz.float


pathCurveFuzzer : Fuzzer PathCurve
pathCurveFuzzer =
    [ Line, Curve ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


imageFuzzer : Fuzzer Image
imageFuzzer =
    Fuzz.map5 Image
        -- Composition
        compositionFuzzer
        -- Turn angle
        Fuzz.float
        -- SvgPathAndBoundaries
        (Fuzz.constant Nothing)
        -- Background color
        colorFuzzer
        -- Stroke Color
        colorFuzzer
        -- Stroke width
        |> Fuzz.andMap Fuzz.float
        -- Translate
        |> Fuzz.andMap (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
        -- Scale
        |> Fuzz.andMap Fuzz.float
        -- Path Curve
        |> Fuzz.andMap pathCurveFuzzer



-- TESTS


suite : Test
suite =
    describe "Main"
        [ describe "Core functions inside `init`"
            [ describe "Encode/Decode image and gallery (localStorage)"
                [ fuzz imageFuzzer "Image essentials" <|
                    \fuzzyImage ->
                        fuzzyImage
                            |> Image.encode
                            |> Decode.decodeValue Image.decoder
                            |> Expect.equal (Ok fuzzyImage)
                , fuzz2 imageFuzzer (Fuzz.list imageFuzzer) "Image and Gallery" <|
                    \fuzzyImage fuzzyGallery ->
                        { image = fuzzyImage, gallery = fuzzyGallery }
                            |> encodeModel
                            |> Decode.decodeValue modelDecoder
                            |> Expect.equal (Ok ( fuzzyImage, fuzzyGallery ))
                , test "Decode Image without strokeWidth" <|
                    \_ ->
                        imageV3
                            |> Decode.decodeString Image.decoder
                            |> Expect.equal (Ok defaultImage)
                ]
            , describe "parseUrl"
                -- HAPPY PATH
                -- https://hybridcode.art/
                --                      ?composition=%5B%22DLDDD%22%5D
                --                      &turnAngle=60
                --                      &backgroundColor=%22%23333333%22
                --                      &strokeColor=%22%2300b46e%22
                --                      &strokeWidth=1.00000000000009
                --                      &translateX=-6.72000000000002
                --                      &translateY=-0.24000000000000055
                --                      &scale=1.480000000000001
                [ fuzz imageFuzzer "Happy path: Image on query" <|
                    \fuzzyImage ->
                        Maybe.withDefault emptyUrl (Url.fromString ("https://test.art" ++ Image.toQuery fuzzyImage))
                            |> parseUrl
                            |> Expect.equal
                                (Editor
                                    { composition = Just fuzzyImage.composition
                                    , turnAngle = Just fuzzyImage.turnAngle
                                    , backgroundColor = Just fuzzyImage.backgroundColor
                                    , strokeColor = Just fuzzyImage.strokeColor
                                    , strokeWidth = Just fuzzyImage.strokeWidth
                                    , translate = Just fuzzyImage.translate
                                    , scale = Just fuzzyImage.scale
                                    , curve = Just fuzzyImage.curve
                                    }
                                )

                {--
                , todo "Missing composition"
                , todo "Missing turnAngle"
                , todo "Missing backgroundColor"
                , todo "Missing strokeColor"
                , todo "Missing translateX"
                , todo "Missing translateY"
                , todo "Missing scale"
                --}
                ]
            ]
        ]



-- BACKWARD COMPATIBILITY VALUES


imageV3 : String
imageV3 =
    "{\"composition\":[\"DLDLDLD\",\"D\"],\"turnAngle\":90,\"backgroundColor\":\"#333333\",\"strokeColor\":\"#00b46e\",\"translateX\":0,\"translateY\":0,\"scale\":1}"
