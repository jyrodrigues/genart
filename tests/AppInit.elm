module AppInit exposing (backwardCompatibilitySuite, suite)

{--
    Note that Main.init cannot be tested directly because Navigation.Key can't be mocked.

    So we import everything from Main because anything other than `main` is exposed for testing.

    It seems that Elmer can circumvent this (https://elmer-test.cfapps.io/elmer/)

    Check for more info:
    - https://discourse.elm-lang.org/t/testing-update-for-browser-application-nav-key-gets-in-the-way/2101
    - https://github.com/elm-explorations/test/issues/24
    - https://elmer-test.cfapps.io/elmer/versions/6.0.0/module/Elmer.Navigation
--}

import Colors exposing (Color(..))
import Expect
import Fuzz exposing (Fuzzer)
import ImageEssentials
    exposing
        ( Gallery
        , ImageAndGallery
        , ImageEssentials
        , V2_Image
        , V2_ImageAndGallery
        , encodeImage
        , encodeImageAndGallery
        , imageAndGalleryDecoder
        , imageDecoder
        , mergeV2toV3
        , replaceComposition
        , v2_encodeImage
        , v2_encodeImageAndGallery
        , v2_imageAndGalleryDecoder
        , v2_imageDecoder
        , v2_imageToImageEssentials
        )
import Json.Decode as Decode
import Json.Encode as Encode
import LSystem.Core as LCore exposing (Composition, Step(..), encodeComposition)
import Main exposing (..)
import Test exposing (Test, describe, fuzz, fuzz2, test, todo)
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
            (\r g b a -> Color r g b a)
            (Fuzz.intRange 0 255)
            (Fuzz.intRange 0 255)
            (Fuzz.intRange 0 255)
            Fuzz.float


imageEssentialsFuzzer : Fuzzer ImageEssentials
imageEssentialsFuzzer =
    Fuzz.map5
        (\c ta bg st tr s ->
            { composition = c
            , turnAngle = ta
            , backgroundColor = bg
            , strokeColor = st
            , translate = tr
            , scale = s
            }
        )
        compositionFuzzer
        Fuzz.float
        colorFuzzer
        colorFuzzer
        (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
        |> Fuzz.andMap Fuzz.float


imageAndGalleryFuzzer : Fuzzer ImageAndGallery
imageAndGalleryFuzzer =
    Fuzz.map2 ImageAndGallery
        imageEssentialsFuzzer
        (Fuzz.list imageEssentialsFuzzer)



-- COMPOSITION TRANSFORMS
-- TODO refactor those duplicated functions


compositionToFragment : Composition -> String
compositionToFragment composition =
    Encode.encode 0 (encodeComposition composition)


compositionToUrlOnFragment : Composition -> Url
compositionToUrlOnFragment composition =
    { emptyUrl | fragment = Just (compositionToFragment composition) }


compositionToUrlPercentEncodedOnFragment : Composition -> Url
compositionToUrlPercentEncodedOnFragment composition =
    { emptyUrl | fragment = Just (Url.percentEncode (compositionToFragment composition)) }


compositionToRoute : Composition -> Route
compositionToRoute composition =
    Editor (Just (replaceComposition initialImage composition))



-- N.B. EXPERIMENTING FUZZERS passing tuples like (input, output)


compositionOnFragmentUrlAndRouteFuzzer : Fuzzer ( Url, Route )
compositionOnFragmentUrlAndRouteFuzzer =
    Fuzz.map
        (\fuzzyComposition ->
            ( compositionToUrlOnFragment fuzzyComposition
            , compositionToRoute fuzzyComposition
            )
        )
        compositionFuzzer


compositionOnFragmentUrlPercentEncodedAndRouteFuzzer : Fuzzer ( Url, Route )
compositionOnFragmentUrlPercentEncodedAndRouteFuzzer =
    Fuzz.map
        (\fuzzyComposition ->
            ( compositionToUrlPercentEncodedOnFragment fuzzyComposition
            , compositionToRoute fuzzyComposition
            )
        )
        compositionFuzzer



-- TESTS


suite : Test
suite =
    describe "Main"
        [ describe "Core functions inside `init`"
            [ describe "Encode/Decode image and gallery (localStorage)"
                [ fuzz imageEssentialsFuzzer "Image essentials" <|
                    \fuzzyImage ->
                        fuzzyImage
                            |> encodeImage
                            |> Decode.decodeValue imageDecoder
                            |> Expect.equal (Ok fuzzyImage)
                , fuzz imageAndGalleryFuzzer "Image and Gallery" <|
                    \fuzzyImageAndGallery ->
                        fuzzyImageAndGallery
                            |> encodeImageAndGallery
                            |> Decode.decodeValue imageAndGalleryDecoder
                            |> Expect.equal (Ok fuzzyImageAndGallery)
                , fuzz2 v2_imageAndGalleryFuzzer imageAndGalleryFuzzer "Migrate ImageAndGallery v2 to v3" <|
                    \v2_fuzzyImageAndGallery fuzzyImageAndGallery ->
                        mergeV2toV3 v2_fuzzyImageAndGallery fuzzyImageAndGallery
                            |> Expect.equal
                                { fuzzyImageAndGallery
                                    | gallery =
                                        fuzzyImageAndGallery.gallery
                                            ++ ImageEssentials.extractImage v2_fuzzyImageAndGallery
                                            :: List.map v2_imageToImageEssentials v2_fuzzyImageAndGallery.gallery
                                }
                ]
            , describe "parseUrl"
                -- https://hybridcode.art/
                [ test "Empty URL" <|
                    \_ ->
                        emptyUrl
                            |> parseUrl
                            |> Expect.equal (Editor Nothing)

                -- HAPPY PATH
                -- https://hybridcode.art/
                --                      ?composition=%5B%22DLDDD%22%5D
                --                      &turnAngle=60
                --                      &backgroundColor=%22%23333333%22
                --                      &strokeColor=%22%2300b46e%22
                --                      &translateX=-6.72000000000002
                --                      &translateY=-0.24000000000000055
                --                      &scale=1.480000000000001
                , fuzz imageEssentialsFuzzer "URL v2 - composition on query" <|
                    \fuzzyImage ->
                        Maybe.withDefault emptyUrl (Url.fromString ("https://test.art" ++ ImageEssentials.toUrlPathString fuzzyImage))
                            |> parseUrl
                            |> Expect.equal (Editor (Just fuzzyImage))
                , describe "Backwards compatibility"
                    -- HAPPY PATH
                    -- e.g. https://hybridcode.art/#["DLRS","DLRS"]
                    -- N.B. Experimenting with passing tuples like (input, output)
                    [ fuzz compositionOnFragmentUrlAndRouteFuzzer "URL v1 -- composition on fragment (hash)" <|
                        \( fuzzyUrl, fuzzyRoute ) -> Expect.equal fuzzyRoute (parseUrl fuzzyUrl)

                    -- HAPPY PATH
                    -- e.g. https://hybridcode.art/#%5B%22DLRS%22%2C%22DLRS%22%5D
                    --                               [  " DLRS "   ,  " DLRS "  ]
                    -- N.B. Experimenting with passing tuples like (input, output)
                    , fuzz compositionOnFragmentUrlPercentEncodedAndRouteFuzzer "URL v1 -- percent-encoded composition on fragment (hash)" <|
                        \( fuzzyUrl, fuzzyRoute ) -> Expect.equal fuzzyRoute (parseUrl fuzzyUrl)

                    -- e.g. https://hybridcode.art/#asdfjahdlfkjhadl%%%%%&&&@@@134i987987
                    , fuzz Fuzz.string "Random strings on fragment" <|
                        \fuzzyString ->
                            { emptyUrl | fragment = Just fuzzyString }
                                |> parseUrl
                                |> Expect.equal (Editor Nothing)

                    -- https://hybridcode.art/
                    --                      ?composition=%5B%22DLDDD%22%5D
                    --                      &turnAngle=60
                    --                      &backgroundColor=%22%23333333%22
                    --                      &strokeColor=%22%2300b46e%22
                    --                      &translateX=-6.72000000000002
                    --                      &translateY=-0.24000000000000055
                    --                      &scale=1.480000000000001
                    --                      #["DLRS","DLRS"]
                    , fuzz2 compositionFuzzer imageEssentialsFuzzer "URL v1 and v2 - composition on fragment && on query" <|
                        \fuzzyComposition fuzzyImage ->
                            Maybe.withDefault emptyUrl
                                (Url.fromString
                                    ("https://test.art"
                                        ++ ImageEssentials.toUrlPathString fuzzyImage
                                        ++ "#"
                                        ++ compositionToFragment fuzzyComposition
                                    )
                                )
                                |> parseUrl
                                |> Expect.equal (Editor (Just fuzzyImage))
                    ]

                {--Implement a not-so-strict URL parsing strategy, i.e. allow for missing query parameters.
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



-- BACKWARD COMPATIBILITY STUFF
-- FUZZERS


v2_imageAndGalleryFuzzer : Fuzzer V2_ImageAndGallery
v2_imageAndGalleryFuzzer =
    Fuzz.map5 V2_ImageAndGallery
        compositionFuzzer
        (Fuzz.list v2_imageFuzzer)
        colorFuzzer
        colorFuzzer
        Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap (Fuzz.tuple ( Fuzz.float, Fuzz.float ))


v2_imageFuzzer : Fuzzer V2_Image
v2_imageFuzzer =
    Fuzz.map4 V2_Image
        compositionFuzzer
        Fuzz.float
        colorFuzzer
        colorFuzzer



-- TESTS


backwardCompatibilitySuite : Test
backwardCompatibilitySuite =
    describe "Backwards compatibility"
        [ describe "ImageEssentials Data Structure"
            [ fuzz v2_imageFuzzer "V2_Image Encode Decode" <|
                \fuzzyImage ->
                    fuzzyImage
                        |> v2_encodeImage
                        |> Decode.decodeValue v2_imageDecoder
                        |> Expect.equal (Ok fuzzyImage)
            , fuzz v2_imageAndGalleryFuzzer "V2_ImageAndGallery Encode Decode" <|
                \v2_fuzzyImageAndGallery ->
                    v2_fuzzyImageAndGallery
                        |> v2_encodeImageAndGallery
                        |> Decode.decodeValue v2_imageAndGalleryDecoder
                        |> Expect.equal (Ok v2_fuzzyImageAndGallery)
            ]
        ]
