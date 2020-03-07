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

import Colors exposing (Color(..))
import Expect
import Fuzz exposing (Fuzzer)
import Json.Encode as Encode
import LSystem.Core as LCore exposing (Composition, Step(..), encodeComposition)
import Main exposing (..)
import Test exposing (Test, describe, fuzz, test, todo)
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



-- This helps preventing a bug: `Expect.equal` an empty composition


notEmptyComposition : Composition -> Composition
notEmptyComposition composition =
    if LCore.length composition == 0 then
        LCore.fromList [ [ D ] ]

    else
        composition



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


colorFuzzer : Fuzzer Color
colorFuzzer =
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
            , backgroundColor = Colors.fromHexString <| Colors.toHexString bg
            , strokeColor = Colors.fromHexString <| Colors.toHexString st
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



-- COMPOSITION TRANSFORMS


compositionToFragment : Composition -> String
compositionToFragment composition =
    Encode.encode 0 (encodeComposition (notEmptyComposition composition))


compositionToUrlOnFragment : Composition -> Url
compositionToUrlOnFragment composition =
    { emptyUrl | fragment = Just (compositionToFragment composition) }


compositionToUrlPercentEncodedOnFragment : Composition -> Url
compositionToUrlPercentEncodedOnFragment composition =
    { emptyUrl | fragment = Just (Url.percentEncode (compositionToFragment composition)) }


compositionToRoute : Composition -> Route
compositionToRoute composition =
    Editor (Just (replaceComposition initialImage (notEmptyComposition composition)))



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
            [ describe "parseUrl"
                [ test "Empty URL" <|
                    -- http://hybridcode.art/
                    \_ ->
                        emptyUrl
                            |> parseUrl
                            |> Expect.equal (Editor Nothing)
                , describe "Backwards compatibility"
                    -- e.g. https://hybridcode.art/#["DLRS","DLRS"]
                    -- N.B. Experimenting with passing tuples like (input, output)
                    [ fuzz compositionOnFragmentUrlAndRouteFuzzer "URL v1 -- composition on fragment (hash)" <|
                        \( fuzzyUrl, fuzzyRoute ) -> Expect.equal fuzzyRoute (parseUrl fuzzyUrl)

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

                    -- e.g. TODO
                    , fuzz (Fuzz.tuple ( compositionFuzzer, imageEssentialsFuzzer )) "URL v1 and v2 - composition on fragment && on query" <|
                        \( fuzzyComposition, fuzzyImage ) ->
                            Maybe.withDefault emptyUrl
                                (Url.fromString
                                    ("https://test.art"
                                        ++ imageToUrlString
                                            fuzzyImage
                                        ++ "#"
                                        ++ compositionToFragment fuzzyComposition
                                    )
                                )
                                |> parseUrl
                                |> Expect.equal (Editor (Just fuzzyImage))
                    ]

                -- TODO create ImageEssentials fuzzer
                -- http://localhost:9000/?composition=%5B%22DLDDD%22%5D&turnAngle=60&backgroundColor=%22%23333333%22&strokeColor=%22%2300b46e%22&translateX=-6.72000000000002&translateY=-0.24000000000000055&scale=1.480000000000001
                , fuzz imageEssentialsFuzzer "URL v2 - composition on query" <|
                    \fuzzyImage ->
                        Maybe.withDefault emptyUrl (Url.fromString ("https://test.art" ++ imageToUrlString fuzzyImage))
                            |> parseUrl
                            |> Expect.equal (Editor (Just fuzzyImage))
                , todo "Missing composition"
                , todo "Missing turnAngle"
                , todo "Missing backgroundColor"
                , todo "Missing strokeColor"
                , todo "Missing translateX"
                , todo "Missing translateY"
                , todo "Missing scale"
                ]
            ]
        ]



--Decode.decodeValue
--imageAndGalleryDecoder
--localStorage
