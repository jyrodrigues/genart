module AppInit exposing (..)

{--
    Note that Main.init cannot be tested directly because Navigation.Key can't be mocked.

    So we import everything from Main because anything other than `main` is exposed for testing.

    It seems that Elmer can circumvent this (https://elmer-test.cfapps.io/elmer/)

    Check for more info:
    - https://discourse.elm-lang.org/t/testing-update-for-browser-application-nav-key-gets-in-the-way/2101
    - https://github.com/elm-explorations/test/issues/24
    - https://elmer-test.cfapps.io/elmer/versions/6.0.0/module/Elmer.Navigation
--}

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Json.Encode as Encode
import LSystem.Core as LCore exposing (Composition, Step(..), encodeComposition)
import Main exposing (..)
import Test exposing (Test, describe, fuzz, only, test, todo)
import Url exposing (Protocol(..), Url)


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


emptyUrl : Url
emptyUrl =
    { protocol = Https
    , host = "test.art"
    , port_ = Nothing
    , path = "/"
    , query = Nothing
    , fragment = Nothing
    }


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
                    [ fuzz compositionFuzzer "URL v1 -- composition on fragment (hash)" <|
                        \fuzzyComposition ->
                            { emptyUrl | fragment = Just (Encode.encode 0 (encodeComposition fuzzyComposition)) }
                                |> parseUrl
                                |> Expect.equal (Editor (Just (replaceComposition initialImage fuzzyComposition)))

                    -- e.g. https://hybridcode.art/#%5B%22DLRS%22%2C%22DLRS%22%5D
                    --                               [  " DLRS "   ,  " DLRS "  ]
                    , fuzz compositionFuzzer "URL v1 -- percent-encoded composition on fragment (hash)" <|
                        \fuzzyComposition ->
                            { emptyUrl | fragment = Just (Url.percentEncode (Encode.encode 0 (encodeComposition fuzzyComposition))) }
                                |> parseUrl
                                |> Expect.equal (Editor (Just (replaceComposition initialImage fuzzyComposition)))

                    -- e.g. https://hybridcode.art/#asdfjahdlfkjhadl%%%%%&&&@@@134i987987
                    , fuzz Fuzz.string "Random strings on fragment" <|
                        \fuzzyString ->
                            { emptyUrl | fragment = Just fuzzyString }
                                |> parseUrl
                                |> Expect.equal (Editor Nothing)
                    , todo "URL v1 and v2 - composition on fragment && on query"
                    ]

                -- TODO create ImageEssentials fuzzer
                -- http://localhost:9000/?composition=%5B%22DLDDDLDDLDDDLDLDLDDDDDDRDDRDDRRDDRRDDDD%22%2C%22DLDDDLDDLDDDLDLDLDDDDDDRDDRDDRRD%22%2C%22DLDDDLDDLDDDLDLDLDDDDDDRDDRDDRRDDRRDDDD%22%5D&turnAngle=60&backgroundColor=%22%23333333%22&strokeColor=%22%2300b46e%22&translateX=-6.72000000000002&translateY=-0.24000000000000055&scale=1.480000000000001
                , todo "URL v2 - composition on query"
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
