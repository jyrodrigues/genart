module AppInit exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (Route(..), parseUrl)
import Test exposing (..)
import Url exposing (Protocol(..))



{--
    Note that Main.init cannot be tested directly because Navigation.Key can't be mocked.

    It seems that Elmer can circumvent this (https://elmer-test.cfapps.io/elmer/)

    Check for more info:
    - https://discourse.elm-lang.org/t/testing-update-for-browser-application-nav-key-gets-in-the-way/2101
    - https://github.com/elm-explorations/test/issues/24
    - https://elmer-test.cfapps.io/elmer/versions/6.0.0/module/Elmer.Navigation
--}


suite : Test
suite =
    describe "Main"
        [ describe "Core functions inside `init`"
            [ describe "parseUrl"
                [ test "Empty URL" <|
                    \_ ->
                        let
                            emptyUrl =
                                { protocol = Http
                                , host = "hybridcode.art"
                                , port_ = Nothing
                                , path = "/"
                                , query = Nothing
                                , fragment = Nothing
                                }
                        in
                        emptyUrl
                            |> parseUrl
                            |> Expect.equal (Editor Nothing)

                {--fuzz runs the test 100 times with randomly-generated inputs!
                , fuzz string "restores the original string if you run it again" <|
                    \randomlyGeneratedString ->
                        randomlyGeneratedString
                            |> String.reverse
                            |> String.reverse
                            |> Expect.equal randomlyGeneratedString
                --}
                , todo "Use Fuzz.map to map compositions to strings andThen to Urls and check final Route"
                ]
            ]
        ]



--Decode.decodeValue
--imageAndGalleryDecoder
--localStorage
