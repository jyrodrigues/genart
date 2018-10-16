module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (div, text)
import Json.Decode exposing (Decoder)
import LSystem.Core exposing (State, Step(..), Transformation, stringToStep)
import Models exposing (Model, onlyD, squareState)
import Update exposing (Msg(..), update)
import View exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Flags =
    List (List String)



-- put ports here. To do that put an update function here with, say, 2 msg cases that calls update from the Update.elm file


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyUp (Json.Decode.map KeyPress keyDecoder) ]


keyDecoder : Decoder String
keyDecoder =
    Json.Decode.field "key" Json.Decode.string


init : Flags -> ( Model, Cmd Msg )
init localStorage =
    ( createInitialModelWith localStorage, Cmd.none )


createInitialModelWith : List (List String) -> Model
createInitialModelWith localStorage =
    let
        savedStates =
            localStorage
                |> List.map (\state -> List.map stringToStep state)
    in
    Model
        squareState
        onlyD
        True
        []
        savedStates
        squareState
        True
        ""
        0
        0
        0
        True


defaultInitialModel =
    Model
        squareState
        onlyD
        True
        []
        []
        squareState
        True
        ""
        0
        0
        0
        True



{--
init _ =
    ( Models.defaultInitialModel, Cmd.none )
--}
-- SUBS ALTER
-- type Direction
--     = Left
--     | Right
--     | Up
--     | Skip
--     | Backspace1
--     | Other
-- -- keyDecoder : Decoder Direction
-- -- Decode.map toDirection (Decode.field "key" Decode.string)
--
--
--
--
-- toDirection : String -> Direction
-- toDirection string =
--     case string of
--         "ArrowLeft" ->
--             Left
--         "ArrowRight" ->
--             Right
--         "ArrowUp" ->
--             Up
--         "Space" ->
--             Skip
--         "Backspace" ->
--             Backspace1
--         _ ->
--             Other
-- [D L D D L D D D D L D D L D D D D D D R D L D L D D D D D D D L]
