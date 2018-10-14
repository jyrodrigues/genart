module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (div, text)
import Json.Decode exposing (Decoder)
import LSystem.Core exposing (State, Step(..), Transformation, stringToStep)
import Models exposing (Model, defaultInitialRecording, defaultInitialState)
import Msgs exposing (Msg)
import Update exposing (update)
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
        [ Browser.Events.onKeyUp (Json.Decode.map Msgs.KeyPress keyDecoder) ]


keyDecoder : Decoder String
keyDecoder =
    Json.Decode.field "key" Json.Decode.string


init : Flags -> ( Model, Cmd Msg )
init localStorage =
    ( createInitialStateWith localStorage, Cmd.none )


createInitialStateWith : List (List String) -> Model
createInitialStateWith localStorage =
    let
        initialState =
            case List.head localStorage of
                Just stored ->
                    List.map stringToStep stored

                Nothing ->
                    defaultInitialState
    in
    Model initialState defaultInitialRecording True [] True "" 0 0 0 False


defaultInitialModel =
    Model defaultInitialState defaultInitialRecording True [] True "" 0 0 0 False



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
