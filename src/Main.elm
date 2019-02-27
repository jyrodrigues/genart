module Main exposing (main)

import Browser
import Browser.Events
import Colors
import Html exposing (div, text)
import Json.Decode exposing (Decoder)
import LSystem.Core exposing (State, Step(..), Transformation)
import Models exposing (Model, squareState)
import Update exposing (Msg(..), update)
import View exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- type alias Flags =
--     List (List String)
-- init : Flags -> ( Model, Cmd Msg )
-- init localStorage =
--     ( createInitialModelWith localStorage, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultInitialModel, Cmd.none )



-- Todo: put ports here. To do that put an update function here with, say, 2 msg cases that calls update from the Update.elm file


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyUp (Json.Decode.map KeyPress keyDecoder) ]


keyDecoder : Decoder String
keyDecoder =
    Json.Decode.field "key" Json.Decode.string


createInitialModelWith : List (List String) -> Model
createInitialModelWith localStorage =
    let
        -- Todo: next version of storage, i.e. 'genart/v1/savedStates' or something in those lines
        -- savedStates =
        --     localStorage
        --         |> List.map (\state -> List.map stringToStep state)
        savedStates =
            []
    in
    Model
        squareState
        1
        savedStates
        squareState
        True
        ""
        0
        0
        0
        True
        Colors.darkGray


defaultInitialModel =
    Model
        squareState
        1
        []
        squareState
        True
        ""
        0
        0
        0
        True
        Colors.darkGray



-- [D L D D L D D D D L D D L D D D D D D R D L D L D D D D D D D L]
