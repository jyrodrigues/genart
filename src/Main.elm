module Main exposing (main)

import Browser
import Experiment exposing (experiment)
import Html exposing (div, text)
import Models exposing (Model)
import Msgs exposing (Msg)
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)



{--
main =
    experiment
--}


{--}
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



{--
init : List (List String) -> ( Model, Cmd Msg )
init _ =
    ( Models.defaultInitialModel, Cmd.none )
--}


{--}
init : List (List String) -> ( Model, Cmd Msg )
init localStorage =
    ( Models.createInitialStateWith localStorage, Cmd.none )
--}
