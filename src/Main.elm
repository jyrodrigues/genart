module Main exposing (init, main)

import Browser
import Models exposing (Model)
import Msgs exposing (Msg)
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Maybe (List String) -> ( Model, Cmd Msg )
init localStorage =
    ( Models.createInitialStateWith localStorage, Cmd.none )
