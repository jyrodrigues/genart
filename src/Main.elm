module Main exposing (init, main)

import Browser
import LSystem exposing (Step(..), stringToStep)
import Models exposing (Model, initialState)
import Msgs exposing (Msg(..))
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Maybe (List String) -> ( Model, Cmd Msg )
init localStorageState =
    let
        savedState =
            case localStorageState of
                Just storedList ->
                    List.map stringToStep storedList

                Nothing ->
                    initialState
    in
    ( Model savedState [ D ] True [] True ""
    , Cmd.none
    )
