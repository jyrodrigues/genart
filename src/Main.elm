module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Step
    = D
    | R
    | L
    | S


type alias State =
    List Step


type alias Model =
    { state : State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddD


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddD ->
            ( { model | state = model.state ++ [ D ] }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddD ] [ text "Add D" ]
        , div [] [ text (stateToString model.state) ]
        ]



-- AUXILIARY


stateToString state =
    state
        |> List.map stepToChar
        |> List.map String.fromChar
        |> String.join " "


stepToChar step =
    case step of
        D ->
            'D'

        R ->
            'R'

        L ->
            'L'

        S ->
            'S'
