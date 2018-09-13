module Update exposing (update)

import Auxiliary exposing (dropLast)
import Json.Encode as Encode
import LSystem exposing (Step(..), apply, rebuildState, stepToString)
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Ports


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- let
    --     _ =
    --         Debug.log "model" model
    -- in
    case msg of
        Add step ->
            ( { model | recording = model.recording ++ [ step ] }, Cmd.none )

        Backspace ->
            ( { model | recording = dropLast model.recording }, Cmd.none )

        ClearStep ->
            ( { model | recording = [] }, Cmd.none )

        ClearSvg ->
            ( { model | state = Models.defaultInitialState }, Cmd.none )

        Iterate ->
            ( iterate model, Cmd.none )

        Deiterate ->
            ( deiterate model, Cmd.none )

        ToggleShowNextIteration ->
            ( { model | isShowingNextIteration = not model.isShowingNextIteration }, Cmd.none )

        KeyPress dir ->
            ( processKey model dir, Cmd.none )

        SaveState ->
            ( model, Ports.cache <| Encode.list Encode.string <| List.map stepToString model.state )


iterate : Model -> Model
iterate model =
    { model
        | state =
            apply model.recording model.state
        , history =
            model.history ++ [ model.recording ]
    }


deiterate : Model -> Model
deiterate model =
    let
        newHistory =
            dropLast model.history
    in
    { model
        | state = rebuildState Models.defaultInitialState newHistory
        , history = newHistory
    }


processKey : Model -> String -> Model
processKey model dir =
    case dir of
        "r" ->
            { model | recOn = not model.recOn, dir = dir }

        "ArrowLeft" ->
            { model | recording = model.recording ++ [ L ], dir = dir }

        "ArrowRight" ->
            { model | recording = model.recording ++ [ R ], dir = dir }

        "ArrowUp" ->
            { model | recording = model.recording ++ [ D ], dir = dir }

        "ArrowDown" ->
            { model | recording = model.recording ++ [ S ], dir = dir }

        " " ->
            { model | isShowingNextIteration = not model.isShowingNextIteration }

        "Backspace" ->
            { model | recording = dropLast model.recording, dir = dir }

        "i" ->
            let
                newModel =
                    iterate model
            in
            { newModel | dir = dir }

        "d" ->
            let
                newModel =
                    deiterate model
            in
            { newModel | dir = dir }

        _ ->
            { model | dir = dir }
