port module Update exposing (update)

import Auxiliary exposing (dropLast)
import Json.Encode as Encode
import LSystem.Core exposing (Step(..), Transformation, applyRule, rebuildState, stepToString)
import Models exposing (Model, squareState)
import Msgs exposing (Msg(..))


port cache : Encode.Value -> Cmd msg


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
            ( { model | state = Models.squareState }, Cmd.none )

        SetAsBase state ->
            ( { model | state = state }, Cmd.none )

        Iterate transformation ->
            ( iterate model transformation, Cmd.none )

        Deiterate ->
            ( deiterate model, Cmd.none )

        ToggleShowNextIteration ->
            ( { model | isShowingNextIteration = not model.isShowingNextIteration }, Cmd.none )

        KeyPress dir ->
            ( processKey model dir, Cmd.none )

        SaveState ->
            let
                newSavedStates =
                    if model.isShowingNextIteration then
                        [ applyRule model.recording model.state ] ++ model.savedStates

                    else
                        [ model.state ] ++ model.savedStates
            in
            ( { model | savedStates = newSavedStates }
            , cacheSavedStates newSavedStates
            )

        Exclude index ->
            let
                newSavedStates =
                    List.take index model.savedStates ++ List.drop (index + 1) model.savedStates
            in
            ( { model | savedStates = newSavedStates }, cacheSavedStates newSavedStates )

        -- cache <| Encode.list Encode.string <| List.map (\state -> List.map stepToString state) )
        Zoom deltaX deltaY shiftKey ->
            if shiftKey then
                ( { model | wDelta = model.wDelta + deltaX, hDelta = model.hDelta + deltaY }, Cmd.none )

            else
                ( { model | zoomLevel = model.zoomLevel + deltaY }, Cmd.none )


cacheSavedStates savedStates =
    savedStates
        |> List.map (\state -> List.map stepToString state)
        |> Encode.list (\state -> Encode.list Encode.string state)
        |> cache


iterate : Model -> Transformation -> Model
iterate model transformation =
    { model
        | state =
            applyRule transformation model.state
        , history =
            model.history ++ [ transformation ]
    }


deiterate : Model -> Model
deiterate model =
    let
        newHistory =
            dropLast model.history
    in
    { model
        | state = rebuildState model.baseState newHistory
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
            { model | fixed = not model.fixed }

        "Backspace" ->
            { model | recording = dropLast model.recording, dir = dir }

        "i" ->
            let
                newModel =
                    iterate model model.recording
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
