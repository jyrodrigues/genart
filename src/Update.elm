module Update exposing (Msg(..), update)

import Auxiliary exposing (dropLast)
import Element exposing (Color)
import Json.Encode as Encode
import LSystem.Core as LCore
    exposing
        ( State
        , Step(..)
        , Transformation
        , appendToStateAt
        , applyRule
        , buildState
        , dropLastStepFromStateAt
        , dropStateAt
        , getTransformAt
        )
import LSystem.String
import Models exposing (Model, squareState)


type Msg
    = KeyPress String
    | ClearSvg
    | SetAsBase State
    | Iterate Transformation
    | Deiterate
    | ToggleShowNextIteration
    | SaveState
    | Exclude Int
    | Zoom Float Float ShiftKey
      --
    | SetEditingIndex Int
    | DropFromState Int
    | SetBackgroundColor Color
    | SetDrawColor Color



-- | UpdateSaved (List (List State))


type alias ShiftKey =
    Bool



-- port cache : Encode.Value -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg:" msg
    in
    case msg of
        ClearSvg ->
            ( { model | state = Models.squareState }, Cmd.none )

        SetAsBase state ->
            ( { model | state = state }, Cmd.none )

        Iterate transform ->
            ( iterate model transform, Cmd.none )

        Deiterate ->
            ( deiterate model, Cmd.none )

        ToggleShowNextIteration ->
            ( { model | isShowingNextIteration = not model.isShowingNextIteration }, Cmd.none )

        KeyPress keyString ->
            ( processKey model keyString, Cmd.none )

        SaveState ->
            ( { model | savedStates = model.state :: model.savedStates }, Cmd.none )

        Exclude index ->
            let
                newSavedStates =
                    List.take index model.savedStates ++ List.drop (index + 1) model.savedStates
            in
            ( { model | savedStates = newSavedStates }, Cmd.none )

        -- use svg property `transform="scale(2 3.5)"` see: https://developer.mozilla.org/pt-BR/docs/Web/SVG/Attribute/transform
        -- also, see `scaleAbout` in https://github.com/ianmackenzie/elm-geometry-svg/blob/master/src/Geometry/Svg.elm
        -- and later check https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/
        Zoom deltaX deltaY shiftKey ->
            if shiftKey then
                ( { model | wDelta = model.wDelta + deltaX, hDelta = model.hDelta + deltaY }, Cmd.none )

            else
                ( { model | zoomLevel = model.zoomLevel + deltaY }, Cmd.none )

        SetEditingIndex index ->
            ( { model | editingIndex = index }, Cmd.none )

        DropFromState index ->
            let
                newEditingIndex =
                    if model.editingIndex == index then
                        List.length model.state.transforms - 1

                    else if model.editingIndex > index then
                        model.editingIndex - 1

                    else
                        model.editingIndex
            in
            ( { model | state = LCore.dropStateAt index model.state, editingIndex = newEditingIndex }, Cmd.none )

        SetBackgroundColor color ->
            ( { model | backgroundColor = color }, Cmd.none )

        SetDrawColor color ->
            ( { model | drawColor = color }, Cmd.none )


processKey : Model -> String -> Model
processKey model dir =
    case dir of
        "ArrowLeft" ->
            { model | state = LCore.appendToStateAt [ L ] model.editingIndex model.state }

        "ArrowRight" ->
            { model | state = LCore.appendToStateAt [ R ] model.editingIndex model.state }

        "ArrowUp" ->
            { model | state = LCore.appendToStateAt [ D ] model.editingIndex model.state }

        "ArrowDown" ->
            { model | state = LCore.appendToStateAt [ S ] model.editingIndex model.state }

        " " ->
            { model | fixed = not model.fixed }

        "Backspace" ->
            { model | state = LCore.dropLastStepFromStateAt model.editingIndex model.state }

        "i" ->
            let
                newModel =
                    Debug.log "newModel after `i`:" <| iterate model <| LCore.getTransformAt model.editingIndex model.state
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



{--
cacheSavedStates savedStates =
    savedStates
        |> List.map (\state -> List.map LSystem.String.fromStep state)
        |> Encode.list (\state -> Encode.list Encode.string state)
        |> cache
--}


iterate : Model -> Transformation -> Model
iterate model transform =
    let
        state =
            model.state

        newState =
            { state
                | transforms = state.transforms ++ [ transform ]
            }
    in
    { model | state = newState, editingIndex = List.length newState.transforms }


deiterate : Model -> Model
deiterate model =
    let
        state =
            model.state

        newState =
            { state
                | transforms = dropLast state.transforms
            }
    in
    { model | state = newState }
