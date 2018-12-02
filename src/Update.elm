port module Update exposing (Msg(..), update)

import Auxiliary exposing (dropLast)
import Json.Encode as Encode
import LSystem.Core
    exposing
        ( State
        , Step(..)
        , Transformation
        , appendToLastTransform
        , applyRule
        , buildState
        , dropFromLastTransform
        , dropLastTransform
        , getLastTransform
        )
import LSystem.String
import Models exposing (Model, squareState)


type Msg
    = KeyPress String
    | ResetStep
    | ClearSvg
    | SetAsBase State
    | Iterate Transformation
    | Deiterate
    | ToggleShowNextIteration
    | SaveState
    | Exclude Int
    | Zoom Float Float ShiftKey



-- | UpdateSaved (List (List State))


type alias ShiftKey =
    Bool


port cache : Encode.Value -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetStep ->
            ( { model | state = dropLastTransform model.state }, Cmd.none )

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

        KeyPress dir ->
            ( processKey model dir, Cmd.none )

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


cacheSavedStates savedStates =
    savedStates
        |> List.map (\state -> List.map LSystem.String.fromStep state)
        |> Encode.list (\state -> Encode.list Encode.string state)
        |> cache


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
    { model | state = newState }


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


processKey : Model -> String -> Model
processKey model dir =
    case dir of
        "ArrowLeft" ->
            { model | state = appendToLastTransform [ L ] model.state }

        "ArrowRight" ->
            { model | state = appendToLastTransform [ R ] model.state }

        "ArrowUp" ->
            { model | state = appendToLastTransform [ D ] model.state }

        "ArrowDown" ->
            { model | state = appendToLastTransform [ S ] model.state }

        " " ->
            { model | fixed = not model.fixed }

        "Backspace" ->
            { model | state = dropFromLastTransform model.state }

        "i" ->
            let
                newModel =
                    iterate model <| getLastTransform model.state
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
