module Models exposing (Model, createInitialStateWith, defaultInitialState)

import LSystem exposing (State, Step(..), Transformation, stringToStep)


type alias Model =
    { state : State
    , recording : Transformation
    , recOn : Bool
    , history : List Transformation
    , isShowingNextIteration : Bool
    , dir : String
    }


defaultInitialState : State
defaultInitialState =
    [ D, L, D, L, D, L, D ]


defaultInitialRecording : Transformation
defaultInitialRecording =
    [ D ]


createInitialStateWith : Maybe (List String) -> Model
createInitialStateWith localStorage =
    let
        initialState =
            case localStorage of
                Just stored ->
                    List.map stringToStep stored

                Nothing ->
                    defaultInitialState
    in
    Model initialState defaultInitialRecording True [] True ""
