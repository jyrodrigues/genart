module Models exposing (Model, createInitialStateWith, defaultInitialState)

import LSystem exposing (State, Step(..), Transformation, stringToStep)


type alias Model =
    { state : State
    , recording : Transformation
    , recOn : Bool
    , history : List Transformation

    -- , savedStates : List State
    -- , savedTransformations : List Transformation
    , isShowingNextIteration : Bool
    , dir : String
    , zoomLevel : Int
    }


defaultInitialState : State
defaultInitialState =
    [ D, L, D, L, D, L, D ]


defaultInitialRecording : Transformation
defaultInitialRecording =
    [ D ]


createInitialStateWith : List (List String) -> Model
createInitialStateWith localStorage =
    let
        initialState =
            case List.head localStorage of
                Just stored ->
                    List.map stringToStep stored

                Nothing ->
                    defaultInitialState
    in
    Model initialState defaultInitialRecording True [] True "" 0
