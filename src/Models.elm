module Models exposing (Model, createInitialStateWith, defaultInitialModel, defaultInitialState)

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
    , zoomLevel : Float
    , wDelta : Float
    , hDelta : Float
    , fixed : Bool
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
    Model initialState defaultInitialRecording True [] True "" 0 0 0 False


defaultInitialModel =
    Model defaultInitialState defaultInitialRecording True [] True "" 0 0 0 False
