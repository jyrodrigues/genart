module Models exposing (Model, defaultInitialRecording, defaultInitialState)

import LSystem exposing (State, Step(..), Transformation)


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
