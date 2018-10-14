module Models exposing (Model, onlyD, squareState)

import LSystem.Core exposing (State, Step(..), Transformation)


type alias Model =
    -- Todo: remove state, since it can be replaced by baseState + history!
    { state : State
    , recording : Transformation

    -- Todo: remove recOn. Those update branches affected by this refactor problaby should be grouped on a type
    , recOn : Bool
    , history : List Transformation
    , savedStates : List State
    , baseState : State

    -- , savedTransformations : List Transformation
    , isShowingNextIteration : Bool
    , dir : String
    , zoomLevel : Float
    , wDelta : Float
    , hDelta : Float
    , fixed : Bool
    }


squareState : State
squareState =
    [ D, L, D, L, D, L, D ]


onlyD : Transformation
onlyD =
    [ D ]
