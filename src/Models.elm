module Models exposing (Model, squareState)

import LSystem.Core exposing (State, Step(..), Transformation)


type alias Model =
    { state : State

    -- Todo: remove recOn. Those update branches affected by this refactor problaby should be grouped on a type
    , recOn : Bool
    , savedStates : List State
    , baseState : State

    -- , savedTransformations : List Transformation
    , isShowingNextIteration : Bool
    , dir : String
    , zoomLevel : Float
    , wDelta : Float
    , hDelta : Float

    -- Todo: remove fixed. "space" should just center the image
    , fixed : Bool
    }


squareState : State
squareState =
    { base = [ D, L, D, L, D, L, D ]
    , transforms = [ [ D ] ]
    }
