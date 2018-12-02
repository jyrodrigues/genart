module Models exposing (Model, squareState)

import LSystem.Core exposing (State, Step(..), Transformation)


type alias Model =
    { state : State
    , savedStates : List State
    , baseState : State

    -- , savedTransforms : List Transform
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
