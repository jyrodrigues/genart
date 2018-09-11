module Models exposing (Model, initialState)

import LSystem exposing (State, Step(..), Transformation)


type alias Model =
    { state : State
    , recording : Transformation
    , recOn : Bool
    , history : List Transformation
    , isShowingNextIteration : Bool
    , dir : String
    }


initialState : State
initialState =
    [ D, L, D, L, D, L, D ]
