module Msgs exposing (Msg(..))

import LSystem.Core exposing (State, Step, Transformation)


type Msg
    = Add Step
    | Backspace
    | ClearStep
    | ClearSvg
    | SetAsBase State
    | Iterate Transformation
    | Deiterate
    | ToggleShowNextIteration
    | KeyPress String
    | SaveState
    | Exclude Int
    | Zoom Float Float ShiftKey



-- | UpdateSaved (List (List State))


type alias ShiftKey =
    Bool
