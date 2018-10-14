module Msgs exposing (Msg(..))

import LSystem.Core exposing (State, Step)


type Msg
    = Add Step
    | Backspace
    | ClearStep
    | ClearSvg
    | Iterate
    | Deiterate
    | ToggleShowNextIteration
    | KeyPress String
    | SaveState
    | Zoom Float Float ShiftKey



-- | UpdateSaved (List (List State))


type alias ShiftKey =
    Bool
