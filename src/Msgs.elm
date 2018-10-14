module Msgs exposing (Msg(..))

import LSystem.Core exposing (Step)


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


type alias ShiftKey =
    Bool
