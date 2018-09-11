module Msgs exposing (Msg(..))

import LSystem exposing (Step)


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
