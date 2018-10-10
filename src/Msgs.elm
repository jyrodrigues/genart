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
    | Zoom DeltaX DeltaY ShiftKey


type alias DeltaX =
    Int


type alias DeltaY =
    Int


type alias ShiftKey =
    Bool
