module Utils exposing (..)

import Process
import Task


delay : Float -> msg -> Cmd msg
delay milliseconds msg =
    Process.sleep milliseconds
        |> Task.perform (\_ -> msg)
