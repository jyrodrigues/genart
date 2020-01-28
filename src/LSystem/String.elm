module LSystem.String exposing
    ( charToStep
    , fromState
    , fromStep
    , fromTransform
    , toStep
    )

import LSystem.Core exposing (State, Step(..), Transformation)



-- Todo: make a decoder directly to state from js array and then remove toStep


charToStep : Char -> Step
charToStep char =
    case char of
        'D' ->
            D

        'R' ->
            R

        'L' ->
            L

        'S' ->
            S

        _ ->
            S


toStep : String -> Step
toStep char =
    case char of
        "D" ->
            D

        "R" ->
            R

        "L" ->
            L

        "S" ->
            S

        _ ->
            S


fromStep : Step -> String
fromStep step =
    case step of
        D ->
            "D"

        R ->
            "R"

        L ->
            "L"

        S ->
            "S"


fromState : State -> String
fromState state =
    LSystem.Core.buildState state
        |> List.map fromStep
        |> String.join " "


fromTransform : Transformation -> String
fromTransform transform =
    transform
        |> List.map fromStep
        |> String.join " "
