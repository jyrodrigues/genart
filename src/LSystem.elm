module LSystem exposing (State, Step(..), Transformation, apply, makeRule, rebuildState, stateToString, stepToString, stringToStep)


type Step
    = D
    | R
    | L
    | S


type alias State =
    List Step


type alias Transformation =
    State


rebuildState : State -> List Transformation -> State
rebuildState baseState history =
    List.foldl apply baseState history


apply : Transformation -> State -> State
apply transformation baseState =
    let
        rule =
            makeRule transformation
    in
    List.concatMap rule baseState


makeRule : Transformation -> Step -> State
makeRule state step =
    case step of
        D ->
            state

        _ ->
            [ step ]


stateToString : State -> String
stateToString state =
    state
        |> List.map stepToString
        |> String.join " "


stepToString : Step -> String
stepToString step =
    case step of
        D ->
            "D"

        R ->
            "R"

        L ->
            "L"

        S ->
            "S"


stringToStep : String -> Step
stringToStep char =
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
