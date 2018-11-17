module LSystem.Core exposing
    ( State
    , Step(..)
    , Transformation
    , appendToLastTransform
    , applyRule
    , buildState
    , countSize
    , dropFromLastTransform
    , dropLastTransform
    , getLastTransform
    , makeRule
    , stateLength
    , stateToString
    , stepToString
    , stringToStep
    , transformToString
    )

import Auxiliary exposing (dropLast)


type Step
    = D
    | R
    | L
    | S


type alias Transformation =
    List Step


type alias State =
    { base : Transformation
    , transforms : List Transformation
    }


buildState : State -> Transformation
buildState state =
    List.foldl applyRule state.base state.transforms


applyRule : Transformation -> Transformation -> Transformation
applyRule transformation baseState =
    let
        rule =
            makeRule transformation
    in
    List.concatMap rule baseState


makeRule : Transformation -> Step -> Transformation
makeRule transformation step =
    case step of
        D ->
            transformation

        _ ->
            [ step ]


appendToLastTransform : Transformation -> State -> State
appendToLastTransform transform state =
    changeLastTransform (\t -> t ++ transform) state


dropFromLastTransform : State -> State
dropFromLastTransform state =
    changeLastTransform (\t -> dropLast t) state


dropLastTransform : State -> State
dropLastTransform state =
    changeLastTransform (\t -> [ D ]) state


changeLastTransform : (Transformation -> Transformation) -> State -> State
changeLastTransform fn state =
    let
        lastTransform =
            case
                state.transforms
                    |> List.reverse
                    |> List.head
            of
                Just a ->
                    a

                Nothing ->
                    []

        newLastTransform =
            fn lastTransform
    in
    { state | transforms = List.take (List.length state.transforms - 1) state.transforms ++ [ newLastTransform ] }


getLastTransform : State -> Transformation
getLastTransform state =
    case
        state.transforms
            |> List.reverse
            |> List.head
    of
        Just a ->
            a

        Nothing ->
            []



-- Todo: make a decoder directly to state from js array


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


stateToString : State -> String
stateToString state =
    buildState state
        |> List.map stepToString
        |> String.join " "


transformToString : Transformation -> String
transformToString transformation =
    transformation
        |> List.map stepToString
        |> String.join " "


{-|


## MEASURING

-}
type Direction
    = Up
    | Down
    | Right
    | Left


type alias Maxes =
    { maxX : Int
    , minX : Int
    , maxY : Int
    , minY : Int
    }


type alias Pos =
    { x : Int
    , y : Int
    , direction : Direction
    }


initialMaxes : Maxes
initialMaxes =
    { maxX = 0
    , minX = 0
    , maxY = 0
    , minY = 0
    }



-- Todo: make `Right` a parameter


initialPos : Pos
initialPos =
    { x = 0
    , y = 0
    , direction = Right
    }


countMax : Step -> ( Pos, Maxes ) -> ( Pos, Maxes )
countMax step ( pos, maxes ) =
    let
        nextPos =
            case pos.direction of
                Up ->
                    { pos | y = pos.y - 1 }

                Down ->
                    { pos | y = pos.y + 1 }

                Right ->
                    { pos | x = pos.x + 1 }

                Left ->
                    { pos | x = pos.x - 1 }

        -- Could change to a list [maxx, minx, maxy, miny] for simplicity?
        nextMaxes =
            if nextPos.x > maxes.maxX then
                { maxes | maxX = nextPos.x }

            else if nextPos.x < maxes.minX then
                { maxes | minX = nextPos.x }

            else if nextPos.y > maxes.maxY then
                { maxes | maxY = nextPos.y }

            else if nextPos.y < maxes.minY then
                { maxes | minY = nextPos.y }

            else
                maxes
    in
    case step of
        D ->
            ( nextPos, nextMaxes )

        _ ->
            ( { pos | direction = changeDirection step pos.direction }, maxes )



-- Todo: Try to make a simpler changeDirection function
-- type GNState a
--     = NotSeen a
--     | GetNext
--     | Found a
-- getNext : a -> List a -> a
-- getNext sym symList =
--     let
--         fn gnState =
--             Found
--     in
--     List.foldl fn (NotSeen sym) symList


changeDirection : Step -> Direction -> Direction
changeDirection step dir =
    case step of
        L ->
            case dir of
                Up ->
                    Left

                Left ->
                    Down

                Down ->
                    Right

                Right ->
                    Up

        R ->
            case dir of
                Up ->
                    Right

                Right ->
                    Down

                Down ->
                    Left

                Left ->
                    Up

        _ ->
            dir


countSize : State -> Maxes
countSize state =
    Tuple.second <| List.foldl countMax ( initialPos, initialMaxes ) <| buildState state



-- Todo: multiply each D by length of next transform


stateLength : State -> Int
stateLength state =
    let
        countD transform =
            List.foldl
                (\step acc ->
                    if step == D then
                        acc + 1

                    else
                        acc
                )
                0
                transform
    in
    List.foldl
        (\l acc -> countD l * acc)
        (countD state.base)
        state.transforms
