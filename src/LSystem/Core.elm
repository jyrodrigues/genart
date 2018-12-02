module LSystem.Core exposing
    ( State
    , Step(..)
    , Transformation
    , appendToLastTransform
    , applyRule
    , buildState
    , dropFromLastTransform
    , dropLastTransform
    , getLastTransform
    , getSvgBorders
    , makeRule
    , stateLength
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


makeRule : Transformation -> Step -> Transformation
makeRule transform step =
    case step of
        D ->
            transform

        _ ->
            [ step ]


applyRule : Transformation -> Transformation -> Transformation
applyRule transform baseState =
    let
        rule =
            makeRule transform
    in
    List.concatMap rule baseState


buildState : State -> Transformation
buildState state =
    List.foldl applyRule state.base state.transforms


stateLength : State -> ( Int, Int )
stateLength state =
    let
        countSteps transform =
            List.foldl
                (\step acc ->
                    if step == D then
                        ( Tuple.first acc + 1, Tuple.second acc )

                    else
                        ( Tuple.first acc, Tuple.second acc + 1 )
                )
                ( 0, 0 )
                transform
    in
    List.foldl
        (\l acc ->
            let
                ( ld, lo ) =
                    countSteps l

                ( accd, acco ) =
                    acc
            in
            ( accd * ld, accd * lo + acco )
        )
        (countSteps state.base)
        state.transforms


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


{-|


## MEASURING

-}
type Direction
    = Up
    | Down
    | Right
    | Left


type alias Maxes =
    { maxX : Float
    , minX : Float
    , maxY : Float
    , minY : Float
    }


type alias Pos =
    { x : Float
    , y : Float
    , direction : Direction
    }


initialMaxes : Maxes
initialMaxes =
    { maxX = 1
    , minX = -1
    , maxY = 1
    , minY = -1
    }


initialPos : Direction -> Pos
initialPos dir =
    { x = 0
    , y = 0
    , direction = Right
    }


countMax : Step -> ( Pos, Maxes ) -> ( Pos, Maxes )
countMax step ( pos, maxes ) =
    case step of
        D ->
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

                nextMaxes =
                    { maxX = max maxes.maxX nextPos.x
                    , minX = min maxes.minX nextPos.x
                    , maxY = max maxes.maxY nextPos.y
                    , minY = min maxes.minY nextPos.y
                    }
            in
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


getSvgBorders : Transformation -> Maxes
getSvgBorders transform =
    let
        ( finalPos, finalMaxes ) =
            List.foldl countMax ( initialPos Right, initialMaxes ) transform
    in
    { maxX = finalMaxes.maxX + 1
    , minX = finalMaxes.minX - 1
    , maxY = finalMaxes.maxY + 1
    , minY = finalMaxes.minY - 1
    }
