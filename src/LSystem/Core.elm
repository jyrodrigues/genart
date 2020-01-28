module LSystem.Core exposing
    ( State
    , Step(..)
    , Transformation
    , appendToStateAt
    , applyRule
    , buildState
    , charToStep
    , dropLastStepFromStateAt
    , dropStateAt
    , encodeState
    , fromList
    , getSvgBorders
    , getTransformAt
    , makeRule
    , stateDecoder
    , stateLength
    , stateToString
    , stepToString
    , stringToStep
    , toList
    , transformToString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListExtra exposing (dropLast, getAt)



-- TYPES


type Step
    = D
    | R
    | L
    | S


type alias Transformation =
    List Step



-- todo: Make State an opaque type!


type alias State =
    { base : Transformation
    , transforms : List Transformation
    }



-- JSON


encodeState : State -> Encode.Value
encodeState state =
    Encode.list encodeTransform (state.base :: state.transforms)


stateDecoder : Decoder State
stateDecoder =
    let
        listDecoder =
            Decode.list transformDecoder
    in
    Decode.map2 State
        (Decode.map (List.head >> Maybe.withDefault [ D ]) listDecoder)
        (Decode.map (List.tail >> Maybe.withDefault [ [ D, L, D, L, D, L, D ] ]) listDecoder)


encodeTransform : Transformation -> Encode.Value
encodeTransform transform =
    Encode.string (String.join "" (List.map stepToString transform))


transformDecoder : Decoder Transformation
transformDecoder =
    Decode.map (String.toList >> List.map charToStep) Decode.string



-- TAIL, FOR NOW


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


appendToStateAt : Transformation -> Int -> State -> State
appendToStateAt transform =
    changeStateAt (\t -> t ++ transform)


dropLastStepFromStateAt : Int -> State -> State
dropLastStepFromStateAt =
    changeStateAt (\t -> dropLast t)


dropStateAt : Int -> State -> State
dropStateAt =
    -- todo: bug: this is not dropping the transformation, it's replacing it for an empty one
    -- refactor `changeStateAt` to receive (Transformation -> Maybe Transformation) ?
    maybeChangeStateAt (\t -> Nothing)


changeStateAt : (Transformation -> Transformation) -> Int -> State -> State
changeStateAt fn =
    let
        justFn =
            \t -> Just <| fn t
    in
    maybeChangeStateAt justFn


maybeChangeStateAt : (Transformation -> Maybe Transformation) -> Int -> State -> State
maybeChangeStateAt fn index state =
    let
        stateList =
            toList state

        before =
            List.take index stateList

        transformed =
            fn <| getTransformAt index state

        after =
            List.drop (index + 1) stateList
    in
    case transformed of
        Just t ->
            fromList (before ++ [ t ] ++ after)

        Nothing ->
            fromList (before ++ after)


getTransformAt : Int -> State -> Transformation
getTransformAt index state =
    case getAt index (toList state) of
        Just t ->
            t

        Nothing ->
            Debug.log ("Error: Trying to access index " ++ String.fromInt index ++ " on current state. Returning empty: ") []


toList : State -> List Transformation
toList state =
    state.base :: state.transforms


fromList : List Transformation -> State
fromList list =
    case list of
        x :: xs ->
            { base = x, transforms = xs }

        [] ->
            Debug.log
                "Error trying to create state from empty list. Returning a default state: "
                { base = [ D, L, D, L, D, L, D ], transforms = [ [ D ] ] }


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



-- STRING CONVERSION


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
transformToString transform =
    transform
        |> List.map stepToString
        |> String.join " "
