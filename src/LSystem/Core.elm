module LSystem.Core exposing
    ( State
    , Step(..)
    , Transformation
    , applyRule
    , countSize
    , makeRule
    , rebuildState
    , stateToString
    , stepToString
    , stringToStep
    )


type Step
    = D
    | R
    | L
    | S



-- Todo: Change State to be Base + List Transformations


type alias State =
    List Step


type alias Transformation =
    State


rebuildState : State -> List Transformation -> State
rebuildState baseState history =
    List.foldl applyRule baseState history


applyRule : Transformation -> State -> State
applyRule transformation baseState =
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



-- toString Methods


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



-- MEASURING


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
    Tuple.second <| List.foldl countMax ( initialPos, initialMaxes ) state
