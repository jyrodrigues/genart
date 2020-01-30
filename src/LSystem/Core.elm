module LSystem.Core exposing
    ( Block
    , Composition
    , Step(..)
    , appendStepAtIndex
    , blockToString
    , blocks
    , changeBase
    , changeBlocks
    , compositionDecoder
    , digestComposition
    , dropEntireBlockAtIndex
    , dropLastBlock
    , dropLastStepAtIndex
    , encodeComposition
    , fromList
    , getBlockAtIndex
    , getSvgBorders
    , stepsLength
    , toList
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListExtra



-- TYPES


type Step
    = D
    | R
    | L
    | S


type alias Block =
    List Step


type Composition
    = Composition Block (List Block)



-- GET INFO FROM COMPOSITION


base : Composition -> Block
base (Composition base_ _) =
    base_


changeBase : Block -> Composition -> Composition
changeBase newBase (Composition _ blocks_) =
    Composition newBase blocks_


blocks : Composition -> List Block
blocks (Composition _ blocks_) =
    blocks_


changeBlocks : List Block -> Composition -> Composition
changeBlocks newBlocks (Composition base_ _) =
    Composition base_ newBlocks


stepsLength : Composition -> ( Int, Int )
stepsLength composition =
    let
        countStep step ( dSteps, otherSteps ) =
            case step of
                D ->
                    ( dSteps + 1, otherSteps )

                _ ->
                    ( dSteps, otherSteps + 1 )

        countSteps block =
            List.foldl countStep ( 0, 0 ) block

        accumulateLength ( blockCountD, blockCountOthers ) ( accCountD, accCountOthers ) =
            ( blockCountD * accCountD, accCountOthers + accCountD * blockCountOthers )
    in
    composition
        |> toList
        |> List.map countSteps
        |> List.foldl accumulateLength ( 1, 0 )



-- PROCESS COMPOSITION


digestComposition : Composition -> Block
digestComposition (Composition base_ blocks_) =
    let
        digestOne block step =
            case step of
                D ->
                    block

                _ ->
                    [ step ]

        digest nextBlock digestedSoFar =
            List.concatMap (digestOne nextBlock) digestedSoFar
    in
    List.foldl digest base_ blocks_



-- EDIT COMPOSITION


appendStepAtIndex : Step -> Int -> Composition -> Composition
appendStepAtIndex step =
    editBlockAtIndex (ListExtra.pushLast step)


dropLastStepAtIndex : Int -> Composition -> Composition
dropLastStepAtIndex =
    editBlockAtIndex ListExtra.dropLast


editBlockAtIndex : (Block -> Block) -> Int -> Composition -> Composition
editBlockAtIndex editFn blockIndex composition =
    let
        _ =
            if blockIndex >= List.length (toList composition) then
                Debug.log "editBlockAtIndex - NoOp, index out of bounds" blockIndex

            else
                -1

        editAtIndex currentBlockIndex block =
            if currentBlockIndex == blockIndex then
                editFn block

            else
                block
    in
    fromList (List.indexedMap editAtIndex (toList composition))


dropLastBlock : Composition -> Composition
dropLastBlock (Composition base_ blocks_) =
    Composition base_ (ListExtra.dropLast blocks_)


dropEntireBlockAtIndex : Int -> Composition -> Composition
dropEntireBlockAtIndex blockIndex composition =
    let
        list =
            toList composition

        length =
            List.length list
    in
    if length == 1 then
        Debug.log "dropEntireBlockAtIndex - NoOp, composition has only one block" composition

    else if blockIndex >= length then
        Debug.log "dropEntireBlockAtIndex - NoOp, index out of bounds" composition

    else
        fromList (ListExtra.dropIndex blockIndex list)


getBlockAtIndex : Int -> Composition -> Maybe Block
getBlockAtIndex blockIndex composition =
    List.head (List.drop blockIndex (toList composition))



{--
duplicateBlockAndAppendLast : Int -> Composition -> Composition
duplicateBlockAndAppendLast blockIndex composition =
    applyIfInRange blockIndex composition

applyIfInRange : Int -> (Composition -> Composition) -> Composition -> Composition
applyIfInRange index fn composition =
    if isInRange index composition then
        fn composition

    else
        composition


isInRange : Int -> Composition -> Bool
isInRange index composition =
    index <= List.length (toList composition)

--}
-- COMPOSITION CONVERSIONS


toList : Composition -> List Block
toList (Composition base_ blocks_) =
    base_ :: blocks_


{-| Note that if the list is empty an almost blank Composition is created
-}
fromList : List Block -> Composition
fromList blocks_ =
    case blocks_ of
        head :: tail ->
            Composition head tail

        [] ->
            Debug.log
                "Debug.log - Error trying to create composition from empty list. Returning default: "
                (Composition [ D ] [])



-- BLOCK CONVERSIONS


blockToString : Block -> String
blockToString =
    String.fromList << List.map stepToChar



-- STEP CONVERSIONS


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


stepToChar : Step -> Char
stepToChar step =
    case step of
        D ->
            'D'

        R ->
            'R'

        L ->
            'L'

        S ->
            'S'



-- JSON


{-| State encoder and decoder
-}
encodeComposition : Composition -> Encode.Value
encodeComposition composition =
    Encode.list encodeBlock (toList composition)


compositionDecoder : Decoder Composition
compositionDecoder =
    Decode.map fromList (Decode.list blockDecoder)


{-| Block encoder and decoder
-}
encodeBlock : Block -> Encode.Value
encodeBlock block =
    Encode.string (String.fromList (List.map stepToChar block))


blockDecoder : Decoder Block
blockDecoder =
    Decode.map (String.toList >> List.map charToStep) Decode.string



-- MEASURING


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


getSvgBorders : Block -> Maxes
getSvgBorders block =
    let
        ( finalPos, finalMaxes ) =
            List.foldl countMax ( initialPos Right, initialMaxes ) block
    in
    { maxX = finalMaxes.maxX + 1
    , minX = finalMaxes.minX - 1
    , maxY = finalMaxes.maxY + 1
    , minY = finalMaxes.minY - 1
    }
