module LSystem.Core exposing
    ( Block
    , Composition
    , Step(..)
    , appendBlock
    , appendStepAtIndex
    , blockToString
    , blocks
    , changeBase
    , changeBlocks
    , compositionDecoder
    , digestComposition
    , dropAllBlocksButBase
    , dropBlockAtIndex
    , dropLastBlock
    , dropLastStepAtIndex
    , duplicateBlockAndAppendAsLast
    , encodeComposition
    , fromList
    , getBlockAtIndex
    , imageBoundaries
    , length
    , stepsLength
    , toList
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListExtra exposing (pairExec)



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


blocks : Composition -> List Block
blocks (Composition _ blocks_) =
    blocks_


length : Composition -> Int
length composition =
    List.length (toList composition)


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


changeBase : Block -> Composition -> Composition
changeBase newBase (Composition _ blocks_) =
    Composition newBase blocks_


changeBlocks : List Block -> Composition -> Composition
changeBlocks newBlocks (Composition base_ _) =
    Composition base_ newBlocks


appendStepAtIndex : Step -> Int -> Composition -> Composition
appendStepAtIndex step =
    editBlockAtIndex (ListExtra.pushLast step)


dropLastStepAtIndex : Int -> Composition -> Composition
dropLastStepAtIndex =
    editBlockAtIndex ListExtra.dropLast


getBlockAtIndex : Int -> Composition -> Maybe Block
getBlockAtIndex blockIndex composition =
    List.head (List.drop blockIndex (toList composition))


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


{-| Formerly known as `deiterate`
-}
dropLastBlock : Composition -> Composition
dropLastBlock (Composition base_ blocks_) =
    Composition base_ (ListExtra.dropLast blocks_)


dropBlockAtIndex : Int -> Composition -> Composition
dropBlockAtIndex blockIndex composition =
    if length composition == 1 then
        Debug.log "dropEntireBlockAtIndex - NoOp, composition has only one block" composition

    else if blockIndex >= length composition then
        Debug.log "dropEntireBlockAtIndex - NoOp, index out of bounds" composition

    else
        fromList (ListExtra.dropIndex blockIndex (toList composition))


dropAllBlocksButBase : Composition -> Composition
dropAllBlocksButBase (Composition base_ _) =
    Composition base_ []


appendBlock : Block -> Composition -> Composition
appendBlock block (Composition base_ blocks_) =
    Composition base_ (ListExtra.pushLast block blocks_)


{-| Formerly known as `iterate`
-}
duplicateBlockAndAppendAsLast : Int -> Composition -> Composition
duplicateBlockAndAppendAsLast blockIndex composition =
    let
        maybeDuplicatedBlock =
            getBlockAtIndex blockIndex composition
    in
    case maybeDuplicatedBlock of
        Nothing ->
            composition

        Just newBlock ->
            appendBlock newBlock composition



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


type alias Boundaries =
    { topRight : ( Float, Float )
    , bottomLeft : ( Float, Float )
    }


type alias Position =
    ( Float, Float, Int )


computeDStep : ( Boundaries, Position ) -> ( Boundaries, Position )
computeDStep ( boundaries, ( x, y, angle ) ) =
    let
        --_ =
        --Debug.log "Before computeDStep" ( boundaries, ( x, y, angle ) )
        stepVector =
            fromPolar ( 1, degrees (toFloat angle) )

        newXY =
            ( x, y ) |> pairExec (+) stepVector

        newPosition =
            ( Tuple.first newXY
            , Tuple.second newXY
            , angle
            )

        newBoundaries =
            { topRight = newXY |> pairExec max boundaries.topRight
            , bottomLeft = newXY |> pairExec min boundaries.bottomLeft
            }
    in
    --Debug.log "After  computeDStep" ( newBoundaries, newPosition )
    ( newBoundaries, newPosition )


turnLeft : Int -> ( Boundaries, Position ) -> ( Boundaries, Position )
turnLeft degrees ( boundaries, ( x, y, angle ) ) =
    --Debug.log "turnLeft" ( boundaries, ( x, y, modBy 360 (angle + degrees) ) )
    ( boundaries, ( x, y, modBy 360 (angle + degrees) ) )


turnRight : Int -> ( Boundaries, Position ) -> ( Boundaries, Position )
turnRight degrees ( boundaries, ( x, y, angle ) ) =
    ( boundaries, ( x, y, modBy 360 (angle - degrees) ) )


{-| OK
-}
imageBoundaries : Float -> Composition -> Boundaries
imageBoundaries degrees composition =
    let
        updateBoundaries step boundariesAndPosition =
            --let
            --_ =
            --Debug.log "updateBoundaries" ( step, boundariesAndPosition )
            --in
            case step of
                D ->
                    computeDStep boundariesAndPosition

                L ->
                    turnLeft (round degrees) boundariesAndPosition

                R ->
                    turnRight (round degrees) boundariesAndPosition

                _ ->
                    boundariesAndPosition
    in
    composition
        |> digestComposition
        --|> Debug.log "Flat list?"
        |> List.foldl updateBoundaries
            ( Boundaries ( 0, 0 ) ( 0, 0 )
            , ( 0, 0, 0 )
            )
        --|> Debug.log "Final Boundaries"
        |> Tuple.first
