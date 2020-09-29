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
    , encodeComposition
    , fromList
    , getBlockAtIndex
    , length
    , randomComposition
    , replaceBlankBlocks
    , stepsLength
    , toList
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random
import Utils



-- TYPES


type Step
    = D
    | R
    | L
    | S
    | Glyph Char


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
        countStep step ( steps, otherSteps ) =
            case step of
                D ->
                    ( steps + 1, otherSteps )

                Glyph _ ->
                    ( steps + 1, otherSteps )

                _ ->
                    ( steps, otherSteps + 1 )

        countSteps block =
            List.foldl countStep ( 0, 0 ) block

        accumulateLength ( blockCount, blockCountOthers ) ( accCount, accCountOthers ) =
            ( blockCount * accCount, accCountOthers + accCount * blockCountOthers )
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
    editBlockAtIndex (Utils.pushLast step)


dropLastStepAtIndex : Int -> Composition -> Composition
dropLastStepAtIndex =
    editBlockAtIndex Utils.dropLast


getBlockAtIndex : Int -> Composition -> Maybe Block
getBlockAtIndex blockIndex composition =
    List.head (List.drop blockIndex (toList composition))


editBlockAtIndex : (Block -> Block) -> Int -> Composition -> Composition
editBlockAtIndex editFn blockIndex composition =
    let
        editAtIndex currentBlockIndex block =
            if currentBlockIndex == blockIndex then
                editFn block

            else
                block
    in
    fromList (List.indexedMap editAtIndex (toList composition))


dropLastBlock : Composition -> Composition
dropLastBlock (Composition base_ blocks_) =
    Composition base_ (Utils.dropLast blocks_)


dropBlockAtIndex : Int -> Composition -> Composition
dropBlockAtIndex blockIndex composition =
    if length composition == 1 then
        -- Never allow for empty composition, instead replace with a simple block.
        Composition [ D ] []

    else if blockIndex >= length composition then
        -- NoOp, index out of bounds
        composition

    else
        fromList (Utils.dropIndex blockIndex (toList composition))


dropAllBlocksButBase : Composition -> Composition
dropAllBlocksButBase (Composition base_ _) =
    Composition base_ []


appendBlock : Block -> Composition -> Composition
appendBlock block (Composition base_ blocks_) =
    Composition base_ (Utils.pushLast block blocks_)


replaceBlankBlocks : Composition -> Composition
replaceBlankBlocks composition =
    composition
        |> toList
        |> List.map
            (\block ->
                if Tuple.first (stepsLength (Composition block [])) == 0 then
                    D :: block

                else
                    block
            )
        |> fromList



-- COMPOSITION CONVERSIONS


toList : Composition -> List Block
toList (Composition base_ blocks_) =
    base_ :: blocks_


{-| N.B. An empty list does _not_ yield an empty Composition
-}
fromList : List Block -> Composition
fromList blocks_ =
    case blocks_ of
        head :: tail ->
            Composition head tail

        [] ->
            Composition [ D ] []



-- BLOCK CONVERSIONS


blockToString : Block -> String
blockToString =
    List.map stepToString
        >> String.concat



-- STEP CONVERSIONS
-- TODO Since I'm adding letters as an option inside the drawing, neither Steps nor dna can be encoded as simple chars.
-- Change them into numbers or think another way of encoding them.


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
            dnaOrGlyph char


dnaOrGlyph : Char -> Step
dnaOrGlyph char =
    case char of
        'c' ->
            D

        't' ->
            R

        'g' ->
            L

        'a' ->
            S

        _ ->
            -- TODO bug: 'L' or 'S' or ... will never get to here. Should we return a Maybe instead?
            -- This doesn't seem right.
            Glyph char


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

        Glyph char ->
            "_" ++ String.fromChar char



-- RANDOM


randomComposition : Random.Generator Composition
randomComposition =
    Random.int 1 50
        |> Random.andThen (\lenth_ -> Random.list lenth_ randomStep)
        |> Random.list 4
        |> Random.map fromList


randomStep : Random.Generator Step
randomStep =
    Random.weighted ( 20, D ) [ ( 15, L ), ( 5, R ), ( 1, S ) ]



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
    Encode.string (String.concat (List.map stepToString block))


blockDecoder : Decoder Block
blockDecoder =
    Decode.map stringToBlock Decode.string


stringToBlock : String -> Block
stringToBlock =
    String.foldl
        (\char ( blockSoFar, nextIsGlyph ) ->
            if nextIsGlyph then
                ( blockSoFar ++ [ Glyph char ], False )

            else if char == '_' then
                ( blockSoFar, True )

            else
                ( blockSoFar ++ [ charToStep char ], False )
        )
        ( [], False )
        >> Tuple.first
