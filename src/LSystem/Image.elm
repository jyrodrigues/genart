module LSystem.Image exposing
    ( Image
    , PartialImage
    , Polygon(..)
    , Position
    , appendSimpleBlock
    , appendStepAtIndex
    , blockBlueprintString
    , blocksToImages
    , centralize
    , defaultImage
    , dropBlockAtIndex
    , dropLastBlock
    , dropLastStepAtIndex
    , duplicateBlock
    , encodeImage
    , imageDecoder
    , imageStepsLenthString
    , length
    , move
    , polygonAngle
    , polygonBlock
    , queryParser
    , resetBaseTo
    , resetImageComposition
    , toQuery
    , withBackgroundColor
    , withImage
    , withScale
    , withStrokeColor
    , withStrokeWidth
    , withTranslate
    , withTurnAngle
    , zoom
    )

import Colors exposing (Color)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core as Core exposing (Block, Composition, Step(..))
import ListExtra
import Url.Builder
import Url.Parser.Query as Query


type alias Position =
    ( Float, Float )


type alias Angle =
    Float


type alias Scale =
    Float


type alias Width =
    Float


type Polygon
    = Triangle
    | Square
    | Pentagon
    | Hexagon


type alias Image =
    { composition : Composition
    , turnAngle : Angle
    , backgroundColor : Color
    , strokeColor : Color
    , strokeWidth : Width
    , translate : Position
    , scale : Scale
    }


type alias PartialImage =
    { composition : Maybe Composition
    , turnAngle : Maybe Angle
    , backgroundColor : Maybe Color
    , strokeColor : Maybe Color
    , strokeWidth : Maybe Width
    , translate : Maybe Position
    , scale : Maybe Scale
    }


defaultImage : Image
defaultImage =
    { composition = Core.fromList [ polygonBlock Square, [ D ] ]
    , turnAngle = polygonAngle Square
    , backgroundColor = Colors.darkGray
    , strokeColor = Colors.defaultGreen
    , strokeWidth = 1
    , translate = ( 0, 0 )
    , scale = 1
    }



-- UTILITIES


blocksToImages : Image -> List Image
blocksToImages image =
    image.composition
        |> Core.toList
        |> List.map
            (\block ->
                { image
                    | composition = Core.fromList [ block ]
                    , scale = 1
                    , translate = ( 0, 0 )
                    , strokeWidth = 1
                }
            )



-- ACTIONS


appendStepAtIndex : Step -> Int -> Image -> Image
appendStepAtIndex step index image =
    { image | composition = Core.appendStepAtIndex step index image.composition }


appendBlock : Block -> Image -> Image
appendBlock block image =
    { image | composition = Core.appendBlock block image.composition }


appendSimpleBlock : Image -> Image
appendSimpleBlock image =
    appendBlock [ D ] image


duplicateBlock : Int -> Image -> Image
duplicateBlock index image =
    let
        maybeDuplicatedBlock =
            Core.getBlockAtIndex index image.composition
    in
    case maybeDuplicatedBlock of
        Just newBlock ->
            { image | composition = Core.appendBlock newBlock image.composition }

        Nothing ->
            image


dropLastStepAtIndex : Int -> Image -> Image
dropLastStepAtIndex index image =
    { image | composition = Core.dropLastStepAtIndex index image.composition }


dropLastBlock : Image -> Image
dropLastBlock image =
    { image | composition = Core.dropLastBlock image.composition }


dropBlockAtIndex : Int -> Image -> Image
dropBlockAtIndex index image =
    { image | composition = Core.dropBlockAtIndex index image.composition }


resetBaseTo : Polygon -> Image -> Image
resetBaseTo polygon image =
    { image
        | composition = Core.changeBase (polygonBlock polygon) image.composition
        , turnAngle = polygonAngle polygon
    }


resetImageComposition : Image -> Image
resetImageComposition image =
    { image
        | composition = image.composition |> Core.dropAllBlocksButBase |> Core.appendBlock [ D ]
    }



-- MOVEMENT


centralize : Image -> Image
centralize image =
    { image
        | scale = 1
        , translate = ( 0, 0 )
    }


zoom : Float -> Position -> Position -> Image -> Image
zoom scaleDelta zoomFocus imageDivCenter image =
    let
        scale =
            max (image.scale - 0.01 * scaleDelta) 0.1

        vecMouseToImgDivCenter =
            imageDivCenter
                |> ListExtra.pairExec (-) zoomFocus
    in
    { image
        | scale = scale
        , translate =
            vecMouseToImgDivCenter
                |> ListExtra.pairExec (+) image.translate
                |> ListExtra.pairMap (\value -> value * scale / image.scale)
                |> ListExtra.pairExec (-) vecMouseToImgDivCenter
    }


move : Position -> Position -> Image -> Image
move lastPosition newPosition image =
    image
        |> withTranslate
            (newPosition
                |> ListExtra.pairExec (-) lastPosition
                |> ListExtra.pairExec (+) image.translate
            )



-- GET INFO


length : Image -> Int
length image =
    Core.length image.composition


imageStepsLenthString : Image -> String
imageStepsLenthString image =
    let
        ( countD, countOthers ) =
            Core.stepsLength image.composition
    in
    String.fromInt countD ++ ", " ++ String.fromInt countOthers


blockBlueprintString : Int -> Image -> String
blockBlueprintString index image =
    case Core.getBlockAtIndex index image.composition of
        Nothing ->
            ""

        Just block ->
            Core.blockToString block



-- WITH* PATTERN


withBackgroundColor : Color -> Image -> Image
withBackgroundColor color image =
    { image | backgroundColor = color }


withStrokeColor : Color -> Image -> Image
withStrokeColor color image =
    { image | strokeColor = color }


withStrokeWidth : Width -> Image -> Image
withStrokeWidth width image =
    { image | strokeWidth = width }


withTranslate : Position -> Image -> Image
withTranslate position image =
    { image | translate = position }


withTurnAngle : Angle -> Image -> Image
withTurnAngle angle image =
    { image | turnAngle = angle }


withScale : Scale -> Image -> Image
withScale scale image =
    { image | scale = scale }


withImage : PartialImage -> Image -> Image
withImage partial image =
    { composition = Maybe.withDefault image.composition partial.composition
    , turnAngle = Maybe.withDefault image.turnAngle partial.turnAngle
    , backgroundColor = Maybe.withDefault image.backgroundColor partial.backgroundColor
    , strokeColor = Maybe.withDefault image.strokeColor partial.strokeColor
    , strokeWidth = Maybe.withDefault image.strokeWidth partial.strokeWidth
    , translate = Maybe.withDefault image.translate partial.translate
    , scale = Maybe.withDefault image.scale partial.scale
    }



-- POLYGON


polygonBlock : Polygon -> Block
polygonBlock polygon =
    case polygon of
        Triangle ->
            [ D, L, D, L, D ]

        Square ->
            [ D, L, D, L, D, L, D ]

        Pentagon ->
            [ D, L, D, L, D, L, D, L, D ]

        Hexagon ->
            [ D, L, D, L, D, L, D, L, D, L, D ]


polygonAngle : Polygon -> Float
polygonAngle polygon =
    case polygon of
        Triangle ->
            120

        Square ->
            90

        Pentagon ->
            72

        Hexagon ->
            60



-- ENCODER
-- DECODER


encodeImage : Image -> Encode.Value
encodeImage { composition, turnAngle, backgroundColor, strokeColor, strokeWidth, translate, scale } =
    Encode.object
        [ ( keyFor.composition, Core.encodeComposition composition )
        , ( keyFor.turnAngle, Encode.float turnAngle )
        , ( keyFor.backgroundColor, Colors.encode backgroundColor )
        , ( keyFor.strokeColor, Colors.encode strokeColor )
        , ( keyFor.strokeWidth, Encode.float strokeWidth )
        , ( keyFor.translateX, Encode.float (Tuple.first translate) )
        , ( keyFor.translateY, Encode.float (Tuple.second translate) )
        , ( keyFor.scale, Encode.float scale )
        ]


imageDecoder : Decoder Image
imageDecoder =
    let
        composition =
            Decode.field keyFor.composition Core.compositionDecoder

        turnAngle =
            Decode.field keyFor.turnAngle Decode.float

        backgroundColor =
            Decode.field keyFor.backgroundColor Colors.decoder

        strokeColor =
            Decode.field keyFor.strokeColor Colors.decoder

        strokeWidth =
            Decode.field keyFor.strokeWidth Decode.float

        translate =
            Decode.map2 Tuple.pair
                (Decode.field keyFor.translateX Decode.float)
                (Decode.field keyFor.translateY Decode.float)

        scale =
            Decode.field keyFor.scale Decode.float

        succeed =
            Decode.succeed
    in
    Decode.oneOf
        [ Decode.map7 Image composition turnAngle backgroundColor strokeColor strokeWidth translate scale
        , Decode.map7 Image composition turnAngle backgroundColor strokeColor (succeed 1) translate scale
        ]



-- URL
-- QUERY
-- PARSER


queryParser : Query.Parser PartialImage
queryParser =
    let
        parseQuery getKey decoder =
            Query.map (Maybe.andThen (Decode.decodeString decoder >> Result.toMaybe)) (Query.string (getKey keyFor))
    in
    Query.map7 PartialImage
        (parseQuery .composition Core.compositionDecoder)
        (parseQuery .turnAngle Decode.float)
        (parseQuery .backgroundColor Colors.decoder)
        (parseQuery .strokeColor Colors.decoder)
        (parseQuery .strokeWidth Decode.float)
        (Query.map2 (Maybe.map2 Tuple.pair)
            (parseQuery .translateX Decode.float)
            (parseQuery .translateY Decode.float)
        )
        (parseQuery .scale Decode.float)


toQuery : Image -> String
toQuery image =
    let
        query getKey value =
            Url.Builder.string (getKey keyFor) (Encode.encode 0 value)
    in
    Url.Builder.toQuery
        [ query .composition (Core.encodeComposition image.composition)
        , query .turnAngle (Encode.float image.turnAngle)
        , query .backgroundColor (Colors.encode image.backgroundColor)
        , query .strokeColor (Colors.encode image.strokeColor)
        , query .strokeWidth (Encode.float image.strokeWidth)
        , query .translateX (Encode.float (Tuple.first image.translate))
        , query .translateY (Encode.float (Tuple.second image.translate))
        , query .scale (Encode.float image.scale)
        ]


keyFor =
    { composition = "composition"
    , turnAngle = "turnAngle"
    , backgroundColor = "backgroundColor"
    , strokeColor = "strokeColor"
    , strokeWidth = "strokeWidth"
    , translateX = "translateX"
    , translateY = "translateY"
    , scale = "scale"
    }
