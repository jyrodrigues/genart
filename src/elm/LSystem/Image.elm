module LSystem.Image exposing
    ( Boundaries
    , Image
    , PartialImage
    , PathCurve(..)
    , Polygon(..)
    , SvgPathAndBoundaries
    , appendSimpleBlock
    , appendStepAtIndex
    , blockBlueprintString
    , blocksToImages
    , centralize
    , decoder
    , defaultImage
    , dropBlockAtIndex
    , dropLastBlock
    , dropLastStepAtIndex
    , duplicateBlock
    , encode
    , hash
    , imageStepsLengthString
    , imageToSvgPathString
    , length
    , move
    , polygonAngle
    , polygonBlock
    , queryParser
    , random
    , randomRectangle
    , resetBaseTo
    , resetImageComposition
    , toBlocks
    , toQuery
    , updateSvgPathAndBoundaries
    , welcomeImage
    , withBackgroundColor
    , withBlocks
    , withComposition
    , withCurve
    , withImage
    , withScale
    , withStrokeColor
    , withStrokeWidth
    , withSvgPathAndBoundaries
    , withTranslate
    , withTurnAngle
    , zoom
    )

import ABALA.Core exposing (PathSegment(..), rotateSegmentTo, segmentToString)
import ABALA.FontMajorMono as FontToSVG
import Colors exposing (Color)
import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DecodeExtra
import Json.Encode as Encode
import LSystem.Core as Core exposing (Block, Composition, Step(..))
import Murmur3
import Random
import Url.Builder
import Url.Parser.Query as Query
import Utils exposing (Position)


type alias Angle =
    Float


type alias Scale =
    Float


type alias Width =
    Float


type PathCurve
    = Line
    | Curve


type Polygon
    = Triangle
    | Square
    | Pentagon
    | Hexagon


type alias SvgPathAndBoundaries =
    ( String, String, Boundaries )


type alias Boundaries =
    { leftTop : Position
    , rightBottom : Position
    , centerOfMass : Position
    , counter : Float
    }


type alias Image =
    { composition : Composition
    , turnAngle : Angle

    -- Refactor to make impossible states impossible: composition + turnAngle + svgPathAndBoundaries
    , svgPathAndBoundaries : Maybe SvgPathAndBoundaries
    , backgroundColor : Color
    , strokeColor : Color
    , strokeWidth : Width
    , translate : Position
    , scale : Scale
    , curve : PathCurve
    }


type alias PartialImage =
    { composition : Maybe Composition
    , turnAngle : Maybe Angle
    , backgroundColor : Maybe Color
    , strokeColor : Maybe Color
    , strokeWidth : Maybe Width
    , translate : Maybe Position
    , scale : Maybe Scale
    , curve : Maybe PathCurve
    }


defaultImage : Image
defaultImage =
    { composition = Core.fromList [ polygonBlock Square, [ D ] ]
    , turnAngle = polygonAngle Square
    , svgPathAndBoundaries = Nothing
    , backgroundColor = Colors.darkGray
    , strokeColor = Colors.defaultGreen
    , strokeWidth = 1
    , translate = ( 0, 0 )
    , scale = 1
    , curve = Line
    }


welcomeImage : Image
welcomeImage =
    { composition =
        Core.fromList
            --[ [ D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D ]
            [ [ D, D, D, D, D, D, D ]
            , [ D, Core.L, Core.L, Core.S, Core.L, Core.L, D, Core.L, Core.L, Core.S, Core.L, Core.L, D, Core.L, Core.L, Core.S, Core.L, Core.L ]
            , [ Glyph 'W', Glyph 'E', Glyph 'L', Glyph 'C', Glyph 'O', Glyph 'M', Glyph 'E', R, R, Core.S, Core.S, Core.S, Core.S, Core.S, Core.S, Core.S, R, R, R, Core.S, Core.S, R, R, R, Core.S, Core.S, Glyph 'T', Glyph 'O', R, R, Core.S, Core.S, Core.S, Core.S, R, R, R, Core.S, Core.S, R, R, R, Glyph 'G', Glyph 'E', Glyph 'N', Glyph 'A', Glyph 'R', Glyph 'T', R, R, Core.S, Core.S, Core.S, Core.S, Core.S, Core.S, R, Core.S, Core.S, Core.S, Core.S, R, Core.S ]
            ]
    , turnAngle = polygonAngle Square
    , svgPathAndBoundaries = Nothing
    , backgroundColor = Colors.black
    , strokeColor = Colors.pink
    , strokeWidth = 0.01
    , translate = ( 0, 0 )
    , scale = 1.1
    , curve = Line
    }


emptyPartialImage : PartialImage
emptyPartialImage =
    { composition = Nothing
    , turnAngle = Nothing
    , backgroundColor = Nothing
    , strokeColor = Nothing
    , strokeWidth = Nothing
    , translate = Nothing
    , scale = Nothing
    , curve = Nothing
    }



-- UTILITIES


toBlocks : Image -> List Block
toBlocks =
    .composition >> Core.toList


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
                    , svgPathAndBoundaries = Nothing
                }
            )



-- ACTIONS


appendStepAtIndex : Step -> Int -> Image -> Image
appendStepAtIndex step index image =
    { image
        | composition = Core.appendStepAtIndex step index image.composition
        , svgPathAndBoundaries = Nothing
    }


appendBlock : Block -> Image -> Image
appendBlock block image =
    { image
        | composition = Core.appendBlock block image.composition
        , svgPathAndBoundaries = Nothing
    }


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
            { image
                | composition = Core.appendBlock newBlock image.composition
                , svgPathAndBoundaries = Nothing
            }

        Nothing ->
            image


dropLastStepAtIndex : Int -> Image -> Image
dropLastStepAtIndex index image =
    { image
        | composition = Core.dropLastStepAtIndex index image.composition
        , svgPathAndBoundaries = Nothing
    }


dropLastBlock : Image -> Image
dropLastBlock image =
    { image
        | composition = Core.dropLastBlock image.composition
        , svgPathAndBoundaries = Nothing
    }


dropBlockAtIndex : Int -> Image -> Image
dropBlockAtIndex index image =
    { image
        | composition = Core.dropBlockAtIndex index image.composition
        , svgPathAndBoundaries = Nothing
    }


resetBaseTo : Polygon -> Image -> Image
resetBaseTo polygon image =
    { image
        | composition = Core.changeBase (polygonBlock polygon) image.composition
        , turnAngle = polygonAngle polygon
        , svgPathAndBoundaries = Nothing
    }


resetImageComposition : Image -> Image
resetImageComposition image =
    { image
        | composition = image.composition |> Core.dropAllBlocksButBase |> Core.appendBlock [ D ]
        , svgPathAndBoundaries = Nothing
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
                |> Utils.pairExec (-) zoomFocus
    in
    { image
        | scale = scale
        , translate =
            vecMouseToImgDivCenter
                |> Utils.pairExec (+) image.translate
                |> Utils.pairMap (\value -> value * scale / image.scale)
                |> Utils.pairExec (-) vecMouseToImgDivCenter
    }


move : Position -> Position -> Image -> Image
move lastPosition newPosition image =
    image
        |> withTranslate
            (newPosition
                |> Utils.pairExec (-) lastPosition
                |> Utils.pairExec (+) image.translate
            )



-- GET INFO


length : Image -> Int
length image =
    Core.length image.composition


imageStepsLengthString : Image -> String
imageStepsLengthString image =
    let
        ( count, countOthers ) =
            Core.stepsLength image.composition
    in
    String.fromInt count ++ ", " ++ String.fromInt countOthers


blockBlueprintString : Int -> Image -> String
blockBlueprintString index image =
    case Core.getBlockAtIndex index image.composition of
        Nothing ->
            ""

        Just block ->
            Core.blockToString block



-- WITH* PATTERN


withComposition : Composition -> Image -> Image
withComposition composition image =
    { image | composition = composition }


withBlocks : List Block -> Image -> Image
withBlocks blocks image =
    { image
        | composition = Core.fromList blocks
        , svgPathAndBoundaries = Nothing
    }


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
    { image
        | turnAngle = angle
        , svgPathAndBoundaries = Nothing
    }


withSvgPathAndBoundaries : Maybe SvgPathAndBoundaries -> Image -> Image
withSvgPathAndBoundaries svgPathAndBoundaries image =
    { image | svgPathAndBoundaries = svgPathAndBoundaries }


withScale : Scale -> Image -> Image
withScale scale image =
    { image | scale = scale }


withCurve : PathCurve -> Image -> Image
withCurve curve image =
    { image
        | curve = curve
        , svgPathAndBoundaries = Nothing
    }


withImage : PartialImage -> Image -> Image
withImage partial image =
    let
        updatedSvgPathAndBoundaries =
            case ( partial.composition, partial.turnAngle ) of
                -- TODO Question: looks like this has some meaning
                -- but I can't recall. Try refactoring or adding a
                -- comment explaining.
                ( Nothing, Nothing ) ->
                    image.svgPathAndBoundaries

                _ ->
                    Nothing
    in
    { composition = Maybe.withDefault image.composition partial.composition
    , turnAngle = Maybe.withDefault image.turnAngle partial.turnAngle
    , svgPathAndBoundaries = updatedSvgPathAndBoundaries
    , backgroundColor = Maybe.withDefault image.backgroundColor partial.backgroundColor
    , strokeColor = Maybe.withDefault image.strokeColor partial.strokeColor
    , strokeWidth = Maybe.withDefault image.strokeWidth partial.strokeWidth
    , translate = Maybe.withDefault image.translate partial.translate
    , scale = Maybe.withDefault image.scale partial.scale
    , curve = Maybe.withDefault image.curve partial.curve
    }



-- RANDOM


random : Random.Generator PartialImage
random =
    Random.pair Core.randomComposition Colors.random
        |> Random.map
            (\( composition, color ) ->
                { emptyPartialImage
                    | composition = Just (Core.replaceBlankBlocks composition)
                    , strokeColor = Just color
                }
            )


randomRectangle : Random.Generator PartialImage
randomRectangle =
    randomTuple3 Core.randomRectangleComposition Colors.random (Random.float 0 360)
        |> Random.map
            (\( composition, color, turnAngle ) ->
                { emptyPartialImage
                    | composition = Just (Core.replaceBlankBlocks composition)
                    , strokeColor = Just color
                    , turnAngle = Just turnAngle
                }
            )


randomTuple3 : Random.Generator a -> Random.Generator b -> Random.Generator c -> Random.Generator ( a, b, c )
randomTuple3 genA genB genC =
    Random.pair genA (Random.pair genB genC)
        |> Random.map (\( a, ( b, c ) ) -> ( a, b, c ))



-- POLYGON


polygonBlock : Polygon -> Block
polygonBlock polygon =
    case polygon of
        Triangle ->
            [ D, Core.L, D, Core.L, D ]

        Square ->
            [ D, Core.L, D, Core.L, D, Core.L, D ]

        Pentagon ->
            [ D, Core.L, D, Core.L, D, Core.L, D, Core.L, D ]

        Hexagon ->
            [ D, Core.L, D, Core.L, D, Core.L, D, Core.L, D, Core.L, D ]


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



-- HASH


hash : Image -> Int
hash image =
    Murmur3.hashString 8871 (Encode.encode 0 (encode image))



-- ENCODER
-- DECODER


encode : Image -> Encode.Value
encode { composition, turnAngle, backgroundColor, strokeColor, strokeWidth, translate, scale, curve } =
    Encode.object
        [ ( keyFor.composition, Core.encodeComposition composition )
        , ( keyFor.turnAngle, Encode.float turnAngle )
        , ( keyFor.backgroundColor, Colors.encode backgroundColor )
        , ( keyFor.strokeColor, Colors.encode strokeColor )
        , ( keyFor.strokeWidth, Encode.float strokeWidth )
        , ( keyFor.translateX, Encode.float (Tuple.first translate) )
        , ( keyFor.translateY, Encode.float (Tuple.second translate) )
        , ( keyFor.scale, Encode.float scale )
        , ( keyFor.curve, encodeCurve curve )
        ]


decoder : Decoder Image
decoder =
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

        curve =
            Decode.field keyFor.curve curveDecoder
    in
    Decode.oneOf
        [ Decode.succeed Image
            |> DecodeExtra.andMap composition
            |> DecodeExtra.andMap turnAngle
            |> DecodeExtra.andMap (succeed Nothing)
            |> DecodeExtra.andMap backgroundColor
            |> DecodeExtra.andMap strokeColor
            |> DecodeExtra.andMap strokeWidth
            |> DecodeExtra.andMap translate
            |> DecodeExtra.andMap scale
            |> DecodeExtra.andMap curve
        , Decode.succeed Image
            |> DecodeExtra.andMap composition
            |> DecodeExtra.andMap turnAngle
            |> DecodeExtra.andMap (succeed Nothing)
            |> DecodeExtra.andMap backgroundColor
            |> DecodeExtra.andMap strokeColor
            |> DecodeExtra.andMap strokeWidth
            |> DecodeExtra.andMap translate
            |> DecodeExtra.andMap scale
            |> DecodeExtra.andMap (succeed Line)
        , Decode.succeed Image
            |> DecodeExtra.andMap composition
            |> DecodeExtra.andMap turnAngle
            |> DecodeExtra.andMap (succeed Nothing)
            |> DecodeExtra.andMap backgroundColor
            |> DecodeExtra.andMap strokeColor
            |> DecodeExtra.andMap (succeed 1)
            |> DecodeExtra.andMap translate
            |> DecodeExtra.andMap scale
            |> DecodeExtra.andMap (succeed Line)
        ]


encodeCurve : PathCurve -> Encode.Value
encodeCurve curve =
    case curve of
        Line ->
            Encode.string "line"

        Curve ->
            Encode.string "curve"


curveDecoder : Decoder PathCurve
curveDecoder =
    Decode.map
        (\str ->
            if str == "curve" then
                Curve

            else
                Line
        )
        Decode.string



-- URL
-- QUERY
-- PARSER


queryParser : Query.Parser PartialImage
queryParser =
    let
        parseQuery getKey decoder_ =
            Query.map (Maybe.andThen (Decode.decodeString decoder_ >> Result.toMaybe)) (Query.string (getKey keyFor))
    in
    Query.map8 PartialImage
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
        (parseQuery .curve curveDecoder)


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
        , query .curve (encodeCurve image.curve)
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
    , curve = "curve"
    }



-- CALCULATE svgPathAndBoundaries
-- TYPES


type alias EverythingInOnePass =
    { pathString : String
    , angle : Float
    , position : Position
    , boundaries : Boundaries
    }


updateSvgPathAndBoundaries : Image -> Image
updateSvgPathAndBoundaries image =
    case image.svgPathAndBoundaries of
        Just _ ->
            image

        Nothing ->
            withSvgPathAndBoundaries (Just (imageToSvgPathString image)) image


imageToSvgPathString : Image -> SvgPathAndBoundaries
imageToSvgPathString { composition, turnAngle, curve } =
    let
        finalEverything =
            Core.processComposition composition
                |> List.foldl (processCompositionStep curve turnAngle) baseEverything

        ( lastX, lastY ) =
            finalEverything.position
    in
    -- Main path
    -- TODO add initial Q value when curve==Curve
    ( "M 0 0 " ++ finalEverything.pathString
      -- Next step path
      -- TODO move this into a function on Svg module
    , "M "
        ++ String.fromFloat lastX
        ++ " "
        ++ String.fromFloat lastY
        ++ " "
        ++ segmentToString (ABALA.Core.L (nextPositionDelta finalEverything.angle 10))
      -- TopRight and BottomLeft extremes of final image
    , finalEverything.boundaries
    )


baseEverything : EverythingInOnePass
baseEverything =
    EverythingInOnePass
        ""
        0
        ( 0, 0 )
        (Boundaries ( 0, 0 ) ( 0, 0 ) ( 0, 0 ) 0)


{-|

    For performance reasons we have to iterate over the digested composition
    as few times as possible, ideally just one. That's why we aren't breaking
    up this function (as it once was: one step to get a list of PathSegments
    and another to create a string from those)


    Note about `pathString ++ segmentToString ...`:
    There could be a space in between but we should follow W3C SVG Recommendation:

       "Superfluous white space and separators such as commas can be eliminated
        (e.g., "M 100 100 L 200 200" contains unnecessary spaces and could be
        expressed more compactly as "M100 100L200 200")."

    From: https://web.archive.org/web/20200331042341/www.w3.org/TR/SVG/paths.html
    (yes, a link from archive.org)

-}
processCompositionStep : PathCurve -> Float -> Step -> EverythingInOnePass -> EverythingInOnePass
processCompositionStep pathCurve turnAngleSize step currentEverything =
    let
        { pathString, angle, position, boundaries } =
            currentEverything

        -- TODO This `10` value here is scaling the drawing. It's probably related to the viewbox size. Extract it.
        nextPositionDelta_ =
            nextPositionDelta angle 10

        nextPosition =
            Utils.pairExec (+) nextPositionDelta_ position

        nextStrokeCenterOfMass =
            Utils.pairExec (+) (Utils.pairMap (\v -> v / 2) nextPositionDelta_) position

        updatedBoundaries =
            { leftTop = Utils.pairExec min boundaries.leftTop nextPosition
            , rightBottom = Utils.pairExec max boundaries.rightBottom nextPosition
            , centerOfMass = Utils.pairExec (+) boundaries.centerOfMass nextStrokeCenterOfMass
            , counter = boundaries.counter + 1
            }

        curve =
            case pathCurve of
                Line ->
                    ABALA.Core.L

                Curve ->
                    ABALA.Core.T

        turnWith compoundAngle =
            EverythingInOnePass
                pathString
                (modBy360 compoundAngle)
                position
                boundaries
    in
    case step of
        Core.D ->
            EverythingInOnePass
                (pathString ++ segmentToString (curve nextPositionDelta_))
                angle
                nextPosition
                updatedBoundaries

        Core.S ->
            EverythingInOnePass
                (pathString ++ segmentToString (M nextPositionDelta_))
                angle
                nextPosition
                boundaries

        Core.Glyph char ->
            Maybe.withDefault currentEverything (Maybe.map (drawWithGlyph currentEverything) (Dict.get char FontToSVG.dict))

        Core.L ->
            turnWith (angle + turnAngleSize)

        Core.R ->
            turnWith (angle - turnAngleSize)


drawWithGlyph : EverythingInOnePass -> ( List PathSegment, Float ) -> EverythingInOnePass
drawWithGlyph currentEverything ( pathSegments, glyphAdvance ) =
    let
        { pathString, angle, position, boundaries } =
            currentEverything

        -- TODO this is the only line that changes from the above `let ... in` to this function
        -- So we can improve the ergonomics here
        nextPositionDelta_ =
            nextPositionDelta angle glyphAdvance

        nextPosition =
            Utils.pairExec (+) nextPositionDelta_ position

        nextStrokeCenterOfMass =
            Utils.pairExec (+) (Utils.pairMap (\v -> v / 2) nextPositionDelta_) position

        updatedBoundaries =
            { leftTop = Utils.pairExec min boundaries.leftTop nextPosition
            , rightBottom = Utils.pairExec max boundaries.rightBottom nextPosition
            , centerOfMass = Utils.pairExec (+) boundaries.centerOfMass nextStrokeCenterOfMass
            , counter = boundaries.counter + 1
            }

        path =
            List.map (rotateSegmentTo nextPositionDelta_) pathSegments
                |> List.map segmentToString
                |> String.join ""
    in
    EverythingInOnePass
        (pathString ++ path)
        angle
        nextPosition
        updatedBoundaries


nextPositionDelta : Float -> Float -> Position
nextPositionDelta angle advance =
    fromPolar ( 1, degrees angle )
        |> Utils.pairMap ((*) advance)
        |> adjustForViewportAxisOrientation


adjustForViewportAxisOrientation : Position -> Position
adjustForViewportAxisOrientation ( x, y ) =
    ( x, -y )



-- HELPER


modBy360 : Float -> Float
modBy360 angle =
    if angle > 360 then
        angle - 360

    else if angle < -360 then
        angle + 360

    else
        angle
