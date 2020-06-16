module LSystem.Image exposing
    ( Boundaries
    , Image
    , PartialImage
    , PathCurve(..)
    ,  PathSegment(..)
       -- Remove (extract from this module)

    ,  PathSegmentString
       -- Remove (extract from this module)

    , Polygon(..)
    , Position
    , SvgPathAndBoundaries
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
    , imageToSvgPathString
    , length
    , move
    , polygonAngle
    , polygonBlock
    , queryParser
    , random
    , resetBaseTo
    , resetImageComposition
    ,  segmentToString
       -- Remove (extract from this module)

    ,  toAbsoluteValue
       -- Remove (extract from this module)

    , toQuery
    , updateSvgPathAndBoundaries
    , withBackgroundColor
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

import Colors exposing (Color)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DecodeExtra
import Json.Encode as Encode
import LSystem.Core as Core exposing (Block, Composition, Step(..))
import ListExtra
import Random
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


withComposition : Composition -> Image -> Image
withComposition composition image =
    { image | composition = composition }


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



-- ENCODER
-- DECODER


encodeImage : Image -> Encode.Value
encodeImage { composition, turnAngle, backgroundColor, strokeColor, strokeWidth, translate, scale, curve } =
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
        parseQuery getKey decoder =
            Query.map (Maybe.andThen (Decode.decodeString decoder >> Result.toMaybe)) (Query.string (getKey keyFor))
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


{-| <https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths>

    Important:

    Using uppercase letters only because Elm forces us when creating types,
    *BUT* every case represents lowercase counterparts from the
    SVG specification!


    Also check:
    - v1: https://www.w3.org/TR/SVG11/paths.html#PathElement
    - v2: https://svgwg.org/svg2-draft/paths.html#PathElement
    - Working draft: https://svgwg.org/specs/paths/#PathElement

-}
type
    PathSegment
    -- Move to delta
    -- m dx dy
    = M Position
      -- Line to delta
      -- l dx dy
    | L Position
      -- Horizontal line to delta
      -- h dx
    | H Float
      -- Vertical line to delta
      -- v dy
    | V Float
      -- Close path ("from A to Z")
      -- z
    | Z
      -- Cubic bezier curve: `C controlPoint1 controlPoint2 destination`
      -- c dx1 dy1, dx2 dy2, dx dy
    | C Position Position Position
      -- Cubic bezier curve reflecting last curve's last control point
      -- s dx2 dy2, dx dy
    | S Position Position
      -- Quadratic bezier curve: `Q controlPoint destination`
      -- q dx1 dy1, dx dy
    | Q Position Position
      -- Quadratic bezier curve reflecting last curve's last control point
      -- t dx dy
    | T Position
      -- Arc curve
      -- a rx ry x-axis-rotation large-arc-flag sweep-flag dx dy
    | A Position Float Bool Bool Position


type alias PathSegmentString =
    String


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
            Core.digestComposition composition
                |> List.foldl (processCompositionStep curve turnAngle) baseEverything

        ( lastX, lastY ) =
            finalEverything.position
    in
    -- Main path
    -- TODO add initial Q value when curve==Curve
    ( "M 0 0 " ++ finalEverything.pathString
      -- Next step path
    , "M "
        ++ String.fromFloat lastX
        ++ " "
        ++ String.fromFloat lastY
        ++ " "
        ++ segmentToString (L (nextPositionDelta finalEverything.angle))
      -- TopRight and BottomLeft extremes of final image
    , finalEverything.boundaries
    )


baseEverything : EverythingInOnePass
baseEverything =
    EverythingInOnePass
        ""
        0
        ( 0, 0 )
        (Boundaries ( 0, 0 ) ( 0, 0 ))


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
processCompositionStep pathCurve turnAngleSize step { pathString, angle, position, boundaries } =
    let
        nextPositionDelta_ =
            nextPositionDelta angle

        nextPosition =
            ListExtra.pairExec (+) nextPositionDelta_ position

        updatedBoundaries =
            { leftTop = ListExtra.pairExec min boundaries.leftTop nextPosition
            , rightBottom = ListExtra.pairExec max boundaries.rightBottom nextPosition
            }

        curve =
            case pathCurve of
                Line ->
                    L

                Curve ->
                    T
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
                updatedBoundaries

        Core.L ->
            EverythingInOnePass
                pathString
                (modBy360 (angle + turnAngleSize))
                position
                boundaries

        Core.R ->
            EverythingInOnePass
                pathString
                (modBy360 (angle - turnAngleSize))
                position
                boundaries


{-|

    Note about `"m" ++ String.fromFloat dx ...`:
    There could be a space in between but we should follow W3C SVG Recommendation:

       "Superfluous white space and separators such as commas can be eliminated
        (e.g., "M 100 100 L 200 200" contains unnecessary spaces and could be
        expressed more compactly as "M100 100L200 200")."

    From: https://web.archive.org/web/20200331042341/www.w3.org/TR/SVG/paths.html
    (yes, a link from archive.org)

-}
segmentToString : PathSegment -> PathSegmentString
segmentToString segment =
    case segment of
        M ( dx, dy ) ->
            "m" ++ String.fromFloat dx ++ " " ++ String.fromFloat dy

        L ( dx, dy ) ->
            "l" ++ String.fromFloat dx ++ " " ++ String.fromFloat dy

        H dx ->
            "h" ++ String.fromFloat dx

        V dy ->
            "v" ++ String.fromFloat dy

        Z ->
            "z"

        C ( dx1, dy1 ) ( dx2, dy2 ) ( dx, dy ) ->
            "c"
                ++ (String.fromFloat dx1 ++ " " ++ String.fromFloat dy1)
                ++ (" " ++ String.fromFloat dx2 ++ " " ++ String.fromFloat dy2)
                ++ (" " ++ String.fromFloat dx ++ " " ++ String.fromFloat dy)

        S ( dx2, dy2 ) ( dx, dy ) ->
            "s"
                ++ (String.fromFloat dx2 ++ " " ++ String.fromFloat dy2)
                ++ (" " ++ String.fromFloat dx ++ " " ++ String.fromFloat dy)

        Q ( dx1, dy1 ) ( dx, dy ) ->
            "q"
                ++ (String.fromFloat dx1 ++ " " ++ String.fromFloat dy1)
                ++ (" " ++ String.fromFloat dx ++ " " ++ String.fromFloat dy)

        T ( dx, dy ) ->
            "t" ++ String.fromFloat dx ++ " " ++ String.fromFloat dy

        A ( rx, ry ) xAxisRotation largeArcFlag sweepFlag ( x, y ) ->
            "a"
                ++ (String.fromFloat rx ++ " " ++ String.fromFloat ry)
                ++ (" " ++ String.fromFloat (degrees xAxisRotation))
                ++ (" "
                        ++ (if largeArcFlag then
                                "1"

                            else
                                "0"
                           )
                   )
                ++ (" "
                        ++ (if sweepFlag then
                                "1"

                            else
                                "0"
                           )
                   )
                ++ (" " ++ String.fromFloat x ++ " " ++ String.fromFloat y)


toAbsoluteValue : PathSegmentString -> PathSegmentString
toAbsoluteValue =
    String.toUpper


toRelativeValue : PathSegmentString -> PathSegmentString
toRelativeValue =
    String.toLower


{-| TODO This `10` value here is scaling the drawing. It's probably related to the viewbox size. Extract it.
-}
nextPositionDelta : Float -> Position
nextPositionDelta angle =
    fromPolar ( 1, degrees angle )
        |> ListExtra.pairMap ((*) 10)
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
