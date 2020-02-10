module LSystem.Draw exposing
    ( Image
    , Polygon(..)
    , appendStepAtIndex
    , decoder
    , drawImage
    , encode
    , getBackgroundColor
    , getComposition
    , getScale
    , getStrokeColor
    , getTranslate
    , getTurnAngle
    , initialImage
    , mapComposition
    , mapTranslate
    , parser
    , parserWithDefault
    , splitIntoBlockImages
    , toUrlString
    , updateDefiningPolygon
    , withBackgroundColor
    , withComposition
    , withId
    , withOnClick
    , withScale
    , withStrokeColor
    , withTranslate
    , withTurnAngle
    )

import Colors exposing (Color)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core as LCore exposing (Block, Composition, Step(..), digestComposition, imageBoundaries)
import ListExtra exposing (floatsToSpacedString, pairExec, pairMap)
import Svg.Styled exposing (Svg, circle, line, polyline, svg)
import Svg.Styled.Attributes exposing (cx, cy, fill, id, points, r, stroke, strokeDasharray, style, viewBox, x1, x2, y1, y2)
import Svg.Styled.Events exposing (onClick)
import Url
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser)
import Url.Parser.Query as Query



-- IMAGE (MAIN MODULE TYPE)


type alias ImageEssentials =
    { composition : Composition
    , turnAngle : Float
    , backgroundColor : Color
    , strokeColor : Color
    , translate : Position
    , scale : Float
    }


type Image msg
    = Image ImageEssentials (Maybe Id) (Maybe msg)



-- TYPES


type alias Position =
    { x : Float
    , y : Float
    }


type alias Drawing =
    { path : String
    , pos : Position
    , deg : Float
    }


type alias Angle =
    Float


type alias Scale =
    Float


type alias Id =
    String



-- URL
-- VERSIONS


type alias UrlDataVersions =
    { v1_dataOnQueryParams : Maybe ImageEssentials
    , v0_dataOnHash : Maybe Composition
    }



-- INIT


initialImage : Image msg
initialImage =
    Image initialImageEssentials Nothing Nothing


initialImageEssentials : ImageEssentials
initialImageEssentials =
    { composition = LCore.fromList [ [ D ] ]
    , turnAngle = 90
    , backgroundColor = Colors.darkGray
    , strokeColor = Colors.defaultGreen
    , translate = { x = 0, y = 0 }
    , scale = 1
    }



-- GET PATTERN


getBackgroundColor : Image msg -> Color
getBackgroundColor (Image { backgroundColor } _ _) =
    backgroundColor


getStrokeColor : Image msg -> Color
getStrokeColor (Image { strokeColor } _ _) =
    strokeColor


getTurnAngle : Image msg -> Float
getTurnAngle (Image { turnAngle } _ _) =
    turnAngle


getComposition : Image msg -> Composition
getComposition (Image { composition } _ _) =
    composition


getTranslate : Image msg -> Position
getTranslate (Image { translate } _ _) =
    translate


getScale : Image msg -> Scale
getScale (Image { scale } _ _) =
    scale



-- WITH* PATTERN


withComposition : Composition -> Image msg -> Image msg
withComposition composition (Image image id msg) =
    Image { image | composition = composition } id msg


withTurnAngle : Angle -> Image msg -> Image msg
withTurnAngle angle (Image image id msg) =
    Image { image | turnAngle = angle } id msg


withStrokeColor : Color -> Image msg -> Image msg
withStrokeColor color (Image image id msg) =
    Image { image | strokeColor = color } id msg


withBackgroundColor : Color -> Image msg -> Image msg
withBackgroundColor color (Image image id msg) =
    Image { image | backgroundColor = color } id msg


withScale : Scale -> Image msg -> Image msg
withScale scale (Image image id msg) =
    Image { image | scale = scale } id msg


withTranslate : Position -> Image msg -> Image msg
withTranslate translate (Image image id msg) =
    Image { image | translate = translate } id msg


withId : Id -> Image msg -> Image msg
withId id (Image image _ msg) =
    Image image (Just id) msg


withOnClick : msg -> Image msg -> Image msg
withOnClick msg (Image image id _) =
    Image image id (Just msg)


withImageEssentials : ImageEssentials -> Image msg -> Image msg
withImageEssentials imageEssentials (Image _ id msg) =
    Image imageEssentials id msg



-- MAP


mapComposition : (Composition -> Composition) -> Image msg -> Image msg
mapComposition fn ((Image { composition } _ _) as image) =
    withComposition (fn composition) image


mapTranslate : (Position -> Position) -> Image msg -> Image msg
mapTranslate fn ((Image { translate } _ _) as image) =
    withTranslate (fn translate) image



-- CONVERSION


splitIntoBlockImages : Image msg -> List (Image msg)
splitIntoBlockImages ((Image { composition } _ _) as image) =
    LCore.toList composition
        |> List.map
            (\block ->
                withComposition (LCore.fromList [ block ]) image
            )


appendStepAtIndex : Step -> Int -> Image msg -> Image msg
appendStepAtIndex step editingIndex ((Image { composition } _ _) as image) =
    image
        |> withComposition (LCore.appendStepAtIndex step editingIndex composition)



-- DRAWING


{-| About vecTranslateToImgCenter:

The drawing's math coordinate system is UPxRIGHT while
SVG viewbox coordinate system is DOWNxRIGHT.

So the vector to translate viewboxe's (0,0) into image's
center for x-axis is the same as the middle point of the image
but is inverted for y-axis.

-}
drawImage : Image msg -> Svg msg
drawImage (Image { composition, turnAngle, strokeColor, backgroundColor, scale, translate } maybeId maybeMsg) =
    let
        { topRight, bottomLeft } =
            imageBoundaries turnAngle composition

        vecTranslateOriginToDrawingCenter =
            topRight
                -- Sum boundaries and get the mean for both axis
                |> pairExec (+) bottomLeft
                |> pairMap (\value -> value / 2)
                -- Scale both by a factor of 10 (this value is arbitrary, probably should live in a variable)
                |> pairMap ((*) 10)
                -- Check the comment above to understand why we invert only the y-axis
                |> pairExec (*) ( 1, -1 )

        scaledSize =
            topRight
                -- Get width and height
                |> pairExec (-) bottomLeft
                -- Make sure both are at least `2`
                |> pairMap (max 2)
                -- Scale both by a factor of 10 (this value is arbitrary, probably should live in a variable)
                |> pairMap ((*) 10)
                -- Scale with 50% of margin (somehow this is a magic number, I think we should be able to change this
                -- value without moving the image from the center
                |> pairMap ((*) 1.6)

        vecTranslateOriginToViewportCenter =
            -- Move origin by half the viewport size in the oposite direction, centralizing the drawing.
            scaledSize |> pairMap ((*) (-1 / 2))

        vecTranslate =
            vecTranslateOriginToDrawingCenter |> pairExec (+) vecTranslateOriginToViewportCenter

        drawing =
            transformToSvgPath (digestComposition composition) 0 0 turnAngle

        maybeIdAttr =
            case maybeId of
                Just id_ ->
                    [ id id_ ]

                Nothing ->
                    []

        maybeOnClick =
            case maybeMsg of
                Just msg ->
                    [ onClick msg ]

                Nothing ->
                    []
    in
    svg
        ([ viewBox <|
            floatsToSpacedString
                [ Tuple.first vecTranslate
                , Tuple.second vecTranslate
                , Tuple.first scaledSize
                , Tuple.second scaledSize
                ]
         , style <|
            "display: block; "
                ++ "height: 100%; "
                ++ "width: 100%; "
                ++ "background-color: "
                ++ Colors.toString backgroundColor
                ++ "; "
                ++ "transform: "
                -- As stated here (https://stackoverflow.com/a/10765771),
                -- multiple transform functions are applied from right
                -- to left. So `scale` should come last (to be applied first)
                -- to prevent scaled translation (moving 2px with mouse and
                -- seeing a 200px move in the image).
                ++ ("translate("
                        ++ String.fromFloat translate.x
                        ++ "px, "
                        ++ String.fromFloat translate.y
                        ++ "px)"
                   )
                ++ ("scale(" ++ String.fromFloat scale ++ ")")
         ]
            ++ maybeIdAttr
            ++ maybeOnClick
        )
        [ originPoint 0 0
        , nextLine drawing
        , polyline
            [ points <| .path <| drawing
            , stroke (Colors.toString strokeColor)
            , fill "none"
            ]
            []
        ]



-- ENCODE AND DECODER


keyFor :
    { composition : String
    , turnAngle : String
    , backgroundColor : String
    , strokeColor : String
    , translateX : String
    , translateY : String
    , scale : String
    }
keyFor =
    { composition = "composition"
    , turnAngle = "turnAngle"
    , backgroundColor = "backgroundColor"
    , strokeColor = "strokeColor"
    , translateX = "translateX"
    , translateY = "translateY"
    , scale = "scale"
    }


decoder : Decoder (Image msg)
decoder =
    Decode.map (\imageEssentials -> Image imageEssentials Nothing Nothing) <|
        Decode.map6 ImageEssentials
            (Decode.field keyFor.composition LCore.compositionDecoder)
            (Decode.field keyFor.turnAngle Decode.float)
            (Decode.field keyFor.backgroundColor Colors.decoder)
            (Decode.field keyFor.strokeColor Colors.decoder)
            (Decode.map2 Position
                (Decode.field keyFor.translateX Decode.float)
                (Decode.field keyFor.translateY Decode.float)
            )
            (Decode.field keyFor.scale Decode.float)


encode : Image msg -> Encode.Value
encode (Image { composition, turnAngle, backgroundColor, strokeColor, translate, scale } _ _) =
    Encode.object
        [ ( keyFor.composition, LCore.encodeComposition composition )
        , ( keyFor.turnAngle, Encode.float turnAngle )
        , ( keyFor.backgroundColor, Colors.encode backgroundColor )
        , ( keyFor.strokeColor, Colors.encode strokeColor )
        , ( keyFor.translateX, Encode.float translate.x )
        , ( keyFor.translateY, Encode.float translate.y )
        , ( keyFor.scale, Encode.float scale )
        ]



-- URL PARSING


parserWithDefault : Parser (Image msg -> a) a
parserWithDefault =
    Parser.map (Maybe.withDefault initialImage) parser


parser : Parser (Maybe (Image msg) -> a) a
parser =
    Parser.map urlDataVersionsToImage <|
        Parser.map
            UrlDataVersions
            -- Fragment is parsed for backward-compatibility only
            (Parser.query queryParser </> fragmentToCompositionParser)


{-| Current version of Url building and parsing
-}
queryParser : Query.Parser (Maybe ImageEssentials)
queryParser =
    let
        queryMapFromDecoder decoder_ =
            Query.map (Maybe.andThen (Result.toMaybe << Decode.decodeString decoder_))
    in
    Query.map6 (ListExtra.maybeMap6 ImageEssentials)
        (Query.string keyFor.composition |> queryMapFromDecoder LCore.compositionDecoder)
        (Query.string keyFor.turnAngle |> Query.map (Maybe.andThen String.toFloat))
        (Query.string keyFor.backgroundColor |> queryMapFromDecoder Colors.decoder)
        (Query.string keyFor.strokeColor |> queryMapFromDecoder Colors.decoder)
        (Query.map2 (Maybe.map2 Position)
            (Query.string keyFor.translateX |> Query.map (Maybe.andThen String.toFloat))
            (Query.string keyFor.translateY |> Query.map (Maybe.andThen String.toFloat))
        )
        (Query.string keyFor.scale |> Query.map (Maybe.andThen String.toFloat))


{-| Backwards compatibility: Old version of Url building and parsing.
-}
fragmentToCompositionParser : Parser (Maybe Composition -> a) a
fragmentToCompositionParser =
    Parser.fragment <|
        Maybe.andThen
            (Url.percentDecode
                >> Maybe.andThen (Decode.decodeString LCore.compositionDecoder >> Result.toMaybe)
            )


toUrlString : Image msg -> String
toUrlString (Image image _ _) =
    Url.Builder.absolute []
        [ Url.Builder.string keyFor.composition (image.composition |> LCore.encodeComposition |> Encode.encode 0)
        , Url.Builder.string keyFor.turnAngle (String.fromFloat image.turnAngle)
        , Url.Builder.string keyFor.backgroundColor (image.backgroundColor |> Colors.encode |> Encode.encode 0)
        , Url.Builder.string keyFor.strokeColor (image.strokeColor |> Colors.encode |> Encode.encode 0)
        , Url.Builder.string keyFor.translateX (image.translate.x |> Encode.float |> Encode.encode 0)
        , Url.Builder.string keyFor.translateY (image.translate.y |> Encode.float |> Encode.encode 0)
        , Url.Builder.string keyFor.scale (image.scale |> Encode.float |> Encode.encode 0)
        ]


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


urlDataVersionsToImage : UrlDataVersions -> Maybe (Image msg)
urlDataVersionsToImage dataVersions =
    if isJust dataVersions.v1_dataOnQueryParams then
        Maybe.map (\imageEssentials -> withImageEssentials imageEssentials initialImage) dataVersions.v1_dataOnQueryParams

    else if isJust dataVersions.v0_dataOnHash then
        -- Migrating from old URL format
        Maybe.map (\composition -> withComposition composition initialImage) dataVersions.v0_dataOnHash

    else
        Nothing



-- POLYGON


type Polygon
    = Triangle
    | Square
    | Pentagon
    | Hexagon


updateDefiningPolygon : Polygon -> Image msg -> Image msg
updateDefiningPolygon polygon ((Image { composition } _ _) as image) =
    image
        |> withComposition (LCore.changeBase (polygonBlock polygon) composition)
        |> withTurnAngle (polygonAngle polygon)


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



-- TODO Refactor this tail of functions


originPoint : Float -> Float -> Svg msg
originPoint x y =
    circle
        [ cx <| String.fromFloat x
        , cy <| String.fromFloat y
        , r "1"
        , fill "white"
        ]
        []


nextLine : Drawing -> Svg msg
nextLine drawing =
    let
        { pos, deg } =
            drawing

        newPos =
            movePoint pos (degrees deg)
    in
    line
        [ x1 <| String.fromFloat pos.x
        , y1 <| String.fromFloat pos.y
        , x2 <| String.fromFloat newPos.x
        , y2 <| String.fromFloat newPos.y
        , stroke "rgba(255, 0, 0, 0.5)"
        , strokeDasharray "1"
        ]
        []


positionToString : Position -> String
positionToString pos =
    String.fromFloat pos.x ++ " " ++ String.fromFloat pos.y


transformToSvgPath : Block -> Float -> Float -> Float -> Drawing
transformToSvgPath transform x0 y0 turn =
    let
        initialPos =
            Position x0 y0
    in
    List.foldl
        (addStepToDrawing turn)
        (Drawing (positionToString initialPos) initialPos 0)
        transform


movePoint : Position -> Float -> Position
movePoint pos rad =
    Position (pos.x + cos rad * 10) (pos.y + sin rad * 10)


addStepToDrawing : Float -> Step -> Drawing -> Drawing
addStepToDrawing turn step drawing =
    let
        newPos =
            movePoint drawing.pos (degrees drawing.deg)

        newPath =
            drawing.path ++ ", " ++ positionToString newPos
    in
    case step of
        L ->
            { drawing | deg = drawing.deg - turn }

        R ->
            { drawing | deg = drawing.deg + turn }

        D ->
            Drawing newPath newPos drawing.deg

        _ ->
            drawing
