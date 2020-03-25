module LSystem.Image exposing
    ( Gallery
    , Image
    , ImageAndGallery
    , Polygon(..)
    , Position
    , V2_Image
    , V2_ImageAndGallery
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
    , encodeImageAndGallery
    , extractImage
    , imageAndGalleryDecoder
    , imageDecoder
    , imageStepsLenthString
    , length
    , mergeAllVersions_ImageAndGalleryDecoder
    , mergeToV3
    , move
    , polygonAngle
    , polygonBlock
    , queryToImageParser
    , replaceComposition
    , resetBaseTo
    , resetImageComposition
    , toUrlPathString
    , urlParser
    , v2_encodeImage
    , v2_encodeImageAndGallery
    , v2_imageAndGalleryDecoder
    , v2_imageDecoder
    , v2_imageToImage
    , withBackgroundColor
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
import Url
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser)
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
encodeImage { composition, turnAngle, backgroundColor, strokeColor, translate, scale } =
    Encode.object
        [ ( keyFor.composition, Core.encodeComposition composition )
        , ( keyFor.turnAngle, Encode.float turnAngle )
        , ( keyFor.backgroundColor, Colors.encode backgroundColor )
        , ( keyFor.strokeColor, Colors.encode strokeColor )
        , ( keyFor.translateX, Encode.float (Tuple.first translate) )
        , ( keyFor.translateY, Encode.float (Tuple.second translate) )
        , ( keyFor.scale, Encode.float scale )
        ]


imageDecoder : Decoder Image
imageDecoder =
    Decode.map7 Image
        (Decode.field keyFor.composition Core.compositionDecoder)
        (Decode.field keyFor.turnAngle Decode.float)
        (Decode.field keyFor.backgroundColor Colors.decoder)
        (Decode.field keyFor.strokeColor Colors.decoder)
        (Decode.succeed 1)
        (Decode.map2 Tuple.pair
            (Decode.field keyFor.translateX Decode.float)
            (Decode.field keyFor.translateY Decode.float)
        )
        (Decode.field keyFor.scale Decode.float)



-- URL
-- QUERY
-- PARSER


urlParser : Parser (Maybe Image -> a) a
urlParser =
    Parser.map
        mergeUrlV1andV2
        (Parser.query queryToImageParser </> fragmentToCompositionParser)


queryToImageParser : Query.Parser (Maybe Image)
queryToImageParser =
    let
        queryMapFromDecoder decoder =
            Query.map (Maybe.andThen (Result.toMaybe << Decode.decodeString decoder))

        image a b c d e f =
            -- TODO remove this, only here while strokeWidth doesn't make into the URL
            Image a b c d 1 e f
    in
    Query.map6 (ListExtra.maybeMap6 image)
        (Query.string keyFor.composition |> queryMapFromDecoder Core.compositionDecoder)
        (Query.string keyFor.turnAngle |> Query.map (Maybe.andThen String.toFloat))
        (Query.string keyFor.backgroundColor |> queryMapFromDecoder Colors.decoder)
        (Query.string keyFor.strokeColor |> queryMapFromDecoder Colors.decoder)
        (Query.map2 (Maybe.map2 Tuple.pair)
            (Query.string keyFor.translateX |> Query.map (Maybe.andThen String.toFloat))
            (Query.string keyFor.translateY |> Query.map (Maybe.andThen String.toFloat))
        )
        (Query.string keyFor.scale |> Query.map (Maybe.andThen String.toFloat))


toUrlPathString : Image -> String
toUrlPathString image =
    Url.Builder.absolute []
        [ Url.Builder.string keyFor.composition (image.composition |> Core.encodeComposition |> Encode.encode 0)
        , Url.Builder.string keyFor.turnAngle (String.fromFloat image.turnAngle)
        , Url.Builder.string keyFor.backgroundColor (image.backgroundColor |> Colors.encode |> Encode.encode 0)
        , Url.Builder.string keyFor.strokeColor (image.strokeColor |> Colors.encode |> Encode.encode 0)
        , Url.Builder.string keyFor.translateX (Tuple.first image.translate |> Encode.float |> Encode.encode 0)
        , Url.Builder.string keyFor.translateY (Tuple.second image.translate |> Encode.float |> Encode.encode 0)
        , Url.Builder.string keyFor.scale (image.scale |> Encode.float |> Encode.encode 0)
        ]


keyFor =
    { composition = "composition"
    , turnAngle = "turnAngle"
    , backgroundColor = "backgroundColor"
    , strokeColor = "strokeColor"
    , translateX = "translateX"
    , translateY = "translateY"
    , scale = "scale"
    }



-- DECODE LOCAL STORAGE (ALL VERSIONS) (FLAGS)


mergeAllVersions_ImageAndGalleryDecoder : Decoder ImageAndGallery
mergeAllVersions_ImageAndGalleryDecoder =
    Decode.map2 mergeToV3
        (Decode.field "latest" (Decode.maybe imageAndGalleryDecoder))
        (Decode.field "v2" (Decode.maybe v2_imageAndGalleryDecoder))



{--How we could do it with more than 2 versions:
mergeAllVersions : V1_Image -> V2_ImageAndGallery -> ImageAndGallery -> ImageAndGallery
mergeAllVersions v1 v2 latest =
    v1
        |> mergeToV2 v2
        |> mergeToV3 latest
--}


mergeToV3 : Maybe ImageAndGallery -> Maybe V2_ImageAndGallery -> ImageAndGallery
mergeToV3 maybeImageAndGallery maybeV2 =
    case ( maybeImageAndGallery, maybeV2 ) of
        ( Just imageAndGallery, Just v2_imageAndGallery ) ->
            let
                v2_mainImg =
                    extractImage v2_imageAndGallery

                v2_gallery =
                    List.map v2_imageToImage v2_imageAndGallery.gallery
            in
            { imageAndGallery | gallery = imageAndGallery.gallery ++ v2_mainImg :: v2_gallery }

        ( Just imageAndGallery, Nothing ) ->
            imageAndGallery

        ( Nothing, Just v2_imageAndGallery ) ->
            { image = extractImage v2_imageAndGallery, gallery = List.map v2_imageToImage v2_imageAndGallery.gallery }

        ( Nothing, Nothing ) ->
            { image = defaultImage, gallery = [] }



-- BACKWARD COMPATIBILITY
-- 1. MIGRATE FROM OLD VERSIONS
-- 2. MERGE WITH NEW VERSION
-- 3. DELETE OLD
--
--
--
--
-- URL - VERSION 1


mergeUrlV1andV2 : Maybe Image -> Maybe Composition -> Maybe Image
mergeUrlV1andV2 v2_dataOnQueryParams v1_dataOnHash =
    case ( v2_dataOnQueryParams, v1_dataOnHash ) of
        -- If there is an image on query parameters, we ignore the hash.
        ( Just image, _ ) ->
            Just image

        ( Nothing, Just composition ) ->
            Just
                -- TODO replace with `initialImage`/`baseImage`
                { composition = composition
                , turnAngle = 90
                , backgroundColor = Colors.darkGray
                , strokeColor = Colors.defaultGreen
                , strokeWidth = 1
                , translate = ( 0, 0 )
                , scale = 1
                }

        ( Nothing, Nothing ) ->
            Nothing


fragmentToCompositionParser : Parser (Maybe Composition -> a) a
fragmentToCompositionParser =
    Parser.fragment <|
        Maybe.andThen
            (Url.percentDecode
                >> Maybe.andThen (Decode.decodeString Core.compositionDecoder >> Result.toMaybe)
            )



-- MIGRATE LOCAL STORAGE
-- LOCAL STORAGE - VERSION 2


type alias V2_ImageAndGallery =
    { composition : Composition
    , gallery : List V2_Image
    , backgroundColor : Color
    , strokeColor : Color
    , turnAngle : Float
    , scale : Float
    , translate : Position
    }


type alias V2_Image =
    { composition : Composition
    , turnAngle : Float
    , backgroundColor : Color
    , strokeColor : Color
    }


v2_imageAndGalleryDecoder : Decoder V2_ImageAndGallery
v2_imageAndGalleryDecoder =
    Decode.map7 V2_ImageAndGallery
        (Decode.field "state" Core.compositionDecoder)
        (Decode.field "gallery" (Decode.list v2_imageDecoder))
        (Decode.field "bgColor" Colors.decoder)
        (Decode.field "strokeColor" Colors.decoder)
        (Decode.field "turnAngle" Decode.float)
        (Decode.field "scale" Decode.float)
        (Decode.map2 Tuple.pair
            (Decode.field "translateX" Decode.float)
            (Decode.field "translateY" Decode.float)
        )


v2_imageDecoder : Decoder V2_Image
v2_imageDecoder =
    Decode.map4 V2_Image
        (Decode.field "composition" Core.compositionDecoder)
        (Decode.field "turnAngle" Decode.float)
        (Decode.field "bgColor" Colors.decoder)
        (Decode.field "strokeColor" Colors.decoder)


{-| TODO make this dependable on defaultImage
-}
v2_imageToImage : V2_Image -> Image
v2_imageToImage { composition, turnAngle, backgroundColor, strokeColor } =
    { composition = composition
    , turnAngle = turnAngle
    , backgroundColor = backgroundColor
    , strokeColor = strokeColor
    , strokeWidth = 1
    , translate = ( 0, 0 )
    , scale = 1
    }


v2_encodeImageAndGallery : V2_ImageAndGallery -> Encode.Value
v2_encodeImageAndGallery { composition, gallery, backgroundColor, strokeColor, turnAngle, scale, translate } =
    Encode.object
        [ ( "state", Core.encodeComposition composition )
        , ( "gallery", Encode.list v2_encodeImage gallery )
        , ( "bgColor", Colors.encode backgroundColor )
        , ( "strokeColor", Colors.encode strokeColor )
        , ( "turnAngle", Encode.float turnAngle )
        , ( "scale", Encode.float scale )
        , ( "translateX", Encode.float (Tuple.first translate) )
        , ( "translateY", Encode.float (Tuple.second translate) )
        ]


v2_encodeImage : V2_Image -> Encode.Value
v2_encodeImage { composition, turnAngle, backgroundColor, strokeColor } =
    Encode.object
        [ ( "composition", Core.encodeComposition composition )
        , ( "turnAngle", Encode.float turnAngle )
        , ( "bgColor", Colors.encode backgroundColor )
        , ( "strokeColor", Colors.encode strokeColor )
        ]



-- LOCAL STORAGE - VERSION 1
-- TODO Search git for "genart/v0.1/state" and create migrations
{--



















-- OLD STUFF





--}
-- IMAGE AND GALLERY
-- Should it be here or on Main.elm?


type alias Gallery =
    List Image


type alias ImageAndGallery =
    { image : Image
    , gallery : Gallery
    }


type alias HasImage a =
    { a
        | composition : Composition
        , turnAngle : Float
        , backgroundColor : Color
        , strokeColor : Color
        , translate : Position
        , scale : Float
    }


encodeImageAndGallery : ImageAndGallery -> Encode.Value
encodeImageAndGallery { image, gallery } =
    Encode.object
        [ ( "image", encodeImage image )
        , ( "gallery", Encode.list encodeImage gallery )
        ]


imageAndGalleryDecoder : Decoder ImageAndGallery
imageAndGalleryDecoder =
    Decode.map2 ImageAndGallery
        (Decode.field "image" imageDecoder)
        (Decode.field "gallery" (Decode.list imageDecoder))



-- Should we delete this function? This would imply adding a lot of other functions
-- to deal with composition transformation/replacement


replaceComposition : Image -> Composition -> Image
replaceComposition image composition =
    { image | composition = composition }



-- DELETE THIS FUNCTIONS


extractImage : HasImage a -> Image
extractImage { composition, turnAngle, backgroundColor, strokeColor, translate, scale } =
    { composition = composition
    , turnAngle = turnAngle
    , backgroundColor = backgroundColor
    , strokeColor = strokeColor
    , strokeWidth = 1
    , translate = translate
    , scale = scale
    }
