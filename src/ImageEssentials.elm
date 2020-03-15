module ImageEssentials exposing
    ( Gallery
    , ImageAndGallery
    , ImageEssentials
    , Polygon(..)
    , Position
    , V2_Image
    , V2_ImageAndGallery
    , defaultImage
    , encodeImage
    , encodeImageAndGallery
    , extractImage
    , imageAndGalleryDecoder
    , imageDecoder
    , mergeAllVersions_ImageAndGalleryDecoder
    , mergeToV3
    , polygonAngle
    , polygonBlock
    , queryToImageParser
    , replaceComposition
    , replaceImage
    , toUrlPathString
    , urlParser
    , v2_encodeImage
    , v2_encodeImageAndGallery
    , v2_imageAndGalleryDecoder
    , v2_imageDecoder
    , v2_imageToImageEssentials
    )

import Colors exposing (Color)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core as LCore exposing (Block, Composition, Step(..))
import ListExtra
import Url
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser)
import Url.Parser.Query as Query



-- TYPES


type alias ImageEssentials =
    { composition : Composition
    , turnAngle : Float
    , backgroundColor : Color
    , strokeColor : Color
    , strokeWidth : Float
    , translate : Position
    , scale : Float
    }


type alias Gallery =
    List ImageEssentials


type alias ImageAndGallery =
    { image : ImageEssentials
    , gallery : Gallery
    }


type alias Position =
    ( Float, Float )


type Polygon
    = Triangle
    | Square
    | Pentagon
    | Hexagon


type alias HasImageEssentials a =
    { a
        | composition : Composition
        , turnAngle : Float
        , backgroundColor : Color
        , strokeColor : Color
        , translate : Position
        , scale : Float
    }



-- DEFAULT VALUES


defaultImage : ImageEssentials
defaultImage =
    { composition = LCore.fromList [ polygonBlock Square, [ D ] ]
    , turnAngle = polygonAngle Square
    , backgroundColor = Colors.darkGray
    , strokeColor = Colors.defaultGreen
    , strokeWidth = 1
    , translate = ( 0, 0 )
    , scale = 1
    }



-- HELPERS
-- FUNCTIONS
-- TRANSFORMERS


extractImage : HasImageEssentials a -> ImageEssentials
extractImage { composition, turnAngle, backgroundColor, strokeColor, translate, scale } =
    { composition = composition
    , turnAngle = turnAngle
    , backgroundColor = backgroundColor
    , strokeColor = strokeColor
    , strokeWidth = 1
    , translate = translate
    , scale = scale
    }


replaceImage : ImageEssentials -> HasImageEssentials a -> HasImageEssentials a
replaceImage { composition, turnAngle, backgroundColor, strokeColor } something =
    { something
        | composition = composition
        , turnAngle = turnAngle
        , backgroundColor = backgroundColor
        , strokeColor = strokeColor
    }


replaceComposition : ImageEssentials -> Composition -> ImageEssentials
replaceComposition image composition =
    { image | composition = composition }



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
-- URL
-- QUERY
-- PARSER


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


encodeImage : ImageEssentials -> Encode.Value
encodeImage { composition, turnAngle, backgroundColor, strokeColor, translate, scale } =
    Encode.object
        [ ( keyFor.composition, LCore.encodeComposition composition )
        , ( keyFor.turnAngle, Encode.float turnAngle )
        , ( keyFor.backgroundColor, Colors.encode backgroundColor )
        , ( keyFor.strokeColor, Colors.encode strokeColor )
        , ( keyFor.translateX, Encode.float (Tuple.first translate) )
        , ( keyFor.translateY, Encode.float (Tuple.second translate) )
        , ( keyFor.scale, Encode.float scale )
        ]


imageDecoder : Decoder ImageEssentials
imageDecoder =
    Decode.map7 ImageEssentials
        (Decode.field keyFor.composition LCore.compositionDecoder)
        (Decode.field keyFor.turnAngle Decode.float)
        (Decode.field keyFor.backgroundColor Colors.decoder)
        (Decode.field keyFor.strokeColor Colors.decoder)
        (Decode.succeed 1)
        (Decode.map2 Tuple.pair
            (Decode.field keyFor.translateX Decode.float)
            (Decode.field keyFor.translateY Decode.float)
        )
        (Decode.field keyFor.scale Decode.float)


urlParser : Parser (Maybe ImageEssentials -> a) a
urlParser =
    Parser.map
        mergeUrlV1andV2
        (Parser.query queryToImageParser </> fragmentToCompositionParser)


queryToImageParser : Query.Parser (Maybe ImageEssentials)
queryToImageParser =
    let
        queryMapFromDecoder decoder =
            Query.map (Maybe.andThen (Result.toMaybe << Decode.decodeString decoder))

        imageEssentials a b c d e f =
            -- TODO remove this, only here while strokeWidth doesn't make into the URL
            ImageEssentials a b c d 1 e f
    in
    Query.map6 (ListExtra.maybeMap6 imageEssentials)
        (Query.string keyFor.composition |> queryMapFromDecoder LCore.compositionDecoder)
        (Query.string keyFor.turnAngle |> Query.map (Maybe.andThen String.toFloat))
        (Query.string keyFor.backgroundColor |> queryMapFromDecoder Colors.decoder)
        (Query.string keyFor.strokeColor |> queryMapFromDecoder Colors.decoder)
        (Query.map2 (Maybe.map2 Tuple.pair)
            (Query.string keyFor.translateX |> Query.map (Maybe.andThen String.toFloat))
            (Query.string keyFor.translateY |> Query.map (Maybe.andThen String.toFloat))
        )
        (Query.string keyFor.scale |> Query.map (Maybe.andThen String.toFloat))


toUrlPathString : ImageEssentials -> String
toUrlPathString image =
    Url.Builder.absolute []
        [ Url.Builder.string keyFor.composition (image.composition |> LCore.encodeComposition |> Encode.encode 0)
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
                    List.map v2_imageToImageEssentials v2_imageAndGallery.gallery
            in
            { imageAndGallery | gallery = imageAndGallery.gallery ++ v2_mainImg :: v2_gallery }

        ( Just imageAndGallery, Nothing ) ->
            imageAndGallery

        ( Nothing, Just v2_imageAndGallery ) ->
            { image = extractImage v2_imageAndGallery, gallery = List.map v2_imageToImageEssentials v2_imageAndGallery.gallery }

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


mergeUrlV1andV2 : Maybe ImageEssentials -> Maybe Composition -> Maybe ImageEssentials
mergeUrlV1andV2 v2_dataOnQueryParams v1_dataOnHash =
    case ( v2_dataOnQueryParams, v1_dataOnHash ) of
        -- If there is an image on query parameters, we ignore the hash.
        ( Just imageEssentials, _ ) ->
            Just imageEssentials

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
                >> Maybe.andThen (Decode.decodeString LCore.compositionDecoder >> Result.toMaybe)
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
        (Decode.field "state" LCore.compositionDecoder)
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
        (Decode.field "composition" LCore.compositionDecoder)
        (Decode.field "turnAngle" Decode.float)
        (Decode.field "bgColor" Colors.decoder)
        (Decode.field "strokeColor" Colors.decoder)


{-| TODO make this dependable on defaultImage
-}
v2_imageToImageEssentials : V2_Image -> ImageEssentials
v2_imageToImageEssentials { composition, turnAngle, backgroundColor, strokeColor } =
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
        [ ( "state", LCore.encodeComposition composition )
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
        [ ( "composition", LCore.encodeComposition composition )
        , ( "turnAngle", Encode.float turnAngle )
        , ( "bgColor", Colors.encode backgroundColor )
        , ( "strokeColor", Colors.encode strokeColor )
        ]



-- LOCAL STORAGE - VERSION 1
-- TODO Search git for "genart/v0.1/state" and create migrations
