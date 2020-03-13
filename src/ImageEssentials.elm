module ImageEssentials exposing
    ( Gallery
    , ImageAndGallery
    , ImageEssentials
    , Position
    , V2_Image
    , V2_ImageAndGallery
    , encodeImage
    , encodeImageAndGallery
    , extractImage
    , imageAndGalleryDecoder
    , imageDecoder
    , mergeV2toV3
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
import LSystem.Core as LCore exposing (Composition)
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


type alias HasImageEssentials a =
    { a
        | composition : Composition
        , turnAngle : Float
        , backgroundColor : Color
        , strokeColor : Color
        , translate : Position
        , scale : Float
    }



-- HELPERS
-- FUNCTIONS
-- TRANSFORMERS


extractImage : HasImageEssentials a -> ImageEssentials
extractImage something =
    { composition = something.composition
    , turnAngle = something.turnAngle
    , backgroundColor = something.backgroundColor
    , strokeColor = something.strokeColor
    , translate = something.translate
    , scale = something.scale
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
    Decode.map6 ImageEssentials
        (Decode.field keyFor.composition LCore.compositionDecoder)
        (Decode.field keyFor.turnAngle Decode.float)
        (Decode.field keyFor.backgroundColor Colors.decoder)
        (Decode.field keyFor.strokeColor Colors.decoder)
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
    in
    Query.map6 (ListExtra.maybeMap6 ImageEssentials)
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
-- TODO actually make this work, i.e. use these functions


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


mergeV2toV3 : V2_ImageAndGallery -> ImageAndGallery -> ImageAndGallery
mergeV2toV3 v2_imageAndGallery imageAndGallery =
    let
        v2_mainImg =
            extractImage v2_imageAndGallery

        v2_gallery =
            List.map v2_imageToImageEssentials v2_imageAndGallery.gallery
    in
    { imageAndGallery | gallery = imageAndGallery.gallery ++ v2_mainImg :: v2_gallery }


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


v2_imageToImageEssentials : V2_Image -> ImageEssentials
v2_imageToImageEssentials { composition, turnAngle, backgroundColor, strokeColor } =
    { composition = composition
    , turnAngle = turnAngle
    , backgroundColor = backgroundColor
    , strokeColor = strokeColor
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
