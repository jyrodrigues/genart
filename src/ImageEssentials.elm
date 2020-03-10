module ImageEssentials exposing
    ( Gallery
    , ImageAndGallery
    , ImageEssentials
    , Position
    , encodeImage
    , encodeImageAndGallery
    , extractImage
    , imageAndGalleryDecoder
    , imageDecoder
    , imageToUrlString
    , queryToImageParser
    , replaceComposition
    , replaceImage
    )

{--
    , encodeImage
    , encodeImageAndGallery
    , imageAndGalleryDecoder
    , imageDecoder
    , imageToUrlString
    --}

import Colors exposing (Color)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core as LCore exposing (Composition)
import ListExtra
import Url.Builder
import Url.Parser.Query as Query



{--
import LSystem.Core as LCore exposing (Block, Composition, Step(..))
--}
-- IMAGE ESSENTIALS


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


keyFor =
    { composition = "composition"
    , turnAngle = "turnAngle"
    , backgroundColor = "backgroundColor"
    , strokeColor = "strokeColor"
    , translateX = "translateX"
    , translateY = "translateY"
    , scale = "scale"
    }


encodeImageAndGallery : ImageEssentials -> List ImageEssentials -> Encode.Value
encodeImageAndGallery image gallery =
    Encode.object
        [ ( "image", encodeImage image ) --(extractImage model) )
        , ( "gallery", Encode.list encodeImage gallery )
        ]


imageAndGalleryDecoder : Decoder ImageAndGallery
imageAndGalleryDecoder =
    Decode.map2 ImageAndGallery
        (Decode.field "image" imageDecoder)
        (Decode.field "gallery" (Decode.list imageDecoder))


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


imageToUrlString : ImageEssentials -> String
imageToUrlString image =
    Url.Builder.absolute []
        [ Url.Builder.string keyFor.composition (image.composition |> LCore.encodeComposition |> Encode.encode 0)
        , Url.Builder.string keyFor.turnAngle (String.fromFloat image.turnAngle)
        , Url.Builder.string keyFor.backgroundColor (image.backgroundColor |> Colors.encode |> Encode.encode 0)
        , Url.Builder.string keyFor.strokeColor (image.strokeColor |> Colors.encode |> Encode.encode 0)
        , Url.Builder.string keyFor.translateX (Tuple.first image.translate |> Encode.float |> Encode.encode 0)
        , Url.Builder.string keyFor.translateY (Tuple.second image.translate |> Encode.float |> Encode.encode 0)
        , Url.Builder.string keyFor.scale (image.scale |> Encode.float |> Encode.encode 0)
        ]
