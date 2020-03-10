module ImageEssentials exposing
    ( Gallery
    , ImageAndGallery
    , ImageEssentials
    , Position
    , extractImage
    , replaceComposition
    )

{--
    , encodeImage
    , encodeImageAndGallery
    , imageAndGalleryDecoder
    , imageDecoder
    , imageToUrlString
    --}

import Colors exposing (Color)
import LSystem.Core exposing (Composition)



{--
import LSystem.Core as LCore exposing (Block, Composition, Step(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListExtra
import Url.Builder
import Url.Parser.Query as Query
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


replaceComposition : ImageEssentials -> Composition -> ImageEssentials
replaceComposition image composition =
    { image | composition = composition }
