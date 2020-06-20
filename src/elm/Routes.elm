module Routes exposing (Page(..), Route(..), editorParser, galleryParser, mapRouteToPage, parseUrl, routeFor)

import LSystem.Image as Image exposing (PartialImage)
import Url
import Url.Parser as Parser exposing (Parser)



-- ROUTING


type Route
    = Editor PartialImage
    | Gallery
    | NotFound


routeFor =
    { gallery = "gallery"
    , editor = "editor"
    }


parseUrl : Url.Url -> Route
parseUrl url =
    let
        parsedRoute =
            Parser.parse (Parser.oneOf [ editorParser, galleryParser ]) url
    in
    case parsedRoute of
        Just route ->
            route

        Nothing ->
            NotFound


galleryParser : Parser (Route -> a) a
galleryParser =
    Parser.map Gallery (Parser.s routeFor.gallery)


editorParser : Parser (Route -> a) a
editorParser =
    Parser.map Editor (Parser.query Image.queryParser)



-- Pages


type Page
    = EditorPage
    | GalleryPage


mapRouteToPage : Route -> Page
mapRouteToPage route =
    -- TODO think about this flow that requires this function. It doesn't seem the best one.
    case route of
        Gallery ->
            GalleryPage

        Editor _ ->
            EditorPage

        NotFound ->
            EditorPage
