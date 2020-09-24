module Routes exposing
    ( Page(..)
    , Route(..)
    , editorEncodeIntoUrl
    , editorParser
    , galleryParser
    , mapRouteToPage
    , parseUrl
    )

import Config exposing (routeFor)
import LSystem.Image exposing (PartialImage)
import Pages.Editor as Editor
import Url
import Url.Parser as Parser exposing ((<?>), Parser)



-- ROUTING


type Route
    = Editor PartialImage
    | Gallery
      -- | Welcome -- Match with top "/"
    | NotFound -- Show random image with a 404 overlay and a button to go to any page (gallery, editor, welcome?)


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
    Parser.map Editor (Parser.s routeFor.editor <?> Editor.queryParser)


editorEncodeIntoUrl : Editor.Model -> String
editorEncodeIntoUrl editor =
    routeFor.editor ++ Editor.queryEncode editor



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
