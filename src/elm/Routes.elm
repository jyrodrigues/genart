module Routes exposing
    ( Page(..)
    , Route(..)
    , editorParser
    , galleryParser
    , mapRouteToPage
    , parseUrl
    )

import Config exposing (routeFor)
import LSystem.Image exposing (PartialImage)
import Pages.Editor as Editor
import Url
import Url.Parser as Parser exposing (Parser)



-- ROUTING


type Route
    = Editor PartialImage
    | Gallery
    | Writing
    | Welcome -- Match with top "/"
    | NotFound -- TODO Show random image with a 404 overlay and a button to go to any page (gallery, editor, welcome?)


parseUrl : Url.Url -> Route
parseUrl url =
    let
        parsedRoute =
            Parser.parse
                (Parser.oneOf
                    [ welcomeParser
                    , writingParser
                    , editorParser
                    , galleryParser
                    ]
                )
                url
    in
    case parsedRoute of
        Just route ->
            route

        Nothing ->
            NotFound


editorParser : Parser (Route -> a) a
editorParser =
    Parser.map Editor Editor.urlParser


galleryParser : Parser (Route -> a) a
galleryParser =
    Parser.map Gallery (Parser.s routeFor.gallery)


writingParser : Parser (Route -> a) a
writingParser =
    Parser.map Writing (Parser.s routeFor.writing)


welcomeParser : Parser (Route -> a) a
welcomeParser =
    Parser.map Welcome Parser.top



-- Pages


type Page
    = EditorPage
    | GalleryPage
    | WelcomePage
    | WritingPage


mapRouteToPage : Route -> Page
mapRouteToPage route =
    -- TODO think about this flow that requires this function. It doesn't seem the best one.
    case route of
        Gallery ->
            GalleryPage

        Editor _ ->
            EditorPage

        Welcome ->
            WelcomePage

        Writing ->
            WritingPage

        NotFound ->
            EditorPage
