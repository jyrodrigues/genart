module Routes exposing
    ( Route(..)
    , editorParser
    , galleryParser
    , mapRouteToPage
    , parseUrl
    )

import LSystem.Image exposing (PartialImage)
import Pages exposing (Page(..), routeFor)
import Pages.Editor as Editor
import Url
import Url.Parser as Parser exposing (Parser)



-- ROUTING


type Route
    = Editor PartialImage
    | Gallery
    | Writing
    | Dev
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
                    , devParser
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
    simpleParser Gallery GalleryPage


writingParser : Parser (Route -> a) a
writingParser =
    simpleParser Writing WritingPage


devParser : Parser (Route -> a) a
devParser =
    simpleParser Dev DevPage


simpleParser : Route -> Page -> Parser (Route -> a) a
simpleParser route page =
    Parser.map route (Parser.s (routeFor page))


welcomeParser : Parser (Route -> a) a
welcomeParser =
    Parser.map Welcome Parser.top


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

        Dev ->
            DevPage

        NotFound ->
            EditorPage
