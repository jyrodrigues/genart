module Routes exposing (Page(..), Route(..), editorParser, galleryParser, mapRouteToPage, parseUrl, replaceUrl, routeFor)

import Browser.Navigation as Nav
import LSystem.Image as Image exposing (Image, PartialImage)
import Url
import Url.Parser as Parser exposing (Parser)



-- ROUTING


type Page
    = EditorPage
    | GalleryPage


type Route
    = Editor PartialImage
    | Gallery
    | NotFound


mapRouteToPage : Route -> Page
mapRouteToPage route =
    -- TODO think about this flow that requires this function. It doesn't seem the best one.
    case route of
        Gallery ->
            GalleryPage

        _ ->
            EditorPage


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


{-| This function used on `galleryParser` makes it asymmetric with `editorParser`.
--- Not sure if this is a good idea.
-}
routeFor : Page -> String
routeFor page =
    case page of
        GalleryPage ->
            "gallery"

        EditorPage ->
            "/"


galleryParser : Parser (Route -> a) a
galleryParser =
    Parser.map Gallery (Parser.s (routeFor GalleryPage))


editorParser : Parser (Route -> a) a
editorParser =
    Parser.map Editor (Parser.query Image.queryParser)


{-| Current version of Url building and parsing
-}
replaceUrl : Nav.Key -> Image -> Cmd msg
replaceUrl key image =
    {--
            Note about Nav.replaceUrl: Browsers may rate-limit this function by throwing an
            exception. The discussion here suggests that the limit is 100 calls per 30 second
            interval in Safari in 2016. It also suggests techniques for people changing the
            URL based on scroll position.

            https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl
    --}
    Nav.replaceUrl key (routeFor EditorPage ++ Image.toQuery image)
