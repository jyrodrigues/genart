module Pages exposing (Page(..), routeFor, toString)


type Page
    = EditorPage
    | GalleryPage
    | WelcomePage
    | WritingPage
    | DevPage


routeFor : Page -> String
routeFor page =
    case page of
        EditorPage ->
            "editor"

        GalleryPage ->
            "gallery"

        WelcomePage ->
            ""

        WritingPage ->
            "writing"

        DevPage ->
            "dev"


toString : Page -> String
toString page =
    case page of
        EditorPage ->
            "Editor"

        GalleryPage ->
            "Gallery"

        WelcomePage ->
            "Welcome"

        WritingPage ->
            "Writing"

        DevPage ->
            "Development playground"
