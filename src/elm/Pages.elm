module Pages exposing (Page(..), routeFor)


type Page
    = EditorPage
    | GalleryPage
    | WelcomePage
    | WritingPage


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
