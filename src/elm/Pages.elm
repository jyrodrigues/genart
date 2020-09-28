module Pages exposing (Page(..), routeFor, toString)


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
