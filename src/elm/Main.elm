-- Anything other than `main` is exposed for testing only
-- TODO research how we could change this in order to only expose `main`
-- Question: Should Main module be tested? Should every test-deserving function live
--           outside Main module?


port module Main exposing (decoder, encode, main)

import Browser
import Browser.Navigation as Nav
import Html
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core exposing (Step(..))
import Pages.Editor as Editor exposing (ExternalMsg(..))
import Pages.Gallery as Gallery
import Routes exposing (Page(..), Route(..), mapRouteToPage, parseUrl)
import Url



-- PORTS


port saveModelToLocalStorage : Encode.Value -> Cmd msg



-- MODEL


type alias Model =
    -- Pages
    { editor : Editor.Model
    , gallery : Gallery.Model
    , viewingPage : Page

    -- Url
    , url : Url.Url
    , navKey : Nav.Key
    }



-- MSG


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | EditorMsg Editor.Msg
    | GalleryMsg Gallery.Msg



-- INI MODEL
{--TODO
initialModel : Image -> List Image -> Url.Url -> Nav.Key -> Model
initialModel image gallery url navKey =
--}


initialModel : Url.Url -> Nav.Key -> Model
initialModel url navKey =
    -- Pages
    { editor = Editor.initialModel
    , gallery = []

    -- Current viewing page
    , viewingPage = EditorPage

    -- Url
    , url = url
    , navKey = navKey
    }



-- MAIN


main : Program Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- INIT


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init locallyStoredModel url navKey =
    let
        ( initialEditorFromStorage, initialGallery ) =
            case Decode.decodeValue decoder locallyStoredModel of
                Ok ( storedEditor, storedGallery ) ->
                    -- Change name to `withStorage` or change `storedEditor` to `storedPartialImage` ?
                    ( storedEditor, storedGallery )

                Err _ ->
                    ( Editor.initialModel, Gallery.initialModel )

        route =
            parseUrl url

        viewingPage =
            mapRouteToPage route

        initialEditor =
            case route of
                -- TODO Make this name consistent with the above (check above comment about `storedEditor`)
                Editor partialImage ->
                    Editor.withPartialImage partialImage initialEditorFromStorage

                _ ->
                    initialEditorFromStorage

        updateUrlForEditor =
            case viewingPage of
                EditorPage ->
                    {--
                        Note about Nav.replaceUrl: Browsers may rate-limit this function by throwing an
                        exception. The discussion here suggests that the limit is 100 calls per 30 second
                        interval in Safari in 2016. It also suggests techniques for people changing the
                        URL based on scroll position.

                        https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl
                    --}
                    Nav.replaceUrl navKey (Editor.encodeIntoUrl initialEditor)

                GalleryPage ->
                    Cmd.none

        model =
            { editor = initialEditor
            , gallery = initialGallery
            , viewingPage = viewingPage
            , url = url
            , navKey = navKey
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map EditorMsg (Editor.initialCmd initialEditor)
        , saveModelToLocalStorage (encode model)
        , updateUrlForEditor
        ]
    )



-- VIEW


documentMap : (innerMsg -> outerMsg) -> Browser.Document innerMsg -> Browser.Document outerMsg
documentMap msg document =
    let
        { title, body } =
            document
    in
    { title = title
    , body = List.map (Html.map msg) body
    }


view : Model -> Browser.Document Msg
view model =
    case model.viewingPage of
        EditorPage ->
            documentMap EditorMsg (Editor.view model.editor)

        GalleryPage ->
            documentMap GalleryMsg (Gallery.view model.gallery)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            -- Called via replaceUrl (and indirectly via click on internal links/href, see LinkClicked above)
            ( { model | viewingPage = mapRouteToPage (parseUrl url) }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.path of
                        "/" ->
                            ( model, Nav.pushUrl model.navKey (Editor.encodeIntoUrl model.editor) )

                        _ ->
                            ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        EditorMsg editorMsg ->
            let
                ( editor, editorCmd, externalMsg ) =
                    Editor.update editorMsg model.editor

                newModel =
                    { model | editor = editor }

                cmd =
                    Cmd.map EditorMsg editorCmd
            in
            case externalMsg of
                UpdatedEditor ->
                    ( newModel, Cmd.batch [ cmd, saveModelToLocalStorage (encode newModel) ] )

                UpdatedGallery image ->
                    ( { newModel | gallery = Gallery.addImage image newModel.gallery }, cmd )

                NothingToUpdate ->
                    ( newModel, cmd )

        GalleryMsg galleryMsg ->
            let
                ( gallery, galleryCmd, externalMsg ) =
                    Gallery.update galleryMsg model.gallery

                newModel =
                    { model | gallery = gallery }

                cmd =
                    Cmd.map GalleryMsg galleryCmd
            in
            case externalMsg of
                Gallery.UpdatedGallery ->
                    ( newModel, Cmd.batch [ cmd, saveModelToLocalStorage (encode newModel) ] )

                Gallery.OpenedEditor image ->
                    ( { newModel | editor = Editor.withImage image model.editor, viewingPage = EditorPage }, cmd )

                Gallery.NothingToUpdate ->
                    ( newModel, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map EditorMsg <| Editor.subscriptions model.editor (model.viewingPage == EditorPage)
        ]



-- WITH* PATTERN


withRoute : Route -> Model -> Model
withRoute route model =
    { model | viewingPage = mapRouteToPage route }



-- ENCODER
-- DECODER


keyFor : { editor : String, gallery : String }
keyFor =
    { editor = "editor"
    , gallery = "gallery"
    }


encode : { a | editor : Editor.Model, gallery : Gallery.Model } -> Encode.Value
encode { editor, gallery } =
    Encode.object
        [ ( keyFor.editor, Editor.encode editor )
        , ( keyFor.gallery, Gallery.encode gallery )
        ]


decoder : Decoder ( Editor.Model, Gallery.Model )
decoder =
    Decode.oneOf
        -- Add new model versions here!
        -- TODO Decouple editor/image decoder success/failure from gallery success/failure.
        --      What happens if just one of the two has corrupted data?
        [ Decode.map2 Tuple.pair
            (Decode.field keyFor.editor Editor.decoder)
            (Decode.field keyFor.gallery Gallery.decoder)
        ]
