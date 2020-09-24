-- Anything other than `main` is exposed for testing only
-- TODO research how we could change this in order to only expose `main`
-- Question: Should Main module be tested? Should every test-deserving function live
--           outside Main module?


port module Main exposing (decoder, encode, main)

import Browser
import Browser.Navigation as Nav
import Config exposing (routeFor)
import Html
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core exposing (Step(..))
import Pages.Editor as Editor
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
        -- Decode Local Storage or fallback to default values
        ( editorFromStorage, galleryFromStorage ) =
            case Decode.decodeValue decoder locallyStoredModel of
                Ok ( storedEditor, storedGallery ) ->
                    ( storedEditor, storedGallery )

                Err _ ->
                    ( Editor.initialModel, Gallery.initialModel )

        route =
            parseUrl url

        -- Combine URL data with storage/default editor model
        ( editor, updateUrlIfInEditor ) =
            case route of
                Editor partialImage ->
                    -- Merging the URL encoded image with the LocalStorage one allows for easily changing the image via
                    -- query parameters!
                    ( Editor.withPartialImage partialImage editorFromStorage
                      -- Replace URL to get rid of Image in query string and have a clean URL
                    , Nav.replaceUrl navKey routeFor.editor
                    )

                _ ->
                    ( editorFromStorage, Cmd.none )

        model =
            { editor = Editor.withUrl url editor
            , gallery = galleryFromStorage
            , viewingPage = mapRouteToPage route
            , url = url
            , navKey = navKey
            }
    in
    ( model
    , Cmd.batch
        [ saveModelToLocalStorage (encode model)
        , updateUrlIfInEditor
        , Cmd.map EditorMsg (Editor.initialCmd editor)
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
                Editor.UpdatedEditor ->
                    ( newModel, Cmd.batch [ cmd, saveModelToLocalStorage (encode newModel) ] )

                Editor.UpdatedGallery image ->
                    let
                        newModelWithUpdatedGallery =
                            { newModel | gallery = Gallery.addImage image newModel.gallery }
                    in
                    ( newModelWithUpdatedGallery
                    , Cmd.batch
                        [ cmd
                        , saveModelToLocalStorage (encode newModelWithUpdatedGallery)
                        ]
                    )

                Editor.NothingToUpdate ->
                    ( newModel, cmd )

        GalleryMsg galleryMsg ->
            let
                -- TODO: Change this to (galleryCmd, externalMsg), given that one of those msgs will be
                -- `GalleryUpdated Gallery.Model`. On other msgs the model didn't changed and isn't necessary to update
                -- the main model.
                ( gallery, galleryCmd, externalMsg ) =
                    Gallery.update galleryMsg model.gallery

                cmd =
                    Cmd.map GalleryMsg galleryCmd
            in
            case externalMsg of
                Gallery.UpdatedGallery ->
                    let
                        newModel =
                            { model | gallery = gallery }
                    in
                    ( newModel, Cmd.batch [ cmd, saveModelToLocalStorage (encode newModel) ] )

                Gallery.OpenedEditor maybeImage ->
                    let
                        newModel =
                            { model | editor = Editor.withImage (Maybe.withDefault model.editor.image maybeImage) model.editor }
                    in
                    ( newModel
                    , Cmd.batch
                        [ cmd

                        -- This will trigger UrlChanged and then viewingPage will be set to its right value.
                        , Nav.replaceUrl newModel.navKey routeFor.editor
                        , saveModelToLocalStorage (encode newModel)
                        ]
                    )

                Gallery.NothingToUpdate ->
                    ( model, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map EditorMsg <| Editor.subscriptions model.editor (model.viewingPage == EditorPage)
        ]



-- ENCODER
-- DECODER


storageKeyFor : { editor : String, gallery : String }
storageKeyFor =
    { editor = "editor"
    , gallery = "gallery"
    }


encode : { a | editor : Editor.Model, gallery : Gallery.Model } -> Encode.Value
encode { editor, gallery } =
    Encode.object
        [ ( storageKeyFor.editor, Editor.encode editor )
        , ( storageKeyFor.gallery, Gallery.encode gallery )
        ]


decoder : Decoder ( Editor.Model, Gallery.Model )
decoder =
    Decode.oneOf
        -- Add new model versions here!
        -- TODO Decouple editor/image decoder success/failure from gallery success/failure.
        --      What happens if just one of the two has corrupted data?
        [ Decode.map2 Tuple.pair
            (Decode.field storageKeyFor.editor Editor.decoder)
            (Decode.field storageKeyFor.gallery Gallery.decoder)
        ]
