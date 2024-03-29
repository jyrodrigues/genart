-- Anything other than `main` is exposed for testing only
-- TODO research how we could change this in order to only expose `main`
-- Question: Should Main module be tested? Should every test-deserving function live
--           outside Main module?


port module Main exposing (decoder, encode, main)

--import Components.TopBar

import Browser
import Browser.Navigation as Nav
import Css
import Css.Global
import Html
import Html.Styled
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core exposing (Step(..))
import Pages exposing (Page(..), routeFor)
import Pages.DevPlayground as Dev
import Pages.Editor as Editor
import Pages.Gallery as Gallery
import Pages.Welcome as Welcome
import Pages.Writing as Writing
import Routes exposing (Route(..), mapRouteToPage, parseUrl)
import Url



-- PORTS


port saveModelToLocalStorage : Encode.Value -> Cmd msg



-- MODEL


type alias Model =
    -- Pages
    { editor : Editor.Model
    , gallery : Gallery.Model
    , welcome : Welcome.Model
    , writting : Writing.Model
    , dev : Dev.Model
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
    | WelcomeMsg Welcome.Msg
    | WritingMsg Writing.Msg
    | DevMsg Dev.Msg



-- INI MODEL
{--TODO
initialModel : Image -> List Image -> Url.Url -> Nav.Key -> Model
initialModel image gallery url navKey =
--}


initialModel : Url.Url -> Nav.Key -> Model
initialModel url navKey =
    -- Pages
    { editor = Editor.initialModel
    , gallery = Gallery.initialModel
    , writting = Writing.initialModel
    , welcome = Welcome.initialModel
    , dev = Dev.initialModel

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
        --
        -- Note: Assuming after first visit there will be something in local storage to decode.
        --       and so we don't want to show the Welcome page again, go straight to Editor.
        --
        ( editorFromStorage, galleryFromStorage, overrideWelcomeWithEditorPage ) =
            case Decode.decodeValue decoder locallyStoredModel of
                Ok ( storedEditor, storedGallery ) ->
                    ( storedEditor, storedGallery, True )

                Err _ ->
                    ( Editor.initialModel, Gallery.initialModel, False )

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
                    , Nav.replaceUrl navKey (routeFor EditorPage)
                    )

                NotFound ->
                    ( editorFromStorage
                      -- Replace URL to get rid of unknown path and have a clean URL
                    , Nav.replaceUrl navKey (routeFor EditorPage)
                    )

                _ ->
                    ( editorFromStorage, Cmd.none )

        model =
            initialModel url navKey

        modelWithUrlAndStorage =
            { model
                | editor = Editor.withUrl url editor
                , gallery = galleryFromStorage
                , viewingPage = mapRouteToPage route overrideWelcomeWithEditorPage
            }
    in
    ( modelWithUrlAndStorage
    , Cmd.batch
        [ saveModelToLocalStorage (encode modelWithUrlAndStorage)
        , updateUrlIfInEditor
        , Cmd.map EditorMsg (Editor.initialCmd editor)
        , Cmd.map WelcomeMsg Welcome.initialCmd
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
    , body =
        List.map (Html.map msg) body
            ++ [ Html.Styled.toUnstyled
                    (Css.Global.global [ Css.Global.selector "body" [ Css.fontFamily Css.sansSerif ] ])
               ]
    }


view : Model -> Browser.Document Msg
view model =
    case model.viewingPage of
        EditorPage ->
            documentMap EditorMsg (Editor.view model.editor)

        GalleryPage ->
            documentMap GalleryMsg (Gallery.view model.gallery)

        WelcomePage ->
            documentMap WelcomeMsg (Welcome.view model.welcome)

        WritingPage ->
            documentMap WritingMsg (Writing.view model.writting)

        DevPage ->
            documentMap DevMsg (Dev.view model.dev)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            let
                viewingPage =
                    mapRouteToPage (parseUrl url) False
            in
            -- Called via replaceUrl (and indirectly via click on internal links/href, see LinkClicked above)
            ( { model | viewingPage = viewingPage }, initializeCmds model viewingPage )

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
                        , Nav.replaceUrl newModel.navKey (routeFor EditorPage)
                        , saveModelToLocalStorage (encode newModel)
                        ]
                    )

                Gallery.NothingToUpdate ->
                    ( model, cmd )

        WelcomeMsg welcomeMsg ->
            let
                ( welcome, welcomeCmd, externalMsg ) =
                    Welcome.update welcomeMsg model.welcome

                cmd =
                    Cmd.map WelcomeMsg welcomeCmd
            in
            case externalMsg of
                Welcome.GoToEditor image ->
                    ( { model | editor = Editor.withImage image model.editor }
                    , Nav.replaceUrl model.navKey (routeFor EditorPage)
                    )

                Welcome.UpdateWelcome ->
                    ( { model | welcome = welcome }, cmd )

        DevMsg devMsg ->
            let
                ( dev, devCmd, externalMsg ) =
                    Dev.update devMsg model.dev

                cmd =
                    Cmd.map DevMsg devCmd
            in
            case externalMsg of
                Dev.GoToEditor image ->
                    ( { model | editor = Editor.withImage image model.editor }
                    , Nav.replaceUrl model.navKey (routeFor EditorPage)
                    )

                Dev.UpdateWelcome ->
                    ( { model | dev = dev }, cmd )

        WritingMsg writtingMsg ->
            let
                ( writting, writtingCmd, externalMsg ) =
                    Writing.update writtingMsg model.writting

                cmd =
                    Cmd.map WritingMsg writtingCmd
            in
            case externalMsg of
                Writing.OpenedEditor image ->
                    ( { model | editor = Editor.withImage image model.editor }
                    , Nav.replaceUrl model.navKey (routeFor EditorPage)
                    )

                Writing.UpdateWriting ->
                    ( { model | writting = writting }, cmd )

                Writing.NothingToUpdate ->
                    ( model, cmd )



-- TODO Refactor to something like changeViews to be called on UrlChanged
-- TODO Maybe remove initialCmds from init function?


initializeCmds : Model -> Page -> Cmd Msg
initializeCmds model page =
    case page of
        EditorPage ->
            Cmd.map EditorMsg (Editor.initialCmd model.editor)

        GalleryPage ->
            Cmd.map GalleryMsg Gallery.initialCmd

        WelcomePage ->
            Cmd.map WelcomeMsg Welcome.initialCmd

        WritingPage ->
            Cmd.map WritingMsg Writing.initialCmd

        DevPage ->
            Cmd.map DevMsg Dev.initialCmd



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.viewingPage of
        EditorPage ->
            Sub.map EditorMsg <| Editor.subscriptions model.editor

        GalleryPage ->
            Sub.map GalleryMsg <| Gallery.subscriptions model.gallery

        WelcomePage ->
            Sub.map WelcomeMsg <| Welcome.subscriptions model.welcome

        WritingPage ->
            Sub.map WritingMsg <| Writing.subscriptions model.writting

        DevPage ->
            Sub.map DevMsg <| Dev.subscriptions model.dev



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
