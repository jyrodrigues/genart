module Pages.Gallery exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , addImage
    , decoder
    , encode
    , initialModel
    , update
    , view
    )

import Browser
import Colors exposing (toCssColor)
import Components as C
import Components.TopBar as TopBar
import Css
    exposing
        ( absolute
        , backgroundColor
        , borderBox
        , bottom
        , boxShadow5
        , boxSizing
        , cursor
        , display
        , height
        , hidden
        , inlineBlock
        , left
        , margin
        , none
        , overflow
        , padding
        , pct
        , pointer
        , position
        , px
        , relative
        , scroll
        , width
        , zero
        )
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html.Styled exposing (Html, div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Icons exposing (withColor, withCss, withOnClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Draw as LDraw
import LSystem.Image as Image exposing (Image)
import List.Extra
import Pages exposing (Page(..))
import Task
import Utils



-- MODEL


type alias Model =
    { gallery : Gallery
    , topBar : TopBar.State Msg
    }


type alias Gallery =
    List Image



-- MSG


type Msg
    = RemovedFromGallery Int
    | CopiedToEditor Int
    | BackToEditor
    | DownloadRequested
    | UploadRequested
    | SelectedGallery File
    | LoadedGallery String
    | TopBarMsg TopBar.Msg


type
    ExternalMsg
    -- Maybe Image because when clicking on "Go back to editor" we set it with Nothing
    = OpenedEditor (Maybe Image)
    | UpdatedGallery
    | NothingToUpdate



-- INITIAL MODEL


initialModel : Model
initialModel =
    { gallery = []
    , topBar = TopBar.init TopBarMsg
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, ExternalMsg )
update msg model =
    -- TODO: Change this to (galleryCmd, externalMsg), given that one of those msgs will be
    -- `GalleryUpdated Gallery.Model`. On other msgs the model didn't changed and isn't necessary to update
    -- the main model.
    case msg of
        RemovedFromGallery index ->
            let
                updatedGallery =
                    Utils.dropIndex index model.gallery
            in
            ( { model | gallery = updatedGallery }, Cmd.none, UpdatedGallery )

        CopiedToEditor index ->
            let
                maybeImage =
                    List.Extra.getAt index model.gallery
            in
            case maybeImage of
                Just image ->
                    ( model, Cmd.none, OpenedEditor (Just image) )

                Nothing ->
                    ( model, Cmd.none, NothingToUpdate )

        BackToEditor ->
            ( model, Cmd.none, OpenedEditor Nothing )

        DownloadRequested ->
            let
                galleryAsString =
                    Encode.encode 4 (encode model)
            in
            ( model, Download.string "genart-gallery.json" "application/json" galleryAsString, NothingToUpdate )

        UploadRequested ->
            ( model, Select.file [ "application/json" ] SelectedGallery, NothingToUpdate )

        SelectedGallery file ->
            ( model, Task.perform LoadedGallery (File.toString file), NothingToUpdate )

        LoadedGallery galleryAsString ->
            let
                decodedString =
                    Decode.decodeString decoder galleryAsString
            in
            case decodedString of
                Ok uploadedGallery ->
                    ( { model | gallery = uploadedGallery.gallery ++ model.gallery }, Cmd.none, UpdatedGallery )

                Err _ ->
                    ( model, Cmd.none, NothingToUpdate )

        TopBarMsg subMsg ->
            let
                ( updatedTopBar, cmd ) =
                    TopBar.update subMsg model.topBar
            in
            ( { model | topBar = updatedTopBar }, cmd, UpdatedGallery )



-- VIEW
-- TODO this is duplicated here and on Editor.elm


layout :
    { controlPanel : Float
    , transformsList : Float
    , mainImg : Float
    }
layout =
    { controlPanel = 15
    , transformsList = 15
    , mainImg = 70
    }


view : Model -> Browser.Document Msg
view model =
    { title = "Generative Art"
    , body =
        [ TopBar.view GalleryPage [] model.topBar |> toUnstyled
        , div
            [ css [ width (pct 100), height (pct 100), backgroundColor (toCssColor Colors.darkGray), overflow hidden ] ]
            [ div
                [ css
                    [ width (pct (layout.transformsList + layout.mainImg))
                    , height (pct 100)
                    , padding (px 10)
                    , boxSizing borderBox
                    , overflow scroll
                    , display inlineBlock
                    ]
                ]
                (List.indexedMap imageBox model.gallery)
            , C.fixedDiv
                [ css
                    [ width (pct layout.controlPanel)
                    , height (pct 100)
                    , display inlineBlock
                    , padding (px 10)
                    , boxSizing borderBox
                    ]
                ]
                [ C.primaryButton BackToEditor "Back to editor"
                , C.primaryButton DownloadRequested "Download gallery"
                , C.primaryButton UploadRequested "Upload gallery"
                ]
            ]
            |> toUnstyled
        ]
    }


imageBox : Int -> Image -> Html Msg
imageBox index image =
    div
        [ css
            [ width (px 300)
            , height (px 300)
            , display inlineBlock
            , margin (px 10)
            , position relative
            , overflow hidden
            , boxShadow5 zero zero (px 5) (px 1) (toCssColor Colors.black)
            ]
        ]
        [ LDraw.drawFixedImage (Just (CopiedToEditor index)) image
        , Icons.trash
            |> withOnClick (RemovedFromGallery index)
            |> withColor Colors.red_
            |> withCss
                [ cursor pointer
                , position absolute
                , bottom (px 5)
                , left (px 5)
                ]
            |> Icons.toSvg
        ]



-- FUNCTIONS


addImage : Image -> Model -> Model
addImage image model =
    { model | gallery = image :: model.gallery }



-- ENCODE
-- DECODER


encode : Model -> Encode.Value
encode model =
    Encode.list Image.encode model.gallery


decoder : Decoder Model
decoder =
    Decode.map (\gallery -> { initialModel | gallery = gallery }) <|
        Decode.list Image.decoder
