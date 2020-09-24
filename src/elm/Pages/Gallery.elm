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
import Config exposing (routeFor)
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
import Routes exposing (Page(..))
import Task
import Utils



-- MODEL


type alias Model =
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


type ExternalMsg
    = OpenedEditor (Maybe Image)
    | UpdatedGallery
    | NothingToUpdate



-- INITIAL MODEL


initialModel : Model
initialModel =
    []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, ExternalMsg )
update msg model =
    -- TODO: Change this to (galleryCmd, externalMsg), given that one of those msgs will be
    -- `GalleryUpdated Gallery.Model`. On other msgs the model didn't changed and isn't necessary to update
    -- the main model.
    case msg of
        RemovedFromGallery index ->
            let
                newModel =
                    Utils.dropIndex index model
            in
            ( newModel, Cmd.none, UpdatedGallery )

        CopiedToEditor index ->
            let
                maybeImage =
                    Utils.getAt index model
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
                    ( uploadedGallery ++ model, Cmd.none, UpdatedGallery )

                Err _ ->
                    ( model, Cmd.none, NothingToUpdate )



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
        [ div
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
                (List.indexedMap imageBox model)
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
addImage =
    (::)



-- ENCODE
-- DECODER


encode : Model -> Encode.Value
encode model =
    Encode.list Image.encode model


decoder : Decoder Model
decoder =
    Decode.list Image.decoder
