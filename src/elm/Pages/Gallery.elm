module Pages.Gallery exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , addImage
    , decoder
    , encode
    , initialCmd
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
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
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
    List ImageItem


type alias ImageItem =
    { image : Image
    , hash : Int
    }



-- MSG


type Msg
    = RemovedFromGallery Int
    | CopiedToEditor Int
    | BackToEditor
    | DownloadRequested
    | UploadRequested
    | SelectedGalleryFile File
    | LoadedGallery String
    | TopBarMsg TopBar.Msg
    | ComputeSvgPathForEveryImage


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


initialCmd : Cmd Msg
initialCmd =
    Task.perform (always ComputeSvgPathForEveryImage) (Task.succeed ())



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, ExternalMsg )
update msg model =
    -- TODO: Change this to (galleryCmd, externalMsg), given that one of those msgs will be
    -- `GalleryUpdated Gallery.Model`. On other msgs the model didn't changed and isn't necessary to update
    -- the main model.
    case msg of
        RemovedFromGallery hash ->
            let
                updatedGallery =
                    case List.Extra.findIndex (.hash >> (==) hash) model.gallery of
                        Just index ->
                            List.Extra.removeAt index model.gallery

                        Nothing ->
                            model.gallery
            in
            ( { model | gallery = updatedGallery }, Cmd.none, UpdatedGallery )

        CopiedToEditor hash ->
            let
                maybeImageItem =
                    List.Extra.find (.hash >> (==) hash) model.gallery
            in
            case maybeImageItem of
                Just imageItem ->
                    ( model, Cmd.none, OpenedEditor (Just imageItem.image) )

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
            ( model, Select.file [ "application/json" ] SelectedGalleryFile, NothingToUpdate )

        SelectedGalleryFile file ->
            ( model, Task.perform LoadedGallery (File.toString file), NothingToUpdate )

        LoadedGallery galleryAsString ->
            let
                decodedString =
                    Decode.decodeString decoder galleryAsString
            in
            case decodedString of
                Ok uploadedGallery ->
                    ( { model | gallery = List.map computeSvgPath uploadedGallery.gallery ++ model.gallery }, Cmd.none, UpdatedGallery )

                Err _ ->
                    ( model, Cmd.none, NothingToUpdate )

        TopBarMsg subMsg ->
            let
                ( updatedTopBar, cmd ) =
                    TopBar.update subMsg model.topBar
            in
            ( { model | topBar = updatedTopBar }, cmd, UpdatedGallery )

        ComputeSvgPathForEveryImage ->
            ( { model | gallery = List.map computeSvgPath model.gallery }, Cmd.none, UpdatedGallery )


computeSvgPath : ImageItem -> ImageItem
computeSvgPath { image, hash } =
    { hash = hash
    , image = Image.updateSvgPathAndBoundaries image
    }



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
        List.map toUnstyled <|
            [ Lazy.lazy3 TopBar.view GalleryPage [] model.topBar
            , div
                [ css [ width (pct 100), height (pct 100), backgroundColor (toCssColor Colors.darkGray), overflow hidden ] ]
                [ Keyed.node "div"
                    [ css
                        [ width (pct (layout.transformsList + layout.mainImg))
                        , height (pct 100)
                        , padding (px 10)
                        , boxSizing borderBox
                        , overflow scroll
                        , display inlineBlock
                        ]
                    ]
                    (List.map imageBoxKeyed model.gallery)
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
            ]
    }


imageBoxKeyed : ImageItem -> ( String, Html Msg )
imageBoxKeyed imageItem =
    ( "GalleryImage_hash_" ++ String.fromInt imageItem.hash
    , Lazy.lazy imageBox imageItem
    )


imageBox : ImageItem -> Html Msg
imageBox { image, hash } =
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
        [ div [ onClick (CopiedToEditor hash), css [ width (pct 100), height (pct 100) ] ] [ LDraw.drawFixedImage image ]
        , Icons.trash
            |> withOnClick (RemovedFromGallery hash)
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
    { model | gallery = computeSvgPath { image = image, hash = Image.hash image } :: model.gallery }



-- ENCODE
-- DECODER


encode : Model -> Encode.Value
encode model =
    Encode.list Image.encode (List.map .image model.gallery)


decoder : Decoder Model
decoder =
    Image.decoder
        |> Decode.map (\image -> { image = image, hash = Image.hash image })
        |> Decode.list
        |> Decode.map (\gallery -> { initialModel | gallery = gallery })
