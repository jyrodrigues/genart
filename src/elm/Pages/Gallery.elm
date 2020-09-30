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
import Components.Dropdown as Dropdown
import Components.TopBar as TopBar
import Css
    exposing
        ( absolute
        , after
        , backgroundColor
        , before
        , borderBox
        , bottom
        , boxShadow5
        , boxSizing
        , calc
        , center
        , color
        , cursor
        , display
        , displayFlex
        , flexDirection
        , flexWrap
        , fontFamily
        , height
        , hidden
        , hover
        , inlineBlock
        , int
        , justifyContent
        , left
        , lineHeight
        , margin
        , minus
        , none
        , overflow
        , overflowY
        , padding
        , pct
        , pointer
        , position
        , property
        , px
        , relative
        , right
        , row
        , scroll
        , spaceAround
        , textAlign
        , top
        , visibility
        , visible
        , width
        , wrap
        , zIndex
        , zero
        )
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, title)
import Html.Styled.Events exposing (onClick, onMouseEnter, onMouseLeave)
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
    , imageBoxHover : Maybe Int
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
    | MouseEnter Int
    | MouseLeave


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
    , imageBoxHover = Nothing
    }


initialCmd : Cmd Msg
initialCmd =
    Cmd.batch
        [ Task.perform (always ComputeSvgPathForEveryImage) (Task.succeed ())

        -- TODO remove this line and refactor TopBar to have an option "close on click"
        -- Also this could be done as a last step, but I think it's better to do it whenever a page initializes
        , TopBar.closeAllDropdowns TopBarMsg
        ]



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

        MouseEnter hash ->
            ( { model | imageBoxHover = Just hash }, Cmd.none, UpdatedGallery )

        MouseLeave ->
            ( { model | imageBoxHover = Nothing }, Cmd.none, UpdatedGallery )


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
            [ Lazy.lazy3 TopBar.view GalleryPage topBarElements model.topBar
            , Keyed.node "div"
                [ css
                    [ width (pct 100)
                    , height (calc (pct 100) minus (px 41))
                    , padding (px 10)
                    , boxSizing borderBox
                    , backgroundColor (toCssColor Colors.darkGray)
                    , overflowY scroll
                    , displayFlex
                    , flexDirection row
                    , flexWrap wrap
                    , justifyContent spaceAround
                    ]
                ]
                (List.map (imageBoxKeyed model.imageBoxHover) model.gallery)
            ]
    }


topBarElements : List (TopBar.Element Msg)
topBarElements =
    [ TopBar.Dropdown
        { title = "Backup"
        , body =
            div [ css [ width (px 200) ] ]
                [ div [ css (Dropdown.listItemStyle False), onClick DownloadRequested ] [ text "Download backup file" ]
                , div [ css (Dropdown.listItemStyle False), onClick UploadRequested ] [ text "Import file" ]
                ]
        }
    ]


imageBoxKeyed : Maybe Int -> ImageItem -> ( String, Html Msg )
imageBoxKeyed imageBoxHover imageItem =
    ( "GalleryImage_hash_" ++ String.fromInt imageItem.hash
    , Lazy.lazy2 imageBox (Just imageItem.hash == imageBoxHover) imageItem
    )


imageBox : Bool -> ImageItem -> Html Msg
imageBox hasHover { image, hash } =
    let
        deleteVisibility =
            if hasHover then
                visibility visible

            else
                visibility hidden

        deleteIcon =
            Icons.delete
    in
    div
        [ css
            [ width (px 300)
            , height (px 300)
            , margin (px 30)
            , position relative
            , boxShadow5 zero zero (px 5) (px 1) (toCssColor Colors.black)
            , hover
                [ after
                    [ property "content" "\"Copy to editor\""
                    , width (pct 100)
                    , height (px 40)
                    , position absolute
                    , bottom zero
                    , textAlign center
                    , lineHeight (px 40)
                    , backgroundColor (toCssColor Colors.theme.active)
                    , color (toCssColor Colors.theme.color)
                    ]
                , cursor pointer
                ]
            ]
        , onMouseEnter (MouseEnter hash)
        , onMouseLeave MouseLeave
        ]
        [ div [ onClick (CopiedToEditor hash), css [ width (pct 100), height (pct 100) ] ] [ Lazy.lazy LDraw.drawFixedImage image ]
        , div
            [ onClick (RemovedFromGallery hash)
            , css
                [ backgroundColor (Colors.toCssColor Colors.blackShadow)
                , width (px 30)
                , height (px 30)
                , position absolute
                , top (px 8)
                , right (px 8)
                , deleteVisibility
                , hover [ backgroundColor (Colors.toCssColor Colors.theme.active) ]
                ]
            , title "Delete"
            ]
            [ Icons.toSvg
                { deleteIcon
                    | size = Icons.Square 30
                    , color = Colors.white
                }
            ]

        {--
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
        --}
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
