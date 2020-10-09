module Pages.Gallery exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , addImage
    , decoder
    , encode
    , initialCmd
    , initialModel
    , subscriptions
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
        ( Style
        , absolute
        , after
        , auto
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
        , deg
        , display
        , displayFlex
        , flexDirection
        , flexWrap
        , fontFamily
        , grab
        , grabbing
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
        , num
        , opacity
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
        , rotate
        , row
        , scroll
        , spaceAround
        , textAlign
        , top
        , transform
        , visibility
        , visible
        , width
        , wrap
        , zIndex
        , zero
        )
import DnDList
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html.Styled exposing (Attribute, Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, fromUnstyled, id, title)
import Html.Styled.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
import Icons exposing (iconDelete, withColor, withCss, withOnClick)
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
    , dnd : DnDList.Model
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
    | DownloadRequested
    | UploadRequested
    | SelectedGalleryFile File
    | LoadedGallery String
    | TopBarMsg TopBar.Msg
    | ComputeSvgPathForEveryImage
    | MouseEnter Int
    | MouseLeave
    | DnDListMsg DnDList.Msg


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
    , dnd = dndSystem.model
    }


initialCmd : Cmd Msg
initialCmd =
    Cmd.batch
        [ Task.perform (always ComputeSvgPathForEveryImage) (Task.succeed ())

        -- TODO remove this line and refactor TopBar to have an option "close on click"
        -- Also this could be done as a last step, but I think it's better to do it whenever a page initializes
        , TopBar.closeAllDropdowns TopBarMsg
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dndSystem.subscriptions model.dnd
        , TopBar.subscriptions model.topBar
        ]



-- DnDList / Drag and Drop List


dndSystem : DnDList.System ImageItem Msg
dndSystem =
    DnDList.create
        { beforeUpdate = \_ _ list -> list
        , movement = DnDList.Free
        , listen = DnDList.OnDrag
        , operation = DnDList.Rotate
        }
        DnDListMsg



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

        DnDListMsg subMsg ->
            let
                ( dnd, gallery ) =
                    dndSystem.update subMsg model.dnd model.gallery
            in
            ( { model | dnd = dnd, gallery = gallery }, dndSystem.commands dnd, UpdatedGallery )


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
                [ css galleryStyle ]
                (List.indexedMap (imageBoxKeyed model.imageBoxHover model.dnd) model.gallery)
            ]
                ++ ghostImageBox model.dnd model.gallery
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


imageBoxKeyed : Maybe Int -> DnDList.Model -> Int -> ImageItem -> ( String, Html Msg )
imageBoxKeyed imageBoxHover dnd index imageItem =
    let
        imageId =
            "gallery-image-id-" ++ String.fromInt imageItem.hash
    in
    case dndSystem.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                ( imageId
                , imageBox
                    imageItem
                    (id imageId :: List.map fromUnstyled (dndSystem.dropEvents index imageId))
                    (Just imageItem.hash == imageBoxHover)
                    True
                    []
                )

            else
                ( imageId
                , imageBox
                    imageItem
                    [ id imageId ]
                    (Just imageItem.hash == imageBoxHover)
                    True
                    [ opacity (num 0.3) ]
                )

        Nothing ->
            ( imageId
            , imageBox
                imageItem
                (id imageId :: List.map fromUnstyled (dndSystem.dragEvents index imageId))
                (Just imageItem.hash == imageBoxHover)
                False
                []
            )


imageBox : ImageItem -> List (Attribute Msg) -> Bool -> Bool -> List Style -> Html Msg
imageBox { image, hash } dndEvents hasHover isDragging styles =
    div
        [ css (imageBoxStyle ++ styles)
        , onMouseEnter (MouseEnter hash)
        , onMouseLeave MouseLeave
        ]
        -- Image
        [ Lazy.lazy LDraw.drawFixedImage image

        -- Drag handler
        , div (css (setCursorGrabbing isDragging :: dndHandleAreaStyle) :: dndEvents) []

        -- Icon delete
        , div
            [ onClick (RemovedFromGallery hash)
            , title "Delete"
            , css (setVisibility (hasHover && not isDragging) :: deleteStyle)
            ]
            [ Icons.toSvg { iconDelete | size = Icons.Square 30, color = Colors.white } ]

        -- Copy to Editor
        , div
            [ onClick (CopiedToEditor hash)
            , css (setVisibility (hasHover && not isDragging) :: copyToEditorStyle)
            ]
            [ text "Copy to editor" ]
        ]


ghostImageBox : DnDList.Model -> List ImageItem -> List (Html Msg)
ghostImageBox dnd items =
    let
        maybeDragItem : Maybe ImageItem
        maybeDragItem =
            dndSystem.info dnd
                |> Maybe.andThen (\{ dragIndex } -> List.Extra.getAt dragIndex items)
    in
    case maybeDragItem of
        Just imageItem ->
            [ div (List.map fromUnstyled (dndSystem.ghostStyles dnd))
                [ imageBox
                    imageItem
                    []
                    False
                    True
                    [ transform (rotate (deg 4)), margin zero ]
                ]
            ]

        Nothing ->
            []



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



-- STYLES


galleryStyle : List Style
galleryStyle =
    [ width (pct 100)
    , height (calc (pct 100) minus (px 41))
    , padding (px 10)
    , boxSizing borderBox
    , backgroundColor (toCssColor Colors.darkGray)
    , overflowY auto
    , displayFlex
    , flexDirection row
    , flexWrap wrap
    , justifyContent spaceAround
    ]


imageBoxStyle : List Style
imageBoxStyle =
    [ width (px 300)
    , height (px 300)
    , margin (px 30)
    , position relative
    , boxShadow5 zero zero (px 5) (px 1) (toCssColor Colors.black)
    ]


deleteStyle : List Style
deleteStyle =
    [ backgroundColor (Colors.toCssColor Colors.blackShadow)
    , width (px 30)
    , height (px 30)
    , position absolute
    , top (px 8)
    , right (px 8)
    , hover [ backgroundColor (Colors.toCssColor Colors.theme.active) ]
    ]


copyToEditorStyle : List Style
copyToEditorStyle =
    [ width (pct 100)
    , height (px 40)
    , position absolute
    , bottom zero
    , textAlign center
    , lineHeight (px 40)
    , backgroundColor (toCssColor Colors.theme.active)
    , color (toCssColor Colors.theme.color)
    , cursor pointer
    ]


dndHandleAreaStyle : List Style
dndHandleAreaStyle =
    [ width (px 250)
    , height (px 250)
    , position absolute
    , top zero
    , left zero
    ]



-- HELPERS


setVisibility : Bool -> Style
setVisibility isVisible =
    if isVisible then
        visibility visible

    else
        visibility hidden


setCursorGrabbing : Bool -> Style
setCursorGrabbing isGrabbing =
    if isGrabbing then
        cursor grabbing

    else
        cursor grab
