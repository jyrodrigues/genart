-- Anything other than `main` is exposed for testing only
-- TODO research how we could change this in order to only expose `main`
-- Question: Should Main module be tested? Should every test-deserving function live
--           outside Main module?


module Main exposing (main)

import Browser
import Browser.Dom exposing (Element)
import Browser.Events
import Browser.Navigation as Nav
import ColorWheel
import Colors exposing (Color, offWhite, toCssColor)
import Components as C
import Css
    exposing
        ( absolute
        , auto
        , backgroundColor
        , block
        , border
        , border3
        , borderBox
        , borderLeft3
        , borderRadius
        , borderRight3
        , bottom
        , boxShadow5
        , boxShadow6
        , boxSizing
        , breakWord
        , color
        , contentBox
        , cursor
        , display
        , displayFlex
        , fixed
        , flexWrap
        , fontSize
        , height
        , hidden
        , hover
        , inlineBlock
        , inset
        , left
        , margin
        , margin3
        , marginBottom
        , marginTop
        , none
        , overflow
        , overflowWrap
        , overflowX
        , overflowY
        , padding
        , padding2
        , pct
        , pointer
        , position
        , px
        , relative
        , right
        , scroll
        , solid
        , unset
        , vw
        , width
        , wrap
        , zero
        )
import Events exposing (ShiftKey, keyPressDecoder, midiEventDecoder, mousePositionDecoder, onKeyDown, onWheel)
import Html
import Html.Styled exposing (Html, div, input, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css, id, type_, value)
import Html.Styled.Events
    exposing
        ( on
        , onBlur
        , onClick
        , onFocus
        , onInput
        , onMouseUp
        )
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy exposing (lazy)
import Icons exposing (withColor, withCss, withOnClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core exposing (Step(..))
import LSystem.Draw as LDraw exposing (drawBlocks, drawImage)
import LSystem.Image as Image
    exposing
        ( Image
        , PartialImage
        , Polygon(..)
        , Position
        , defaultImage
        , encodeImage
        , imageDecoder
        , withImage
        )
import Midi exposing (adjustInputForStrokeWidth)
import Pages.Editor as Editor
import Pages.Gallery as Gallery
import Random
import Routes exposing (Page(..), Route(..), mapRouteToPage, parseUrl)
import Set exposing (Set)
import Svg.Styled exposing (Svg)
import Task
import Time
import Url
import Url.Parser as Parser exposing (Parser)
import Utils exposing (delay, floatModBy)



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



-- MSG and TYPES
-- TYPES


{-| TODO: Rename Msgs: put all verbs in past tense and choose better words.
-}
type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | EditorMsg Editor.Msg
    | GalleryMsg Gallery.Msg
      -- Sometimes there is nothing to do
    | NoOp



-- FLAGS


type alias Flags =
    Encode.Value



-- MODEL
-- IMPLEMENTATION, LOGIC, FUNCTIONS
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


withRoute : Route -> Model -> Model
withRoute route model =
    { model | viewingPage = mapRouteToPage route }



{--
encodeModel : { a | image : Image, gallery : List Image } -> Encode.Value
encodeModel { image, gallery } =
    Encode.object
        [ ( keyFor.image, encodeImage image )
        , ( keyFor.gallery, Encode.list encodeImage gallery )
        ]


modelDecoder : Decoder ( Image, List Image )
modelDecoder =
    Decode.oneOf
        -- Add new model versions here!
        [ Decode.map2 Tuple.pair
            (Decode.field keyFor.image imageDecoder)
            (Decode.field keyFor.gallery (Decode.list imageDecoder))
        ]


keyFor =
    { image = "image"
    , gallery = "gallery"
    }
--}
-- MAIN


main : Program Flags Model Msg
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
{--
- separate gallery from image in localstorage
- parse url
- init editor model with url (if present) and localStorage (also if present)
--}
{--TODO
init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init localStorage url navKey =
    let
        decodedLocalStorage =
            Decode.decodeValue modelDecoder localStorage

        route =
            parseUrl url

        ( image, gallery ) =
            combineUrlAndStorage decodedLocalStorage route

        model =
            initialModel image gallery url navKey
                |> modelWithRoute route
                |> modelWithEditIndexLast

        updateUrl =
            case route of
                Gallery ->
                    []

                _ ->
                    [ replaceUrl navKey image ]
    in
    ( model
    , Cmd.batch
        ([ getImgDivPosition
         , saveEncodedModelToLocalStorage (encodeModel model)
         , Cmd.map ColorWheelMsg (ColorWheel.getElementDimensions model.colorWheel)
         ]
            ++ updateUrl
        )
    )
--}


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init localStorage url navKey =
    ( initialModel url navKey, Cmd.none )



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
                            ( model, Nav.pushUrl model.navKey (Editor.urlEncoder model.editor) )

                        _ ->
                            ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        EditorMsg editorMsg ->
            let
                ( editor, cmd ) =
                    Editor.update editorMsg model.editor
            in
            ( { model | editor = editor }, Cmd.map EditorMsg cmd )

        GalleryMsg galleryMsg ->
            let
                ( gallery, cmd ) =
                    Gallery.update galleryMsg model.gallery
            in
            ( { model | gallery = gallery }, Cmd.map GalleryMsg cmd )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map EditorMsg <| Editor.subscriptions model.editor (model.viewingPage == EditorPage)
        ]
