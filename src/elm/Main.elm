-- Anything other than `main` is exposed for testing only
-- TODO research how we could change this in order to only expose `main`
-- Question: Should Main module be tested? Should every test-deserving function live
--           outside Main module?


module Main exposing
    ( Route(..)
    , main
    , parseUrl
    )

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
import Random
import Set exposing (Set)
import Svg.Styled exposing (Svg)
import Task
import Time
import Url
import Url.Parser as Parser exposing (Parser)
import Utils exposing (delay, floatModBy)



-- TYPES AND MSG
-- MSG


{-| TODO: Rename Msgs: put all verbs in past tense and choose better words.
-}
type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
      -- Editor
    | EditorMsg Editor.Msg
      -- Storage
    | RemovedFromGallery Int
    | DuplicateToEdit Int
      -- Sometimes there is nothing to do
    | NoOp



-- MODEL


type alias Model =
    -- Pages
    { editorModel : Editor.Model

    -- Storage
    , gallery : List Image
    , viewingPage : Page

    -- Url
    , url : Url.Url
    , navKey : Nav.Key
    }



-- ROUTING


type Page
    = EditorPage
    | GalleryPage


type Route
    = Editor PartialImage
    | Gallery
    | NotFound


mapRouteToPage : Route -> Page
mapRouteToPage route =
    -- TODO think about this flow that requires this function. It doesn't seem the best one.
    case route of
        Gallery ->
            GalleryPage

        _ ->
            EditorPage



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
    { editorModel = Editor.initialModel

    -- Storage
    , gallery = []
    , viewingPage = EditorPage

    -- Url
    , url = url
    , navKey = navKey
    }


modelWithRoute : Route -> Model -> Model
modelWithRoute route model =
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
            documentMap EditorMsg (Editor.view model.editorModel)

        GalleryPage ->
            galleryView model



-- GALLERY VIEW
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


galleryView : Model -> Browser.Document Msg
galleryView model =
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
                [ C.anchorButton (routeFor EditorPage) "Back to editor" ]
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
        [ LDraw.drawFixedImage (Just (DuplicateToEdit index)) image
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
                            ( model, Nav.pushUrl model.navKey (Editor.urlEncoder model.editorModel) )

                        _ ->
                            ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        EditorMsg editorMsg ->
            let
                ( editorModel, editorCmd ) =
                    Editor.update editorMsg model.editorModel
            in
            ( { model | editorModel = editorModel }, Cmd.map EditorMsg editorCmd )

        RemovedFromGallery index ->
            let
                newModel =
                    { model | gallery = Utils.dropIndex index model.gallery }
            in
            -- TODO
            --( newModel, saveEncodedModelToLocalStorage (encodeModel newModel) )
            ( newModel, Cmd.none )

        DuplicateToEdit index ->
            {--TODO
            let
                image =
                    Utils.getAt index model.gallery
                        |> Maybe.withDefault model.image
            in
            updateAndSaveImageAndGallery <|
                { model
                    | viewingPage = EditorPage
                    , image = image
                    , colorWheel = updateColorWheel image model.colorTarget model.colorWheel
                }
            --}
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map EditorMsg <| Editor.subscriptions model.editorModel (model.viewingPage == EditorPage)

        --, Sub.map GalleryMsg <| Gallery.subscriptions model.galleryModel (model.viewingPage == GalleryPage)
        ]



-- URL


parseUrl : Url.Url -> Route
parseUrl url =
    let
        parsedRoute =
            Parser.parse (Parser.oneOf [ editorParser, galleryParser ]) url
    in
    case parsedRoute of
        Just route ->
            route

        Nothing ->
            NotFound


{-| This function used on `galleryParser` makes it asymmetric with `editorParser`.
--- Not sure if this is a good idea.
-}
routeFor : Page -> String
routeFor page =
    case page of
        GalleryPage ->
            "gallery"

        EditorPage ->
            "/"


galleryParser : Parser (Route -> a) a
galleryParser =
    Parser.map Gallery (Parser.s (routeFor GalleryPage))


editorParser : Parser (Route -> a) a
editorParser =
    Parser.map Editor (Parser.query Image.queryParser)


{-| Current version of Url building and parsing
-}
replaceUrl : Nav.Key -> Image -> Cmd msg
replaceUrl key image =
    {--
            Note about Nav.replaceUrl: Browsers may rate-limit this function by throwing an
            exception. The discussion here suggests that the limit is 100 calls per 30 second
            interval in Safari in 2016. It also suggests techniques for people changing the
            URL based on scroll position.

            https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl
    --}
    Nav.replaceUrl key (routeFor EditorPage ++ Image.toQuery image)
