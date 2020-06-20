module Pages.Gallery exposing
    ( Model
    , Msg
    , decoder
    , addImage
    , encode
    , initialModel
    , update
    , view
    )

import Browser
import Colors exposing (toCssColor)
import Components as C
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
import Html.Styled exposing (Html, div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Icons exposing (withColor, withCss, withOnClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Draw as LDraw
import LSystem.Image as Image exposing (Image)
import Routes exposing (Page(..), routeFor)
import Utils



-- MODEL


type alias Model =
    List Image



-- MSG


type Msg
    = RemovedFromGallery Int
    | CopiedToEditor Int



-- INITIAL MODEL


initialModel : Model
initialModel =
    []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RemovedFromGallery index ->
            let
                newModel =
                    Utils.dropIndex index model
            in
            -- TODO
            --( newModel, saveEncodedModelToLocalStorage (encodeModel newModel) )
            ( newModel, Cmd.none )

        CopiedToEditor index ->
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
                [ C.anchorButton routeFor.editor "Back to editor" ]
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
