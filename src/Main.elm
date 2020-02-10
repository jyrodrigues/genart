port module Main exposing (main)

import Browser
import Browser.Dom exposing (Element)
import Browser.Events
import Browser.Navigation as Nav
import Colors exposing (Color, offWhite, toCssColor)
import Css
    exposing
        ( absolute
        , backgroundColor
        , block
        , border3
        , borderBottom3
        , borderBox
        , borderLeft3
        , borderRight3
        , bottom
        , boxSizing
        , color
        , cursor
        , display
        , fixed
        , fontFamily
        , fontSize
        , height
        , hidden
        , inlineBlock
        , left
        , margin
        , overflow
        , overflowX
        , overflowY
        , padding
        , pct
        , pointer
        , position
        , px
        , relative
        , right
        , scroll
        , solid
        , width
        , zero
        )
import Html.Styled exposing (Html, br, button, div, h2, input, label, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css, for, id, type_, value)
import Html.Styled.Events
    exposing
        ( on
        , onBlur
        , onClick
        , onFocus
        , onInput
        , onMouseUp
        , preventDefaultOn
        )
import Icons exposing (withColor, withConditionalColor, withCss, withOnClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core as LCore exposing (Block, Composition, Step(..))
import LSystem.Draw as LDraw
    exposing
        ( Image
        , Polygon(..)
        , drawImage
        , withBackgroundColor
        , withComposition
        , withId
        , withScale
        , withStrokeColor
        , withTranslate
        , withTurnAngle
        )
import ListExtra exposing (pairExec, pairMap)
import Task
import Time
import Url
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser, string)
import Url.Parser.Query as Query



-- PORTS


port saveStateToLocalStorage : Encode.Value -> Cmd msg


port downloadSvg : () -> Cmd msg



-- TYPES
-- MSG


{-| TODO: Rename Msgs: put all verbs in past tense and choose better words.
-}
type Msg
    = ViewingPage Page
      -- Global keyboard listener
    | KeyPress String
      -- Main commands
    | ResetDrawing
    | BasePolygonChanged Polygon
    | Iterate Int
    | Deiterate
    | SetEditingIndex Int
    | DropFromState Int
      -- Storage
    | SavedToGallery
    | RemovedFromGallery Int
    | DuplicateToEdit Int
      -- Pan and Zoom
    | GotImgDivPosition (Result Browser.Dom.Error Element)
    | PanStarted Position
    | PanEnded
    | MouseMoved Position
    | Zoom Float Float ShiftKey Position
      -- Colors
    | SetBackgroundColor Color
    | SetStrokeColor Color
      -- Angle
    | SetTurnAngle Float
      -- Download
    | DownloadSvg
      -- Focus
    | SetFocus Focus
      -- URL
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
      -- Video
    | TogglePlayingVideo
    | VideoSpeedFaster
    | VideoSpeedSlower
    | ReverseAngleChangeDirection



-- MODEL
{--
type Model
    = Editor EditorModel Nav.Key
    | Gallery GalleryModel Nav.Key


type GalleryModel
    = GalleryModel (List Image Msg)
--}


type alias Model =
    -- Aplication heart
    { image : Image Msg
    , editingIndex : Int
    , gallery : Gallery
    , viewingPage : Page

    -- Pan and Zoom
    , panStarted : Bool
    , lastPos : Position

    -- Main Image Div Coordinates
    , imgDivCenter : Position
    , imgDivStart : Position

    -- Browser Focus
    , focus : Focus

    -- Url
    , url : Url.Url
    , navKey : Nav.Key

    -- Video
    , videoAngleChangeRate : Float
    , playingVideo : Bool
    , videoAngleChangeDirection : Float
    }



-- ROUTING


type Page
    = EditorPage
    | GalleryPage


type Route
    = Editor (Image Msg)
    | Gallery
    | NotFound


mapRouteToPage : Route -> Page
mapRouteToPage route =
    case route of
        Gallery ->
            GalleryPage

        _ ->
            EditorPage


type alias Gallery =
    List (Image Msg)


type alias ImageAndGallery =
    { image : Image Msg
    , gallery : Gallery
    }



-- FLAGS


type alias Flags =
    Encode.Value



-- OTHER TYPES


type alias ShiftKey =
    Bool


type Focus
    = KeyboardEditing
    | TurnAngleInput


type alias Position =
    ( Float, Float )



-- IMPLEMENTATION, LOGIC, FUNCTIONS
-- MODEL


initialModel : Image Msg -> Gallery -> Url.Url -> Nav.Key -> Model
initialModel image gallery url navKey =
    -- Aplication heart
    { image = image
    , editingIndex = 1

    -- Storage
    , gallery = gallery
    , viewingPage = EditorPage

    -- Pan and Zoom
    , panStarted = False
    , lastPos = ( 0, 0 )

    -- Main Image Div Coordinates
    , imgDivCenter = ( 0, 0 )
    , imgDivStart = ( 0, 0 )

    -- Browser Focus
    , focus = KeyboardEditing

    -- Url
    , url = url
    , navKey = navKey

    -- Video
    , videoAngleChangeRate = 0.01
    , playingVideo = False
    , videoAngleChangeDirection = 1
    }


initialModelFromImageAndGallery : ImageAndGallery -> Url.Url -> Nav.Key -> Model
initialModelFromImageAndGallery { image, gallery } url navKey =
    initialModel image gallery url navKey


modelWithRoute : Route -> Model -> Model
modelWithRoute route model =
    { model | viewingPage = mapRouteToPage route }


modelWithEditIndexLast : Model -> Model
modelWithEditIndexLast model =
    { model | editingIndex = LCore.length (LDraw.getComposition model.image) - 1 }



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


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init localStorage url navKey =
    let
        route =
            parseUrl url

        imageAndGallery =
            decodeAndCombineUrlAndStorage localStorage route

        model =
            initialModelFromImageAndGallery imageAndGallery url navKey
                |> modelWithRoute route
                |> modelWithEditIndexLast

        updateUrl =
            case route of
                Gallery ->
                    []

                _ ->
                    [ replaceUrl navKey imageAndGallery.image ]
    in
    ( model
    , Cmd.batch
        ([ getImgDivPosition
         , saveStateToLocalStorage (encodeModel model)
         ]
            ++ updateUrl
        )
    )


decodeAndCombineUrlAndStorage : Flags -> Route -> ImageAndGallery
decodeAndCombineUrlAndStorage localStorage route =
    let
        resultImageAndGalleryFromStorage =
            Decode.decodeValue imageAndGalleryDecoder localStorage
    in
    case ( route, resultImageAndGalleryFromStorage ) of
        ( Editor urlImage, Ok storedImageAndGallery ) ->
            { image = urlImage, gallery = storedImageAndGallery.gallery }

        ( Editor urlImage, Err _ ) ->
            { image = urlImage, gallery = [] }

        ( _, Ok storedImageAndGallery ) ->
            storedImageAndGallery

        ( _, Err _ ) ->
            { image = LDraw.initialImage, gallery = [] }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.viewingPage of
        EditorPage ->
            editorView model

        GalleryPage ->
            galleryView model



-- GALLERY VIEW


galleryView : Model -> Browser.Document Msg
galleryView model =
    { title = "Generative Art"
    , body =
        [ div
            [ css [ width (pct 100), height (pct 100), backgroundColor (toCssColor Colors.darkGray), overflow hidden ] ]
            [ fixedDiv
                [ css
                    [ width (pct layout.controlPanel)
                    , height (pct 100)
                    , right zero
                    ]
                ]
                [ button [ onClick (ViewingPage EditorPage), css [ margin (px 10) ] ] [ text "Back to editor" ] ]
            , div
                [ css
                    [ width (pct (layout.transformsList + layout.mainImg))
                    , height (pct 100)
                    , padding (px 10)
                    , overflow scroll
                    ]
                ]
                (List.indexedMap imageBox model.gallery)
            ]
            |> toUnstyled
        ]
    }


imageBox : Int -> Image Msg -> Html Msg
imageBox index image =
    div
        [ css
            [ width (pct 40)
            , borderBottom3 (px 1) solid (toCssColor Colors.black)
            , display inlineBlock
            , margin (px 10)
            , position relative
            ]
        ]
        [ image
            |> LDraw.withOnClick (DuplicateToEdit index)
            |> drawImage
        , Icons.trash
            |> withOnClick (RemovedFromGallery index)
            |> withColor Colors.red_
            |> withCss
                [ cursor pointer
                , border3 (px 1) solid (toCssColor Colors.black)
                , position absolute
                , bottom zero
                , left zero
                ]
            |> Icons.toSvg
        ]



-- EDITOR VIEW


editorView : Model -> Browser.Document Msg
editorView model =
    { title = "Generative Art"
    , body =
        [ div
            [ css [ width (pct 100), height (pct 100) ] ]
            [ controlPanel model
            , transformsList model
            , mainImg model
            ]
            |> toUnstyled
        ]
    }


controlPanel : Model -> Html Msg
controlPanel model =
    let
        backgroundColor_ =
            LDraw.getBackgroundColor model.image

        strokeColor_ =
            LDraw.getStrokeColor model.image

        turnAngle_ =
            LDraw.getTurnAngle model.image
    in
    fixedDiv
        [ css
            [ height (pct 100)
            , width (pct layout.controlPanel)
            , right zero
            , boxSizing borderBox
            , borderLeft3 (px 1) solid (toCssColor Colors.black)
            , overflowY scroll
            , overflowX hidden
            , backgroundColor (toCssColor backgroundColor_)
            , color (toCssColor offWhite)
            ]
        ]
        [ infoAndBasicControls model
        , colorControls backgroundColor_ strokeColor_
        , videoControls model.playingVideo
        , turnAngleControl turnAngle_
        , curatedSettings
        , controlsList
        ]


infoAndBasicControls : Model -> Html Msg
infoAndBasicControls model =
    let
        stateLengthString =
            Debug.toString (LCore.stepsLength (LDraw.getComposition model.image))

        editingTransformBlueprint =
            case LCore.getBlockAtIndex model.editingIndex (LDraw.getComposition model.image) of
                Nothing ->
                    ""

                Just block ->
                    LCore.blockToString block
    in
    controlBlock
        [ button [ onClick (ViewingPage GalleryPage) ] [ text "Go to Gallery" ]
        , button [ onClick ResetDrawing ] [ text "ResetDrawing" ]
        , button [ onClick (Iterate model.editingIndex) ] [ text "Iterate" ]
        , button [ onClick Deiterate ] [ text "Deiterate" ]
        , p [] [ text stateLengthString ]
        , p [] [ text editingTransformBlueprint ]
        , button [ onClick DownloadSvg ] [ text "Download Image" ]
        , button [ onClick SavedToGallery ] [ text "Save to Gallery" ]
        ]


colorControls : Color -> Color -> Html Msg
colorControls backgroundColor strokeColor =
    let
        colorControl inputId msg inputLabel color =
            div []
                [ label [ for inputId ] [ text inputLabel ]
                , input
                    [ type_ "color"
                    , id inputId
                    , css [ display block ]
                    , onInput (Colors.fromHexString >> msg)
                    , value (Colors.toHexString color)
                    ]
                    []
                ]
    in
    controlBlock
        [ colorControl "BgColor" SetBackgroundColor "Change background color" backgroundColor
        , br [] []
        , colorControl "StrokeColor" SetStrokeColor "Change stroke color" strokeColor
        ]


videoControls : Bool -> Html Msg
videoControls playingVideo =
    controlBlock
        [ span [ css [ display block ] ] [ text "Video Playback" ]
        , button [ onClick TogglePlayingVideo ]
            [ text
                (if playingVideo then
                    "Stop"

                 else
                    "Play"
                )
            ]
        , button [ onClick VideoSpeedFaster ] [ text "Faster" ]
        , button [ onClick VideoSpeedSlower ] [ text "Slower" ]
        , button [ onClick ReverseAngleChangeDirection ] [ text "Reverse" ]
        ]


turnAngleControl : Float -> Html Msg
turnAngleControl turnAngle =
    let
        onInputCallback stringValue =
            if stringValue == "" then
                SetTurnAngle 0

            else
                case String.toFloat stringValue of
                    Just degrees ->
                        SetTurnAngle degrees

                    Nothing ->
                        SetTurnAngle turnAngle
    in
    controlBlock
        [ label [ for "TurnAngle" ] [ text "Change angle" ]
        , input
            [ id "TurnAngle" -- See index.js, `id` only exists for use in there.
            , type_ "number"
            , value (String.fromFloat turnAngle)
            , css [ display block, width (px 50) ]
            , onInput onInputCallback
            , onFocus (SetFocus TurnAngleInput)
            , onBlur (SetFocus KeyboardEditing)
            ]
            []
        ]


curatedSettings : Html Msg
curatedSettings =
    controlBlock
        [ span [ css [ display block ] ] [ text "Change Angle and Base" ]
        , button [ onClick (BasePolygonChanged Triangle) ] [ text "Triangle" ]
        , button [ onClick (BasePolygonChanged Square) ] [ text "Square" ]
        , button [ onClick (BasePolygonChanged Pentagon) ] [ text "Pentagon" ]
        , button [ onClick (BasePolygonChanged Hexagon) ] [ text "Hexagon" ]
        ]


controlsList : Html Msg
controlsList =
    controlBlock
        [ h2 [] [ text "Controls" ]
        , controlText "Arrow Up" "Draw"
        , controlText "Arrow Left/Right" "Turn"
        , controlText "Backspace" "Undo last 'Arrow' move on selected image block"
        , controlText "i" "Undo last 'Arrow' move on selected image block"
        , controlText "d" "Delete first image block"
        , controlText "Space" "Center the image again"
        , br [] []
        , p [] [ text "You can also pan and zoom by dragging and scrolling the image" ]
        ]


controlText : String -> String -> Html msg
controlText command explanation =
    div [ css [ margin (px 10) ] ]
        [ span [ css [ fontSize (px 14), backgroundColor (Css.hex "#555") ] ] [ text command ]
        , text <| ": " ++ explanation
        ]


controlBlock : List (Html Msg) -> Html Msg
controlBlock =
    div [ css [ padding (px 10), borderBottom3 (px 1) solid (toCssColor Colors.black), fontFamily Css.monospace ] ]


transformsList : Model -> Html Msg
transformsList model =
    let
        transforms =
            LDraw.splitIntoBlockImages model.image
                |> List.indexedMap (transformBox model.editingIndex)
                |> List.reverse

        backgroundColor_ =
            LDraw.getBackgroundColor model.image
    in
    fixedDiv
        [ css
            [ backgroundColor (toCssColor backgroundColor_)
            , height (pct 100)
            , width (pct layout.transformsList)
            , overflow scroll
            , boxSizing borderBox
            , borderRight3 (px 1) solid (toCssColor Colors.black)
            ]
        ]
        (h2
            [ css
                [ color (toCssColor Colors.offWhite)
                , fontFamily Css.monospace
                ]
            ]
            [ text "Image Blocks" ]
            :: transforms
        )


transformBox : Int -> Int -> Image Msg -> Html Msg
transformBox editingIndex index image =
    div
        [ css
            [ height (px 200)
            , width (pct 100)
            , borderBottom3 (px 1) solid (toCssColor Colors.black)
            ]
        ]
        [ drawImage image
        , Icons.trash
            |> withColor Colors.red_
            |> withOnClick (DropFromState index)
            |> Icons.toSvg
        , Icons.pen
            |> withConditionalColor (index == editingIndex) Colors.green_
            |> withOnClick (SetEditingIndex index)
            |> Icons.toSvg
        ]


mainImg : Model -> Html Msg
mainImg model =
    let
        backgroundColor_ =
            LDraw.getBackgroundColor model.image
    in
    fixedDiv
        [ css
            [ backgroundColor (toCssColor backgroundColor_)
            , position fixed
            , height (pct 100)
            , width (pct layout.mainImg)
            , left (pct layout.transformsList)
            , overflow hidden
            ]
        , id "mainImg"
        , zoomOnWheel
        , on "mousedown" (mousePositionDecoder PanStarted)
        ]
        [ model.image
            |> withId "MainSVG"
            |> drawImage
        ]


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


fixedDiv : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
fixedDiv attrs children =
    div
        (css
            [ position fixed ]
            :: attrs
        )
        children



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAndSaveImageAndGallery newModel =
            ( newModel
            , Cmd.batch
                [ saveStateToLocalStorage (encodeModel newModel)
                , replaceUrl newModel.navKey newModel.image
                ]
            )
    in
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.replaceUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            -- Called via replaceUrl (and indirectly via click on internal links/href, see LinkClicked above)
            ( { model | viewingPage = mapRouteToPage (parseUrl url) }, Cmd.none )

        DownloadSvg ->
            ( model, downloadSvg () )

        ResetDrawing ->
            let
                newComposition =
                    LDraw.getComposition model.image
                        |> LCore.dropAllBlocksButBase
                        |> LCore.appendBlock [ D ]
            in
            updateAndSaveImageAndGallery
                { model
                    | image = withComposition newComposition model.image
                    , editingIndex = 1
                }

        Iterate editingIndex ->
            updateAndSaveImageAndGallery <| iterate model editingIndex

        Deiterate ->
            updateAndSaveImageAndGallery <| deiterate model

        KeyPress keyString ->
            updateAndSaveImageAndGallery <| processKey model keyString

        -- also, see `scaleAbout` in https://github.com/ianmackenzie/elm-geometry-svg/blob/master/src/Geometry/Svg.elm
        -- and later check https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/
        Zoom _ deltaY _ mousePos ->
            updateAndSaveImageAndGallery <| applyZoom deltaY mousePos model

        SetEditingIndex index ->
            ( { model | editingIndex = index }, Cmd.none )

        DropFromState index ->
            updateAndSaveImageAndGallery <|
                { model
                    | image = LDraw.mapComposition (LCore.dropBlockAtIndex index) model.image
                    , editingIndex =
                        if model.editingIndex >= index then
                            model.editingIndex - 1

                        else
                            model.editingIndex
                }

        SetBackgroundColor color ->
            updateAndSaveImageAndGallery <| { model | image = withBackgroundColor color model.image }

        SetStrokeColor color ->
            updateAndSaveImageAndGallery <| { model | image = withStrokeColor color model.image }

        SetTurnAngle turn ->
            let
                newModel =
                    { model | image = withTurnAngle turn model.image }
            in
            if model.playingVideo then
                -- Don't update URL and Local Storage on each video step
                ( newModel, Cmd.none )

            else
                updateAndSaveImageAndGallery newModel

        PanStarted pos ->
            ( { model | panStarted = True, lastPos = pos }, Cmd.none )

        PanEnded ->
            updateAndSaveImageAndGallery <| { model | panStarted = False }

        MouseMoved (( px, py ) as pos) ->
            let
                ( lx, ly ) =
                    model.lastPos

                updatePosition oldPos =
                    { x = px - lx + oldPos.x
                    , y = py - ly + oldPos.y
                    }
            in
            ( { model
                | image = LDraw.mapTranslate updatePosition model.image
                , lastPos = pos
              }
            , Cmd.none
            )

        SetFocus focus ->
            ( { model | focus = focus }, Cmd.none )

        BasePolygonChanged polygon ->
            updateAndSaveImageAndGallery <| { model | image = LDraw.updateDefiningPolygon polygon model.image }

        GotImgDivPosition result ->
            case result of
                Ok data ->
                    let
                        { x, y, width, height } =
                            data.element
                    in
                    ( { model | imgDivCenter = ( x + width / 2, y + height / 2 ), imgDivStart = ( x, y ) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        TogglePlayingVideo ->
            let
                newModel =
                    { model | playingVideo = not model.playingVideo }
            in
            if model.playingVideo then
                -- When video stopped save model in URL and Local Storage
                -- N.B. Copying the URL while a playing video will copy the last image saved
                -- i.e. before playing video or the last not-angle edit while playing.
                updateAndSaveImageAndGallery <| newModel

            else
                ( newModel, Cmd.none )

        VideoSpeedFaster ->
            ( { model | videoAngleChangeRate = min 1 (model.videoAngleChangeRate * 1.4) }, Cmd.none )

        VideoSpeedSlower ->
            ( { model | videoAngleChangeRate = max 0.001 (model.videoAngleChangeRate / 1.4) }, Cmd.none )

        ReverseAngleChangeDirection ->
            ( { model | videoAngleChangeDirection = model.videoAngleChangeDirection * -1 }, Cmd.none )

        SavedToGallery ->
            updateAndSaveImageAndGallery <| { model | gallery = model.image :: model.gallery }

        ViewingPage page ->
            ( { model | viewingPage = page }, Cmd.none )

        RemovedFromGallery index ->
            updateAndSaveImageAndGallery <| { model | gallery = ListExtra.dropIndex index model.gallery }

        DuplicateToEdit index ->
            let
                newModel =
                    { model
                        | image =
                            ListExtra.getAt index model.gallery
                                |> Maybe.withDefault model.image
                    }
            in
            updateAndSaveImageAndGallery <| { newModel | viewingPage = EditorPage }


processKey : Model -> String -> Model
processKey model keyPressed =
    let
        appendStep step =
            { model | image = LDraw.appendStepAtIndex step model.editingIndex model.image }
    in
    case keyPressed of
        "ArrowLeft" ->
            appendStep L

        "ArrowRight" ->
            appendStep R

        "ArrowUp" ->
            appendStep D

        "ArrowDown" ->
            appendStep S

        " " ->
            { model
                | image =
                    model.image
                        |> withScale 1
                        |> withTranslate { x = 0, y = 0 }
            }

        "Backspace" ->
            { model
                | image =
                    model.image
                        |> withComposition (LCore.dropLastStepAtIndex model.editingIndex (LDraw.getComposition model.image))
            }

        "i" ->
            iterate model model.editingIndex

        "d" ->
            deiterate model

        _ ->
            model


iterate : Model -> Int -> Model
iterate model editingIndex =
    { model | image = LDraw.mapComposition (LCore.duplicateBlockAndAppendAsLast editingIndex) model.image }
        |> modelWithEditIndexLast


deiterate : Model -> Model
deiterate model =
    let
        updatedComposition =
            LCore.dropLastBlock (LDraw.getComposition model.image)
    in
    { model
        | image = withComposition updatedComposition model.image
        , editingIndex = min model.editingIndex (LCore.length updatedComposition - 1)
    }


applyZoom : Float -> Position -> Model -> Model
applyZoom deltaY mousePos model =
    let
        modelScale =
            LDraw.getScale model.image

        scale =
            max (modelScale - 0.01 * deltaY) 0.1

        vecMouseToImgDivCenter =
            model.imgDivCenter
                |> pairExec (-) mousePos

        modelTranslate =
            model.image
                |> LDraw.getTranslate

        modelTranslateAsPair =
            ( modelTranslate.x, modelTranslate.y )

        translate =
            vecMouseToImgDivCenter
                |> pairExec (+) modelTranslateAsPair
                |> pairMap (\value -> value * scale / modelScale)
                |> pairExec (-) vecMouseToImgDivCenter

        translateAsRecord =
            { x = Tuple.first translate
            , y = Tuple.second translate
            }
    in
    { model | image = model.image |> withScale scale |> withTranslate translateAsRecord }



-- COMMANDS


getImgDivPosition : Cmd Msg
getImgDivPosition =
    Task.attempt GotImgDivPosition (Browser.Dom.getElement "mainImg")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        panSubs =
            if model.panStarted then
                Sub.batch
                    [ Browser.Events.onMouseMove mouseMoveDecoder
                    , Browser.Events.onMouseUp (Decode.succeed PanEnded)
                    ]

            else
                Sub.none

        keyPressSub =
            if model.focus == KeyboardEditing then
                Browser.Events.onKeyUp keyPressDecoder

            else
                Sub.none

        playingVideoSub =
            if model.playingVideo then
                Time.every 50 (always (SetTurnAngle (LDraw.getTurnAngle model.image + (model.videoAngleChangeDirection * model.videoAngleChangeRate))))

            else
                Sub.none
    in
    Sub.batch [ keyPressSub, panSubs, playingVideoSub ]



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


galleryParser : Parser (Route -> a) a
galleryParser =
    Parser.map Gallery (Parser.s "gallery")


editorParser : Parser (Route -> a) a
editorParser =
    Parser.map Editor LDraw.parserWithDefault


replaceUrl : Nav.Key -> Image Msg -> Cmd msg
replaceUrl key image =
    {--
            Note about Nav.replaceUrl: Browsers may rate-limit this function by throwing an
            exception. The discussion here suggests that the limit is 100 calls per 30 second
            interval in Safari in 2016. It also suggests techniques for people changing the
            URL based on scroll position.

            https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl
        --}
    Nav.replaceUrl key (LDraw.toUrlString image)



-- MODEL ENCODER AND DECODER


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "image", LDraw.encode model.image )
        , ( "gallery", Encode.list LDraw.encode model.gallery )
        ]


imageAndGalleryDecoder : Decoder ImageAndGallery
imageAndGalleryDecoder =
    Decode.map2 ImageAndGallery
        (Decode.field "image" LDraw.decoder)
        (Decode.field "gallery" (Decode.list LDraw.decoder))



-- KEYBOARD, MOUSE and WHEEL


keyPressDecoder : Decoder Msg
keyPressDecoder =
    Decode.map KeyPress
        (Decode.field "key" Decode.string)


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    mousePositionDecoder MouseMoved


mousePositionDecoder : (Position -> msg) -> Decoder msg
mousePositionDecoder msg =
    Decode.map msg <|
        Decode.map2 Tuple.pair
            (Decode.field "clientX" Decode.float)
            (Decode.field "clientY" Decode.float)


zoomOnWheel : Html.Styled.Attribute Msg
zoomOnWheel =
    preventDefaultOn "wheel" (Decode.map alwaysPreventDefault wheelDecoder)


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


wheelDecoder : Decoder Msg
wheelDecoder =
    Decode.map4 Zoom
        (Decode.field "deltaX" Decode.float)
        (Decode.field "deltaY" Decode.float)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.map2 Tuple.pair
            (Decode.field "clientX" Decode.float)
            (Decode.field "clientY" Decode.float)
        )
