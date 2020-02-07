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
        ( drawImage
        , image
        , withBackgroundColor
        , withId
        , withScale
        , withStrokeColor
        , withTranslation
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


type alias Model =
    -- Aplication heart
    { composition : Composition
    , editingIndex : Int

    -- Storage
    , gallery : Gallery
    , viewingPage : Page

    -- Pan and Zoom
    , scale : Float
    , panStarted : Bool
    , lastPos : Position
    , translate : Position

    -- Main Image Div Coordinates
    , imgDivCenter : Position
    , imgDivStart : Position

    -- Color
    , backgroundColor : Color
    , strokeColor : Color

    -- Angle
    , turnAngle : Float

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
    = EditPage
    | GalleryPage


type Route
    = Home UrlDataVersions
    | Gallery
    | NotFound


type alias UrlDataVersions =
    { v1_dataOnQueryParams : Maybe ImageEssentials
    , v0_dataOnHash : Maybe Composition
    }


mapRouteToPage : Route -> Page
mapRouteToPage route =
    -- TODO think about this flow that requires this function. It doesn't seem the best one.
    case route of
        Gallery ->
            GalleryPage

        _ ->
            EditPage



-- IMAGE ESSENTIALS


type alias ImageEssentials =
    { composition : Composition
    , turnAngle : Float
    , backgroundColor : Color
    , strokeColor : Color
    , translate : Position
    , scale : Float
    }


type alias Gallery =
    List ImageEssentials


type alias ImageAndGallery =
    { image : ImageEssentials
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


type Polygon
    = Triangle
    | Square
    | Pentagon
    | Hexagon


type alias Position =
    ( Float, Float )



-- IMPLEMENTATION, LOGIC, FUNCTIONS
-- MODEL


initialImage : ImageEssentials
initialImage =
    { composition = LCore.fromList [ polygonBlock Square, [ D ] ]
    , turnAngle = polygonAngle Square
    , backgroundColor = Colors.darkGray
    , strokeColor = Colors.defaultGreen
    , translate = ( 0, 0 )
    , scale = 1
    }


replaceComposition : ImageEssentials -> Composition -> ImageEssentials
replaceComposition image composition =
    { image | composition = composition }


initialModel : ImageEssentials -> Gallery -> Url.Url -> Nav.Key -> Model
initialModel image gallery url navKey =
    -- Aplication heart
    { composition = image.composition
    , editingIndex = 1

    -- Storage
    , gallery = gallery
    , viewingPage = EditPage

    -- Pan and Zoom
    , scale = image.scale
    , panStarted = False
    , lastPos = ( 0, 0 )
    , translate = image.translate

    -- Main Image Div Coordinates
    , imgDivCenter = ( 0, 0 )
    , imgDivStart = ( 0, 0 )

    -- Color
    , backgroundColor = image.backgroundColor
    , strokeColor = image.strokeColor

    -- Angle
    , turnAngle = image.turnAngle

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


modelToImage : Model -> ImageEssentials
modelToImage model =
    { composition = model.composition
    , turnAngle = model.turnAngle
    , backgroundColor = model.backgroundColor
    , strokeColor = model.strokeColor
    , translate = model.translate
    , scale = model.scale
    }


modelWithImage : ImageEssentials -> Model -> Model
modelWithImage { composition, turnAngle, backgroundColor, strokeColor } model =
    { model
        | composition = composition
        , turnAngle = turnAngle
        , backgroundColor = backgroundColor
        , strokeColor = strokeColor
    }


modelWithRoute : Route -> Model -> Model
modelWithRoute route model =
    { model | viewingPage = mapRouteToPage route }


modelWithEditIndexLast : Model -> Model
modelWithEditIndexLast model =
    { model | editingIndex = LCore.length model.composition - 1 }


urlDataVersionsToImage : UrlDataVersions -> Maybe ImageEssentials
urlDataVersionsToImage dataVersions =
    if isJust dataVersions.v1_dataOnQueryParams then
        dataVersions.v1_dataOnQueryParams

    else if isJust dataVersions.v0_dataOnHash then
        -- Migrating from old URL format
        Maybe.map (replaceComposition initialImage) dataVersions.v0_dataOnHash

    else
        Nothing



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


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False



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
                |> modelWithRoute (Debug.log "init route" route)
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

        maybeImageFromUrl =
            case route of
                Home dataVersions ->
                    urlDataVersionsToImage dataVersions

                _ ->
                    Nothing
    in
    case ( maybeImageFromUrl, resultImageAndGalleryFromStorage ) of
        ( Just urlImage, Ok storedImageAndGallery ) ->
            { image = urlImage, gallery = storedImageAndGallery.gallery }

        ( Just urlImage, Err _ ) ->
            { image = urlImage, gallery = [] }

        ( Nothing, Ok storedImageAndGallery ) ->
            storedImageAndGallery

        ( Nothing, Err _ ) ->
            { image = initialImage, gallery = [] }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case Debug.log "route" model.viewingPage of
        EditPage ->
            editView model

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
                [ button [ onClick (ViewingPage EditPage), css [ margin (px 10) ] ] [ text "Back to Edit" ] ]
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


imageBox : Int -> ImageEssentials -> Html Msg
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
        [ LDraw.image image.composition
            |> withTurnAngle image.turnAngle
            |> withStrokeColor image.strokeColor
            |> withBackgroundColor image.backgroundColor
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



-- EDIT VIEW


editView : Model -> Browser.Document Msg
editView model =
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
    fixedDiv
        [ css
            [ height (pct 100)
            , width (pct layout.controlPanel)
            , right zero
            , boxSizing borderBox
            , borderLeft3 (px 1) solid (toCssColor Colors.black)
            , overflowY scroll
            , overflowX hidden
            , backgroundColor (toCssColor model.backgroundColor)
            , color (toCssColor offWhite)
            ]
        ]
        [ infoAndBasicControls model
        , colorControls model.backgroundColor model.strokeColor
        , videoControls model.playingVideo
        , turnAngleControl model.turnAngle
        , curatedSettings
        , controlsList
        ]


infoAndBasicControls : Model -> Html Msg
infoAndBasicControls model =
    let
        stateLengthString =
            Debug.toString (LCore.stepsLength model.composition)

        editingTransformBlueprint =
            case LCore.getBlockAtIndex model.editingIndex model.composition of
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
            model.composition
                |> LCore.toList
                |> List.indexedMap
                    (transformBox
                        model.editingIndex
                        model.turnAngle
                        model.strokeColor
                        model.backgroundColor
                    )
                |> List.reverse
    in
    fixedDiv
        [ css
            [ backgroundColor (toCssColor model.backgroundColor)
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


transformBox : Int -> Float -> Color -> Color -> Int -> Block -> Html Msg
transformBox editingIndex turnAngle strokeColor backgroundColor index transform =
    div
        [ css
            [ height (px 200)
            , width (pct 100)
            , borderBottom3 (px 1) solid (toCssColor Colors.black)
            ]
        ]
        [ image (LCore.fromList [ transform ])
            |> withTurnAngle turnAngle
            |> withStrokeColor strokeColor
            |> withBackgroundColor backgroundColor
            |> drawImage
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
    fixedDiv
        [ css
            [ backgroundColor (toCssColor model.backgroundColor)
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
        [ image model.composition
            |> withTurnAngle model.turnAngle
            |> withStrokeColor model.strokeColor
            |> withBackgroundColor model.backgroundColor
            |> withScale model.scale
            |> withTranslation model.translate
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
                , replaceUrl newModel.navKey (modelToImage newModel)
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
            updateAndSaveImageAndGallery
                { model
                    | composition = model.composition |> LCore.dropAllBlocksButBase |> LCore.appendBlock [ D ]
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
                    | composition = LCore.dropBlockAtIndex index model.composition
                    , editingIndex =
                        if model.editingIndex >= index then
                            model.editingIndex - 1

                        else
                            model.editingIndex
                }

        SetBackgroundColor color ->
            updateAndSaveImageAndGallery <| { model | backgroundColor = color }

        SetStrokeColor color ->
            updateAndSaveImageAndGallery <| { model | strokeColor = color }

        SetTurnAngle turn ->
            let
                newModel =
                    { model | turnAngle = turn }
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

        MouseMoved pos ->
            ( { model
                | translate =
                    pos
                        |> pairExec (-) model.lastPos
                        |> pairExec (+) model.translate
                , lastPos = pos
              }
            , Cmd.none
            )

        SetFocus focus ->
            ( { model | focus = focus }, Cmd.none )

        BasePolygonChanged polygon ->
            updateAndSaveImageAndGallery <| updateCompositionBaseAndAngle model polygon

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
            updateAndSaveImageAndGallery <| { model | gallery = modelToImage model :: model.gallery }

        ViewingPage page ->
            ( { model | viewingPage = page }, Cmd.none )

        RemovedFromGallery index ->
            updateAndSaveImageAndGallery <| { model | gallery = ListExtra.dropIndex index model.gallery }

        DuplicateToEdit index ->
            let
                image =
                    ListExtra.getAt index model.gallery
                        |> Maybe.withDefault (modelToImage model)

                newModel =
                    modelWithImage image model
            in
            updateAndSaveImageAndGallery <| { newModel | viewingPage = EditPage }


processKey : Model -> String -> Model
processKey model keyPressed =
    let
        appendStep step =
            { model | composition = LCore.appendStepAtIndex step model.editingIndex model.composition }
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
            { model | scale = 1, translate = ( 0, 0 ) }

        "Backspace" ->
            { model | composition = LCore.dropLastStepAtIndex model.editingIndex model.composition }

        "i" ->
            iterate model model.editingIndex

        "d" ->
            deiterate model

        _ ->
            model


iterate : Model -> Int -> Model
iterate model editingIndex =
    let
        updatedComposition =
            LCore.duplicateBlockAndAppendAsLast editingIndex model.composition
    in
    { model
        | composition = updatedComposition
        , editingIndex = LCore.length updatedComposition - 1
    }


deiterate : Model -> Model
deiterate model =
    let
        updatedComposition =
            LCore.dropLastBlock model.composition
    in
    { model
        | composition = updatedComposition
        , editingIndex = min model.editingIndex (LCore.length updatedComposition - 1)
    }


applyZoom : Float -> Position -> Model -> Model
applyZoom deltaY mousePos model =
    let
        scale =
            max (model.scale - 0.01 * deltaY) 0.1

        vecMouseToImgDivCenter =
            model.imgDivCenter
                |> pairExec (-) mousePos
    in
    { model
        | scale = scale
        , translate =
            vecMouseToImgDivCenter
                |> pairExec (+) model.translate
                |> pairMap (\value -> value * scale / model.scale)
                |> pairExec (-) vecMouseToImgDivCenter
    }


updateCompositionBaseAndAngle : Model -> Polygon -> Model
updateCompositionBaseAndAngle model polygon =
    { model
        | composition = LCore.changeBase (polygonBlock polygon) model.composition
        , turnAngle = polygonAngle polygon
    }



-- POLYGON


polygonBlock : Polygon -> Block
polygonBlock polygon =
    case polygon of
        Triangle ->
            [ D, L, D, L, D ]

        Square ->
            [ D, L, D, L, D, L, D ]

        Pentagon ->
            [ D, L, D, L, D, L, D, L, D ]

        Hexagon ->
            [ D, L, D, L, D, L, D, L, D, L, D ]


polygonAngle : Polygon -> Float
polygonAngle polygon =
    case polygon of
        Triangle ->
            120

        Square ->
            90

        Pentagon ->
            72

        Hexagon ->
            60



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
                Time.every 50 (always (SetTurnAngle (model.turnAngle + (model.videoAngleChangeDirection * model.videoAngleChangeRate))))

            else
                Sub.none
    in
    Sub.batch [ keyPressSub, panSubs, playingVideoSub ]



-- URL


parseUrl : Url.Url -> Route
parseUrl url =
    let
        parsedRoute =
            Parser.parse (Parser.oneOf [ homeParser, galleryParser ]) url
    in
    case parsedRoute of
        Just route ->
            route

        Nothing ->
            NotFound


galleryParser : Parser (Route -> a) a
galleryParser =
    Parser.map Gallery (Parser.s "gallery")


homeParser : Parser (Route -> a) a
homeParser =
    Parser.map Home
        (Parser.map
            UrlDataVersions
            -- Fragment is parsed for backward-compatibility only
            (Parser.query queryToImage </> fragmentToCompositionParser)
        )


{-| Current version of Url building and parsing
-}
queryToImage : Query.Parser (Maybe ImageEssentials)
queryToImage =
    let
        queryMapFromDecoder decoder =
            Query.map (Maybe.andThen (Result.toMaybe << Decode.decodeString decoder))
    in
    Query.map6 (ListExtra.maybeMap6 ImageEssentials)
        (Query.string "composition" |> queryMapFromDecoder LCore.compositionDecoder)
        (Query.string "angle" |> Query.map (Maybe.andThen String.toFloat))
        (Query.string "bg" |> queryMapFromDecoder Colors.decoder)
        (Query.string "stroke" |> queryMapFromDecoder Colors.decoder)
        (Query.map2 (Maybe.map2 Tuple.pair)
            (Query.string "x" |> Query.map (Maybe.andThen String.toFloat))
            (Query.string "y" |> Query.map (Maybe.andThen String.toFloat))
        )
        (Query.string "scale" |> Query.map (Maybe.andThen String.toFloat))


imageToUrlString : ImageEssentials -> String
imageToUrlString image =
    Url.Builder.absolute []
        [ Url.Builder.string "composition" (image.composition |> LCore.encodeComposition |> Encode.encode 0)
        , Url.Builder.string "angle" (String.fromFloat image.turnAngle)
        , Url.Builder.string "bg" (image.backgroundColor |> Colors.encode |> Encode.encode 0)
        , Url.Builder.string "stroke" (image.strokeColor |> Colors.encode |> Encode.encode 0)
        , Url.Builder.string "x" (Tuple.first image.translate |> Encode.float |> Encode.encode 0)
        , Url.Builder.string "y" (Tuple.second image.translate |> Encode.float |> Encode.encode 0)
        , Url.Builder.string "scale" (image.scale |> Encode.float |> Encode.encode 0)
        ]


replaceUrl : Nav.Key -> ImageEssentials -> Cmd msg
replaceUrl key image =
    {--
            Note about Nav.replaceUrl: Browsers may rate-limit this function by throwing an
            exception. The discussion here suggests that the limit is 100 calls per 30 second
            interval in Safari in 2016. It also suggests techniques for people changing the
            URL based on scroll position.

            https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl
        --}
    Nav.replaceUrl key (imageToUrlString image)


{-| Backwards compatibility: Old version of Url building and parsing.
-}
fragmentToCompositionParser : Parser (Maybe Composition -> a) a
fragmentToCompositionParser =
    Parser.fragment <|
        Maybe.andThen
            (Url.percentDecode
                >> Maybe.andThen (Decode.decodeString LCore.compositionDecoder >> Result.toMaybe)
            )



-- MODEL ENCODER AND DECODER


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "image", encodeImage (modelToImage model) )
        , ( "gallery", Encode.list encodeImage model.gallery )
        ]


imageAndGalleryDecoder : Decoder ImageAndGallery
imageAndGalleryDecoder =
    Decode.map2 ImageAndGallery
        (Decode.field "image" imageDecoder)
        (Decode.field "gallery" (Decode.list imageDecoder))


imageDecoder : Decoder ImageEssentials
imageDecoder =
    Decode.map6 ImageEssentials
        (Decode.field "composition" LCore.compositionDecoder)
        (Decode.field "turnAngle" Decode.float)
        (Decode.field "bgColor" Colors.decoder)
        (Decode.field "strokeColor" Colors.decoder)
        (Decode.map2 Tuple.pair
            (Decode.field "translateX" Decode.float)
            (Decode.field "translateY" Decode.float)
        )
        (Decode.field "scale" Decode.float)


encodeImage : ImageEssentials -> Encode.Value
encodeImage { composition, turnAngle, backgroundColor, strokeColor, translate, scale } =
    Encode.object
        [ ( "composition", LCore.encodeComposition composition )
        , ( "turnAngle", Encode.float turnAngle )
        , ( "bgColor", Colors.encode backgroundColor )
        , ( "strokeColor", Colors.encode strokeColor )
        , ( "translateX", Encode.float (Tuple.first translate) )
        , ( "translateY", Encode.float (Tuple.second translate) )
        , ( "scale", Encode.float scale )
        ]



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
