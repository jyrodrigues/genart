-- Anything other than `main` is exposed for testing only
-- TODO research how we could change this in order to only expose `main`
-- Question: Should Main module be tested? Should every test-deserving function live
--           outside Main module?


port module Main exposing
    ( Route(..)
    , encodeModel
    , main
    , modelDecoder
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
        , active
        , auto
        , backgroundColor
        , backgroundImage
        , block
        , border
        , border3
        , borderBottom3
        , borderBox
        , borderLeft3
        , borderRadius
        , borderRight3
        , bottom
        , boxShadow5
        , boxShadow6
        , boxSizing
        , breakWord
        , calc
        , center
        , color
        , contentBox
        , cursor
        , default
        , display
        , displayFlex
        , fixed
        , flexWrap
        , fontFamily
        , fontSize
        , height
        , hidden
        , hover
        , inlineBlock
        , inset
        , left
        , lineHeight
        , linearGradient2
        , margin
        , margin2
        , margin3
        , marginBottom
        , marginTop
        , maxWidth
        , minWidth
        , minus
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
        , sansSerif
        , scroll
        , solid
        , stop
        , textAlign
        , textDecoration
        , toRight
        , transparent
        , unset
        , vw
        , width
        , wrap
        , zero
        )
import Css.Global exposing (body, global)
import Css.Media as Media exposing (withMedia)
import Html.Styled exposing (Html, a, br, button, div, h2, h3, input, label, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css, for, href, id, type_, value)
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
import ListExtra
import Midi exposing (adjustInputForStrokeWidth)
import Process
import Random
import Svg.Styled exposing (Svg)
import Task
import Time
import Url
import Url.Parser as Parser exposing (Parser, string)
import Utils exposing (delay)



-- PORTS


port saveEncodedModelToLocalStorage : Encode.Value -> Cmd msg


port downloadSvg : () -> Cmd msg


port downloadSvgAsJpeg : () -> Cmd msg


port requestFullscreen : () -> Cmd msg


port midiEvent : (Encode.Value -> msg) -> Sub msg



-- TYPES
-- MSG


{-| TODO: Rename Msgs: put all verbs in past tense and choose better words.
-}
type
    Msg
    -- Global keyboard listener
    = EditorKeyPress String
    | InputKeyPress String
      -- Main commands
    | AddSimpleBlock
    | ResetDrawing
    | BasePolygonChanged Polygon
    | DuplicateAndAppendBlock Int
    | SetEditingIndex Int
    | DropBlock Int
      -- Storage
    | SavedToGallery
    | RemovedFromGallery Int
    | DuplicateToEdit Int
      -- Pan and Zoom
    | GotImgDivPosition (Result Browser.Dom.Error Element)
    | WindowResized Int Int
    | PanStarted Position
    | PanEnded
    | MouseMoved Position
    | Zoom Float Float ShiftKey Position
      -- Colors
    | SetColorTarget ColorTarget
    | ColorWheelMsg ColorWheel.Msg
      -- Angle
    | SetTurnAngle Float
    | SetTurnAngleInputValue String
      -- Stroke width
    | SetStrokeWidth Float
      -- Download
    | DownloadSvg
    | DownloadSvgAsJpeg
      -- Focus
    | SetFocus Focus
      -- URL
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
      -- Video
    | TogglePlayingVideo
    | ToggleSlowMotion
    | SetVideoAngleChangeRate Float
    | ReverseAngleChangeDirection
      -- Fullscreen
    | FullscreenRequested
      -- Random
    | RandomRequested
    | GotRandomImage PartialImage
      -- MIDI
    | GotMidiEvent Encode.Value
      -- Alert popup
    | HideAlert
      -- Sometimes there is nothing to do
    | NoOp



-- MODEL


type alias Model =
    -- Aplication heart
    { image : Image
    , editingIndex : Int

    -- Storage
    , gallery : List Image
    , viewingPage : Page

    -- Pan and Zoom
    , panStarted : Bool
    , lastPos : Position

    -- Main Image Div Coordinates
    , imgDivCenter : Position
    , imgDivStart : Position

    -- ColorWheel
    , colorWheel : ColorWheel.Model
    , colorTarget : ColorTarget

    -- Browser Focus
    , focus : Focus

    -- Url
    , url : Url.Url
    , navKey : Nav.Key

    -- Video
    , videoAngleChangeRate : Float
    , slowMotion : SlowMotion
    , playingVideo : Bool
    , videoAngleChangeDirection : Float

    -- Input controls value
    , turnAngleInputValue : String

    -- Alert Popup
    , alertActive : Bool
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



-- OTHER TYPES


type alias ShiftKey =
    Bool


type Focus
    = EditorFocus
    | TurnAngleInputFocus


type SlowMotion
    = NotSet
    | Slowly Float


type ColorTarget
    = Stroke
    | Background



-- IMPLEMENTATION, LOGIC, FUNCTIONS
-- MODEL


initialModel : Image -> List Image -> Url.Url -> Nav.Key -> Model
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

    -- ColorWheel
    , colorWheel =
        ColorWheel.initialModel "ColorWheel1"
            |> ColorWheel.trackMouseOutsideWheel True
            |> ColorWheel.withColor image.strokeColor
    , colorTarget = Stroke

    -- Browser Focus
    , focus = EditorFocus

    -- Url
    , url = url
    , navKey = navKey

    -- Video
    , videoAngleChangeRate = 0.01
    , slowMotion = NotSet
    , playingVideo = False
    , videoAngleChangeDirection = 1

    -- Input controls value
    , turnAngleInputValue = String.fromFloat image.turnAngle

    -- Alert Popup
    , alertActive = False
    }


modelWithRoute : Route -> Model -> Model
modelWithRoute route model =
    { model | viewingPage = mapRouteToPage route }


modelWithEditIndexLast : Model -> Model
modelWithEditIndexLast model =
    { model | editingIndex = Image.length model.image - 1 }



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
        decodedLocalStorage =
            Decode.decodeValue modelDecoder localStorage

        route =
            parseUrl url

        ( image, gallery ) =
            combineUrlAndStorage decodedLocalStorage route

        model =
            initialModelFromImageAndGallery image gallery url navKey
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


combineUrlAndStorage : Result Decode.Error ( Image, List Image ) -> Route -> ( Image, List Image )
combineUrlAndStorage storage route =
    case ( route, storage ) of
        ( Editor urlImage, Ok ( storedImage, storedGallery ) ) ->
            ( storedImage |> withImage urlImage, storedGallery )

        ( Editor urlImage, Err _ ) ->
            ( defaultImage |> withImage urlImage, [] )

        ( _, Ok storedImageAndGallery ) ->
            storedImageAndGallery

        ( _, Err _ ) ->
            ( defaultImage, [] )


initialModelFromImageAndGallery : Image -> List Image -> Url.Url -> Nav.Key -> Model
initialModelFromImageAndGallery image gallery url navKey =
    initialModel image gallery url navKey



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.viewingPage of
        EditorPage ->
            --wheel model
            editorView model

        GalleryPage ->
            galleryView model


{-| For development: view of colorwheel for testing and experimenting
-}
wheel : Model -> Browser.Document Msg
wheel model =
    { title = "Wheel"
    , body =
        [ div [ css [ height (pct 50), width (pct 50), margin (px 100), display inlineBlock ] ]
            [ Html.Styled.map ColorWheelMsg (ColorWheel.view model.colorWheel)
            ]
        , button [ onClick DownloadSvg ] [ text "Download Image" ]
        , button [ onClick DownloadSvgAsJpeg ] [ text "Download Image as JPEG" ]
        , global [ body [ backgroundColor (Colors.toCssColor model.image.backgroundColor) ] ]
        ]
            |> List.map toUnstyled
    }



-- GALLERY VIEW


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
            , fixedDiv
                [ css
                    [ width (pct layout.controlPanel)
                    , height (pct 100)
                    , display inlineBlock
                    , padding (px 10)
                    , boxSizing borderBox
                    ]
                ]
                [ anchorButton (routeFor EditorPage) "Back to editor" ]
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



-- EDITOR VIEW


editorView : Model -> Browser.Document Msg
editorView model =
    let
        alert =
            if model.alertActive then
                [ C.alert "Image saved!" ]

            else
                []
    in
    { title = "Generative Art"
    , body =
        [ div
            [ css [ width (pct 100), height (pct 100) ] ]
            ([ compositionBlocksList model
             , lazy mainImg model.image
             , controlPanel model
             ]
                ++ alert
            )
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
            , backgroundColor (toCssColor Colors.darkGray)
            , color (toCssColor offWhite)
            ]
        ]
        [ infoAndBasicControls
        , colorControls model.colorTarget model.colorWheel
        , videoControls model.videoAngleChangeRate model.playingVideo model.slowMotion
        , turnAngleControl model.turnAngleInputValue
        , strokeWidthControl model.image.strokeWidth
        , curatedSettings

        --, controlsList
        , controlBlock "Info"
            [ p [ css [ overflowWrap breakWord, fontSize (px 14) ] ] [ text (Image.imageStepsLenthString model.image) ]
            , p [ css [ overflowWrap breakWord, fontSize (px 14) ] ] [ text (Image.blockBlueprintString model.editingIndex model.image) ]
            ]
        ]


infoAndBasicControls : Html Msg
infoAndBasicControls =
    controlBlockFlex
        [ anchorButtonHalf (routeFor GalleryPage) "Gallery"
        , primaryButtonHalf SavedToGallery "Save"
        , primaryButtonHalf FullscreenRequested "Full"
        , primaryButtonHalf DownloadSvg "Down"
        , primaryButtonHalf ResetDrawing "Reset"
        , primaryButtonHalf RandomRequested "Rand"
        ]


colorControls : ColorTarget -> ColorWheel.Model -> Html Msg
colorControls colorTarget colorWheel =
    controlBlock "Color"
        [ div [ css [ width (pct 100) ] ]
            [ div [ css [ displayFlex, flexWrap wrap ] ]
                [ primaryButtonSelectable (colorTarget == Stroke) (SetColorTarget Stroke) "Stroke"
                , primaryButtonSelectable (colorTarget == Background) (SetColorTarget Background) "Background"
                ]
            , div [ css [ padding2 (px 10) zero ] ] [ Html.Styled.map ColorWheelMsg (ColorWheel.view colorWheel) ]
            ]
        ]


videoControls : Float -> Bool -> SlowMotion -> Html Msg
videoControls angleChangeRate playingVideo slowMotion =
    let
        playPauseText =
            if playingVideo then
                "Stop"

            else
                "Play"

        slowMotionText =
            case slowMotion of
                NotSet ->
                    "Off"

                Slowly value ->
                    String.fromFloat value ++ "x"
    in
    controlBlock "Video"
        [ span [ css [ display block, marginBottom (px 10) ] ]
            [ text (truncateFloatString 5 (String.fromFloat (angleChangeRate * 1000 / framesInterval)) ++ "x")
            ]
        , div []
            [ span [] [ text "Slow Motion: " ]
            , span [] [ text slowMotionText ]
            ]
        , div [ css [ displayFlex, flexWrap wrap ] ]
            [ primaryButton TogglePlayingVideo playPauseText
            , primaryButton ToggleSlowMotion "Slow Motion"
            , primaryButton ReverseAngleChangeDirection "Reverse"
            ]

        -- Magic values:
        -- min: 0.00005 * 20 = 0.001 degrees/second
        -- max: 2 * 20 = 40 degrees/second
        -- center: 1 * 20 degrees/second at 90% of slider
        , sliderExponentialInput SetVideoAngleChangeRate angleChangeRate videoRateSliderConfig
        ]


videoRateSliderConfig : Utils.MinMaxCenterAt
videoRateSliderConfig =
    ( ( 0.00005, 2 ), ( 1, 0.9 ) )


turnAngleControl : String -> Html Msg
turnAngleControl turnAngleInputValue =
    controlBlock "Angle"
        [ input
            [ id "TurnAngle" -- See index.js, `id` only exists for use in there.
            , type_ "text"
            , value (truncateFloatString 5 turnAngleInputValue)
            , css
                [ display block
                , width (pct 90)
                , marginTop (px 10)
                , backgroundColor (Colors.toCssColor Colors.lightGray)
                , color (Colors.toCssColor Colors.darkGray)

                --, border3 (px 1) solid (Colors.toCssColor Colors.lightGray)
                , border unset
                , boxShadow6 inset zero zero (px 1) (px 1) (toCssColor Colors.darkGray)
                , padding (px 6)
                , fontSize (px 14)
                ]
            , onInput SetTurnAngleInputValue
            , onKeyDown InputKeyPress
            , onFocus (SetFocus TurnAngleInputFocus)
            , onBlur (SetFocus EditorFocus)
            ]
            []
        ]


strokeWidthControl : Float -> Html Msg
strokeWidthControl width =
    controlBlock "Line width"
        -- Magic values:
        -- min: 0.0001px
        -- max: 4px
        -- center: 1px at 90% of slider
        [ span [ css [ display block ] ] [ text <| truncateFloatString 6 (String.fromFloat width) ]
        , sliderExponentialInput SetStrokeWidth width strokeWidthSliderConfig
        , primaryButton (SetStrokeWidth 1) "Reset"
        ]


strokeWidthSliderConfig : Utils.MinMaxCenterAt
strokeWidthSliderConfig =
    ( ( 0.0001, 4 ), ( 1, 0.9 ) )


{-|

    - minValue and maxValue are always enforced; while
    - centerValue can be thought of as a center of gravity
    - centerAt should be a percentage (0 ~ 1), the point in the slider where centerValue will target

-}
sliderExponentialInput : (Float -> msg) -> Float -> Utils.MinMaxCenterAt -> Html msg
sliderExponentialInput msg oldValue exponentialConfig =
    let
        {--
            Convert from linear (0 ~ 1) to exponential (0.000001 ~ 4)

            Take f(x) = a*b^x;
            Where f(centerAt) = centerValue;
            And f(1) = maxValue;
        --}
        ( toExponential, fromExponential ) =
            Utils.linearExponentialTransform exponentialConfig

        onInputCallback stringValue =
            case String.toFloat stringValue of
                Just newValue ->
                    msg (toExponential newValue)

                Nothing ->
                    msg oldValue
    in
    input
        [ type_ "range"
        , Html.Styled.Attributes.min "0.0001"
        , Html.Styled.Attributes.max "1"
        , Html.Styled.Attributes.step "0.0001"
        , value <| String.fromFloat <| fromExponential oldValue
        , onInput onInputCallback
        , css
            [ display block
            , Css.width (pct 100)
            , height (px 27)
            , cursor pointer
            ]
        ]
        []


curatedSettings : Html Msg
curatedSettings =
    controlBlock "Base"
        [ div [ css [ displayFlex, flexWrap wrap ] ]
            [ primaryButton (BasePolygonChanged Triangle) "Triangle"
            , primaryButton (BasePolygonChanged Square) "Square"
            , primaryButton (BasePolygonChanged Pentagon) "Pentagon"
            , primaryButton (BasePolygonChanged Hexagon) "Hexagon"
            ]
        ]


controlsList : Html Msg
controlsList =
    controlBlock "Controls"
        [ controlText "Arrow Up" "Draw"
        , controlText "Arrow Left/Right" "Turn"
        , controlText "Backspace" "Undo last 'Arrow' move on selected image block"
        , controlText "i" "Duplicate selected image block"
        , controlText "d" "Delete top image block"
        , controlText "Space" "Center the image"
        , br [] []
        , p [] [ text "You can also pan and zoom by dragging and scrolling the image" ]
        ]


controlText : String -> String -> Html msg
controlText command explanation =
    div [ css [ margin (px 10) ] ]
        [ span [ css [ fontSize (px 14), backgroundColor (Css.hex "#555") ] ] [ text command ]
        , text <| ": " ++ explanation
        ]


truncateFloatString : Int -> String -> String
truncateFloatString precision floatString =
    let
        truncate string =
            if String.length string <= precision then
                string

            else
                String.dropRight (String.length string - precision) string
    in
    case String.split "." floatString of
        integer :: [] ->
            integer

        integer :: [ decimal ] ->
            integer ++ "." ++ truncate decimal

        _ ->
            floatString


compositionBlocksList : Model -> Html Msg
compositionBlocksList model =
    let
        compositionBlocks =
            model.image
                |> drawBlocks
                |> List.indexedMap (blockBox model.editingIndex model.image.strokeColor)
                |> List.reverse
    in
    fixedDiv
        [ css
            [ backgroundColor (toCssColor Colors.darkGray)
            , height (pct 100)
            , width (pct layout.transformsList)
            , overflow scroll
            , boxSizing borderBox
            , borderRight3 (px 1) solid (toCssColor Colors.black)
            , padding (px 10)
            ]
        ]
        (primaryButtonStyled [ marginBottom (px 20) ] AddSimpleBlock "Add new block" :: compositionBlocks)


blockBox : Int -> Color -> Int -> Svg Msg -> Html Msg
blockBox editingIndex strokeColor index blockSvg =
    let
        ( borderBottomWidth, borderWidthSelected, borderWidthHover ) =
            ( 20, 3, 5 )

        style =
            [ height (vw 8)
            , width (pct 100)
            , margin3 zero auto (px 20)
            , cursor pointer
            , borderRadius (px 3)
            , hover
                [ border3 (px 5) solid (toCssColor strokeColor)

                -- Changing the border style for aesthetic reasons.
                -- This rule is more specific so it overrides `borderOnSelected`.
                , boxSizing contentBox

                -- Compensate top and bottom borders.
                , margin3
                    (px -borderWidthHover)
                    (px -borderWidthHover)
                    (px (borderBottomWidth - borderWidthHover))
                ]

            -- Making position relative to allow for Icon placement to the right.
            , position relative
            , boxShadow5 (px 1) (px 1) (px 2) (px 2) (Colors.toCssColor Colors.blackShadow)
            ]

        borderOnSelected =
            [ border3 (px borderWidthSelected) solid (toCssColor strokeColor)
            , margin3
                (px -borderWidthSelected)
                (px -borderWidthSelected)
                (px (borderBottomWidth - borderWidthSelected))
            ]
    in
    div
        [ css
            (style
                ++ (if editingIndex == index then
                        borderOnSelected

                    else
                        []
                   )
            )
        , onClick (SetEditingIndex index)
        ]
        [ blockSvg
        , Icons.trash
            |> withColor Colors.red_
            |> withOnClick (DropBlock index)
            |> Icons.toSvg
        , Icons.duplicate
            |> withColor strokeColor
            |> withOnClick (DuplicateAndAppendBlock index)
            |> withCss
                [ position absolute
                , right (px 5)
                , bottom (px 2)
                ]
            |> Icons.toSvg
        ]


mainImg : Image -> Html Msg
mainImg image =
    fixedDiv
        [ css
            [ backgroundColor (toCssColor image.backgroundColor)
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
        [ Keyed.node "div"
            [ id "MainImgKeyedWrapper"
            , css
                [ width (pct 100)
                , height (pct 100)
                , backgroundColor (toCssColor image.backgroundColor)
                ]
            ]
            [ ( "_MainImg", drawImage (Just "MainSVG") Nothing False image ) ]
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


updateSvgPathAndBoundariesIfNeeded : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateSvgPathAndBoundariesIfNeeded ( model, cmd ) =
    ( { model | image = Image.updateSvgPathAndBoundaries model.image }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateSvgPathAndBoundariesIfNeeded <|
        let
            updateAndSaveImageAndGalleryWithCmd cmd newModel =
                ( newModel
                , Cmd.batch
                    [ saveEncodedModelToLocalStorage (encodeModel newModel)
                    , replaceUrl newModel.navKey newModel.image
                    , cmd
                    ]
                )

            updateAndSaveImageAndGallery =
                updateAndSaveImageAndGalleryWithCmd Cmd.none
        in
        case msg of
            LinkClicked urlRequest ->
                case urlRequest of
                    Browser.Internal url ->
                        case url.path of
                            "/" ->
                                ( model, Nav.pushUrl model.navKey (routeFor EditorPage ++ Image.toQuery model.image) )

                            _ ->
                                ( model, Nav.pushUrl model.navKey (Url.toString url) )

                    Browser.External href ->
                        ( model, Nav.load href )

            UrlChanged url ->
                -- Called via replaceUrl (and indirectly via click on internal links/href, see LinkClicked above)
                ( { model | viewingPage = mapRouteToPage (parseUrl url) }, Cmd.none )

            RandomRequested ->
                ( model, Random.generate GotRandomImage Image.random )

            GotRandomImage partialImage ->
                let
                    newImage =
                        Image.withImage partialImage model.image
                in
                ( { model
                    | image = newImage
                    , colorWheel = updateColorWheel newImage model.colorTarget model.colorWheel
                  }
                    |> modelWithEditIndexLast
                , Cmd.none
                )

            DownloadSvg ->
                ( model, downloadSvg () )

            DownloadSvgAsJpeg ->
                ( model, downloadSvgAsJpeg () )

            FullscreenRequested ->
                ( model, requestFullscreen () )

            ColorWheelMsg subMsg ->
                let
                    ( updatedColorWheel, subCmd, msgType ) =
                        ColorWheel.update subMsg model.colorWheel

                    image =
                        case model.colorTarget of
                            Stroke ->
                                Image.withStrokeColor updatedColorWheel.color model.image

                            Background ->
                                Image.withBackgroundColor updatedColorWheel.color model.image
                in
                case msgType of
                    ColorWheel.ColorChanged ->
                        updateAndSaveImageAndGallery
                            { model
                                | colorWheel = updatedColorWheel
                                , image = image
                            }

                    ColorWheel.SameColor ->
                        ( { model | colorWheel = updatedColorWheel }, Cmd.none )

            ResetDrawing ->
                updateAndSaveImageAndGallery
                    { model
                        | image = Image.resetImageComposition model.image
                        , editingIndex = 1
                    }

            AddSimpleBlock ->
                { model | image = Image.appendSimpleBlock model.image }
                    |> modelWithEditIndexLast
                    |> updateAndSaveImageAndGallery

            DuplicateAndAppendBlock editingIndex ->
                updateAndSaveImageAndGallery <| duplicateAndAppendBlock model editingIndex

            SetEditingIndex index ->
                ( { model | editingIndex = index }, Cmd.none )

            DropBlock index ->
                updateAndSaveImageAndGallery <|
                    { model
                        | image = Image.dropBlockAtIndex index model.image
                        , editingIndex =
                            if index <= model.editingIndex then
                                max 0 (model.editingIndex - 1)

                            else
                                model.editingIndex
                    }

            EditorKeyPress keyString ->
                let
                    ( newModel, shouldUpdate ) =
                        processKey model keyString
                in
                if keyString == "a" then
                    ( model, Task.attempt (\_ -> NoOp) (Browser.Dom.focus "TurnAngle") )

                else if keyString == "q" then
                    ( model, Random.generate GotRandomImage Image.random )

                else if shouldUpdate then
                    updateAndSaveImageAndGallery newModel

                else
                    ( model, Cmd.none )

            -- also, see `scaleAbout` in https://github.com/ianmackenzie/elm-geometry-svg/blob/master/src/Geometry/Svg.elm
            -- and later check https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/
            Zoom _ deltaY _ mousePos ->
                updateAndSaveImageAndGallery <| applyZoom deltaY mousePos model

            SetColorTarget target ->
                ( { model
                    | colorTarget = target
                    , colorWheel = updateColorWheel model.image target model.colorWheel
                  }
                , Cmd.none
                )

            SetTurnAngle turn ->
                let
                    newModel =
                        { model
                            | image = Image.withTurnAngle turn model.image
                            , turnAngleInputValue = String.fromFloat turn
                        }
                in
                if model.playingVideo then
                    -- Don't update URL and Local Storage on each video step
                    ( newModel, Cmd.none )

                else
                    updateAndSaveImageAndGallery newModel

            SetTurnAngleInputValue stringValue ->
                if stringValue == "" then
                    updateAndSaveImageAndGallery
                        { model
                            | image = Image.withTurnAngle 0 model.image
                            , turnAngleInputValue = stringValue
                        }

                else
                    case String.toFloat stringValue of
                        Just turn ->
                            let
                                newModel =
                                    { model
                                        | image = Image.withTurnAngle turn model.image
                                        , turnAngleInputValue = stringValue
                                    }
                            in
                            if model.playingVideo then
                                -- Don't update URL and Local Storage on each video step
                                ( newModel, Cmd.none )

                            else
                                updateAndSaveImageAndGallery newModel

                        Nothing ->
                            ( { model | turnAngleInputValue = stringValue }, Cmd.none )

            InputKeyPress key ->
                let
                    turnAngleDelta =
                        case model.slowMotion of
                            NotSet ->
                                model.videoAngleChangeDirection * model.videoAngleChangeRate

                            Slowly by ->
                                by * model.videoAngleChangeDirection * model.videoAngleChangeRate

                    newTurnAngle op =
                        op model.image.turnAngle turnAngleDelta

                    newTurnAngleInputValue =
                        String.fromFloat << newTurnAngle

                    updateTurnAngle op =
                        updateAndSaveImageAndGallery
                            { model
                                | image = Image.withTurnAngle (newTurnAngle op) model.image
                                , turnAngleInputValue = newTurnAngleInputValue op
                            }
                in
                case key of
                    "Escape" ->
                        ( model, Task.attempt (\_ -> NoOp) (Browser.Dom.blur "TurnAngle") )

                    "ArrowUp" ->
                        updateTurnAngle (+)

                    "ArrowDown" ->
                        updateTurnAngle (-)

                    _ ->
                        ( model, Cmd.none )

            SetStrokeWidth width ->
                updateAndSaveImageAndGallery <| { model | image = Image.withStrokeWidth width model.image }

            PanStarted pos ->
                ( { model | panStarted = True, lastPos = pos }, Cmd.none )

            PanEnded ->
                updateAndSaveImageAndGallery <| { model | panStarted = False }

            MouseMoved pos ->
                ( { model
                    | image = Image.move model.lastPos pos model.image
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

            WindowResized _ _ ->
                ( model
                , Cmd.batch
                    [ getImgDivPosition
                    , Cmd.map ColorWheelMsg (ColorWheel.getElementDimensions model.colorWheel)
                    ]
                )

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

            ToggleSlowMotion ->
                case model.slowMotion of
                    Slowly _ ->
                        ( { model | slowMotion = NotSet }, Cmd.none )

                    NotSet ->
                        ( { model | slowMotion = Slowly 0.05 }, Cmd.none )

            SetVideoAngleChangeRate rate ->
                ( { model | videoAngleChangeRate = rate }, Cmd.none )

            ReverseAngleChangeDirection ->
                ( { model | videoAngleChangeDirection = model.videoAngleChangeDirection * -1 }, Cmd.none )

            SavedToGallery ->
                updateAndSaveImageAndGalleryWithCmd (delay 4000 HideAlert)
                    { model
                        | gallery =
                            model.image
                                :: model.gallery
                        , alertActive = True
                    }

            RemovedFromGallery index ->
                let
                    newModel =
                        { model | gallery = ListExtra.dropIndex index model.gallery }
                in
                ( newModel, saveEncodedModelToLocalStorage (encodeModel newModel) )

            DuplicateToEdit index ->
                let
                    image =
                        ListExtra.getAt index model.gallery
                            |> Maybe.withDefault model.image
                in
                updateAndSaveImageAndGallery <|
                    { model
                        | viewingPage = EditorPage
                        , image = image
                        , colorWheel = updateColorWheel image model.colorTarget model.colorWheel
                    }

            GotMidiEvent value ->
                if model.viewingPage == GalleryPage then
                    ( model, Cmd.none )

                else
                    -- TODO add throttle and then add updateAndSaveImageAndGallery
                    ( processMidi value model, Cmd.none )

            HideAlert ->
                ( { model | alertActive = False }, Cmd.none )

            NoOp ->
                ( model, Cmd.none )


updateColorWheel : Image -> ColorTarget -> ColorWheel.Model -> ColorWheel.Model
updateColorWheel image target =
    let
        color =
            case target of
                Stroke ->
                    image.strokeColor

                Background ->
                    image.backgroundColor
    in
    ColorWheel.withColor color


processMidi : Encode.Value -> Model -> Model
processMidi value model =
    let
        midiDecoded =
            -- Use .andThen to process if successful
            Decode.decodeValue midiEventDecoder value
    in
    case midiDecoded of
        Ok { command, noteMap, velocityPosition } ->
            if command == 176 && noteMap == 114 then
                let
                    turnAngle =
                        -- Magic numbers, after testing with Arturia MiniLab mk2
                        model.image.turnAngle
                            + (velocityPosition - 64)
                            * model.videoAngleChangeRate

                    turnAngleInputValue =
                        String.fromFloat turnAngle
                in
                -- Turn Angle
                { model
                    | image = Image.withTurnAngle turnAngle model.image
                    , turnAngleInputValue = turnAngleInputValue
                }

            else if command == 176 && noteMap == 115 && velocityPosition == 0 then
                { model | image = Image.centralize model.image }
                --
                --
                --

            else if command == 176 && noteMap == 112 then
                -- Angle Change Rate
                { model
                    | videoAngleChangeRate =
                        max (model.videoAngleChangeRate + (velocityPosition - 64) * 0.000501)
                            0.0001
                }

            else if command == 176 && noteMap == 113 && velocityPosition == 0 then
                { model | videoAngleChangeRate = 0.01 }

            else if command == 169 && noteMap == 43 then
                { model | slowMotion = Slowly (1 - velocityPosition / 128) }

            else if command == 137 && noteMap == 43 then
                { model | slowMotion = NotSet }

            else if command == 153 && noteMap == 37 then
                -- Reverse angle change direction
                { model | videoAngleChangeDirection = -model.videoAngleChangeDirection }

            else if command == 153 && noteMap == 36 then
                -- Play / pause video
                { model | playingVideo = not model.playingVideo }

            else if command == 176 && noteMap == 18 then
                -- Stroke Width
                { model | image = model.image |> Image.withStrokeWidth (adjustInputForStrokeWidth velocityPosition) }
                --
                --
                --

            else if command == 176 && noteMap == 93 then
                -- Background Color RGBA: (R, 93) (G, 73) (B, 75)
                { model
                    | image =
                        model.image
                            |> Image.withBackgroundColor (Colors.updateRed (velocityPosition * 2) model.image.backgroundColor)
                }

            else if command == 176 && noteMap == 73 then
                { model
                    | image =
                        model.image
                            |> Image.withBackgroundColor (Colors.updateGreen (velocityPosition * 2) model.image.backgroundColor)
                }

            else if command == 176 && noteMap == 75 then
                { model
                    | image =
                        model.image
                            |> Image.withBackgroundColor (Colors.updateBlue (velocityPosition * 2) model.image.backgroundColor)
                }

            else if command == 176 && noteMap == 91 then
                -- Stroke Color RGBA: (R, 91) (G, 79) (B, 72)
                { model
                    | image =
                        model.image
                            |> Image.withStrokeColor (Colors.updateRed (velocityPosition * 2) model.image.strokeColor)
                }

            else if command == 176 && noteMap == 79 then
                { model
                    | image =
                        model.image
                            |> Image.withStrokeColor (Colors.updateGreen (velocityPosition * 2) model.image.strokeColor)
                }

            else if command == 176 && noteMap == 72 then
                { model
                    | image =
                        model.image
                            |> Image.withStrokeColor (Colors.updateBlue (velocityPosition * 2) model.image.strokeColor)
                }
                --
                --
                --

            else if command == 176 && noteMap == 71 then
                -- Background Color HSL: (H, 71) (G, 76) (B, 77)
                { model
                    | image =
                        model.image
                            |> Image.withBackgroundColor (Colors.updateHue (velocityPosition / 127) model.image.backgroundColor)
                }

            else if command == 176 && noteMap == 76 then
                { model
                    | image =
                        model.image
                            |> Image.withBackgroundColor (Colors.updateSaturation (velocityPosition / 127) model.image.backgroundColor)
                }

            else if command == 176 && noteMap == 77 then
                { model
                    | image =
                        model.image
                            |> Image.withBackgroundColor (Colors.updateLightness (velocityPosition / 127) model.image.backgroundColor)
                }

            else if command == 176 && noteMap == 19 then
                -- Background Color HSL: (H, 19) (G, 16) (B, 17)
                { model
                    | image =
                        model.image
                            |> Image.withStrokeColor (Colors.updateHue (velocityPosition / 127) model.image.strokeColor)
                }

            else if command == 176 && noteMap == 16 then
                { model
                    | image =
                        model.image
                            |> Image.withStrokeColor (Colors.updateSaturation (velocityPosition / 127) model.image.strokeColor)
                }

            else if command == 176 && noteMap == 17 then
                { model
                    | image =
                        model.image
                            |> Image.withStrokeColor (Colors.updateLightness (velocityPosition / 127) model.image.strokeColor)
                }

            else
                -- Else
                model

        _ ->
            model


processKey : Model -> String -> ( Model, Bool )
processKey model keyPressed =
    let
        appendStep step =
            ( { model | image = Image.appendStepAtIndex step model.editingIndex model.image }, True )

        updateAngle deg =
            ( { model
                | image = Image.withTurnAngle deg model.image
                , turnAngleInputValue = String.fromFloat deg
              }
            , True
            )

        exponentialMove config value op =
            let
                ( toExponential, fromExponential ) =
                    Utils.linearExponentialTransform config
            in
            toExponential (op (fromExponential value) 0.01)

        updateStrokeWidth op =
            { model
                | image =
                    Image.withStrokeWidth
                        (exponentialMove strokeWidthSliderConfig model.image.strokeWidth op)
                        model.image
            }

        updateVideoRate op =
            { model | videoAngleChangeRate = exponentialMove videoRateSliderConfig model.videoAngleChangeRate op }
    in
    case keyPressed of
        -- DRAW
        --
        "ArrowLeft" ->
            appendStep L

        "ArrowRight" ->
            appendStep R

        "ArrowUp" ->
            appendStep D

        "ArrowDown" ->
            appendStep S

        "Backspace" ->
            ( { model | image = Image.dropLastStepAtIndex model.editingIndex model.image }, True )

        -- RESET POSITION AND ZOOM
        --
        "c" ->
            ( { model | image = Image.centralize model.image }, True )

        -- ITERATE / DEITERATE
        --
        "i" ->
            ( duplicateAndAppendBlock model model.editingIndex, True )

        "d" ->
            ( dropLastBlock model, True )

        -- VIDEO CONTROLS
        --
        -- Play / pause
        " " ->
            ( { model | playingVideo = not model.playingVideo }, True )

        -- Slow motion
        "s" ->
            case model.slowMotion of
                Slowly _ ->
                    ( { model | slowMotion = NotSet }, True )

                NotSet ->
                    ( { model | slowMotion = Slowly 0.05 }, True )

        "," ->
            ( updateVideoRate (-), True )

        "." ->
            ( updateVideoRate (+), True )

        -- Change turn angle direction
        "r" ->
            ( { model | videoAngleChangeDirection = model.videoAngleChangeDirection * -1 }, True )

        -- ZOOM
        --
        -- Zoom out large
        "-" ->
            ( applyZoom 30 model.imgDivCenter model, True )

        -- Zoom out small
        "_" ->
            ( applyZoom 1 model.imgDivCenter model, True )

        -- Zoom in Large
        "=" ->
            ( applyZoom -30 model.imgDivCenter model, True )

        -- Zoom in small
        "+" ->
            ( applyZoom -1 model.imgDivCenter model, True )

        -- STROKE WIDTH
        --
        "[" ->
            ( updateStrokeWidth (-), True )

        "]" ->
            ( updateStrokeWidth (+), True )

        -- CHANGE CURVE
        --
        "o" ->
            ( { model | image = Image.withCurve Image.Curve model.image }, True )

        "l" ->
            ( { model | image = Image.withCurve Image.Line model.image }, True )

        -- CHANGE ANGLE
        --
        -- Almost a line
        "0" ->
            updateAngle 179

        -- Triangle
        "1" ->
            updateAngle 120

        -- Square
        "2" ->
            updateAngle 90

        -- Pentagon
        "3" ->
            updateAngle 72

        -- Hexagon
        "4" ->
            updateAngle 60

        -- Heptagon
        "5" ->
            updateAngle 51

        -- Octagon
        "6" ->
            updateAngle 45

        -- 4-point star
        "7" ->
            updateAngle 135

        -- X-point star
        "8" ->
            updateAngle 144

        -- Y-point star
        "9" ->
            updateAngle 165

        _ ->
            ( model, False )


duplicateAndAppendBlock : Model -> Int -> Model
duplicateAndAppendBlock model editingIndex =
    { model | image = Image.duplicateBlock editingIndex model.image }
        |> modelWithEditIndexLast


dropLastBlock : Model -> Model
dropLastBlock model =
    let
        image =
            Image.dropLastBlock model.image
    in
    { model
        | image = image
        , editingIndex = min model.editingIndex (Image.length image - 1)
    }


applyZoom : Float -> Position -> Model -> Model
applyZoom deltaY mousePos model =
    { model | image = Image.zoom deltaY mousePos model.imgDivCenter model.image }


updateCompositionBaseAndAngle : Model -> Polygon -> Model
updateCompositionBaseAndAngle model polygon =
    { model | image = Image.resetBaseTo polygon model.image }



-- COMMANDS


getImgDivPosition : Cmd Msg
getImgDivPosition =
    Task.attempt GotImgDivPosition (Browser.Dom.getElement "mainImg")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.viewingPage of
        EditorPage ->
            Sub.batch
                [ editorPageSubs model
                , alwaysSubs
                ]

        GalleryPage ->
            alwaysSubs


alwaysSubs : Sub Msg
alwaysSubs =
    Browser.Events.onResize WindowResized


editorPageSubs : Model -> Sub Msg
editorPageSubs model =
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
            if model.focus == EditorFocus then
                Browser.Events.onKeyDown (keyPressDecoder EditorKeyPress)

            else
                Sub.none

        playingVideoSub =
            if model.playingVideo then
                case model.slowMotion of
                    NotSet ->
                        Time.every framesInterval (always (SetTurnAngle (model.image.turnAngle + (model.videoAngleChangeDirection * model.videoAngleChangeRate))))

                    Slowly by ->
                        Time.every framesInterval (always (SetTurnAngle (model.image.turnAngle + (by * model.videoAngleChangeDirection * model.videoAngleChangeRate))))

            else
                Sub.none
    in
    Sub.batch
        [ keyPressSub
        , panSubs
        , playingVideoSub
        , midiEvent GotMidiEvent
        , Sub.map ColorWheelMsg (ColorWheel.subscriptions model.colorWheel)
        ]


framesInterval : Float
framesInterval =
    50



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



-- KEYBOARD, MOUSE and WHEEL


keyPressDecoder : (String -> Msg) -> Decoder Msg
keyPressDecoder msg =
    Decode.map msg
        (Decode.field "key" Decode.string)


onKeyDown : (String -> Msg) -> Html.Styled.Attribute Msg
onKeyDown msg =
    on "keydown" (keyPressDecoder msg)


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


type alias MidiEvent =
    { command : Float
    , noteMap : Float
    , velocityPosition : Float
    }


{--}
midiEventDecoder : Decoder MidiEvent
midiEventDecoder =
    Decode.map3 MidiEvent
        (Decode.at [ "data", "0" ] Decode.float)
        (Decode.at [ "data", "1" ] Decode.float)
        (Decode.at [ "data", "2" ] Decode.float)
--}



{--Components
--}


primaryButtonStyle : List Css.Style
primaryButtonStyle =
    let
        lightGray =
            toCssColor Colors.lightGray

        darkGray =
            toCssColor Colors.darkGray

        buttonHeight =
            28

        borderWidth =
            1
    in
    [ color lightGray
    , backgroundColor transparent
    , border3 (px borderWidth) solid lightGray
    , borderRadius (px 6)
    , height (px buttonHeight)
    , width (pct 95)

    {--
    , withMedia [ Media.all [ Media.maxWidth (px 1160), Media.minWidth (px 650) ] ]
        [ minWidth (px 85)
        , height (px 60)
        ]
    --}
    --, withMedia [ Media.all [ Media.minWidth (px 1700) ] ] [ width (px 106) ]
    , margin2 (px 5) auto
    , display block
    , cursor pointer
    , boxSizing borderBox

    -- TODO add font files and @font-face:
    -- https://stackoverflow.com/questions/107936/how-to-add-some-non-standard-font-to-a-website
    -- https://fonts.google.com/specimen/Roboto?selection.family=Roboto
    -- Research best fallback option
    --, fontFamilies [ "Roboto" ]
    , fontFamily sansSerif
    , fontSize (px 16)
    , textAlign center
    , textDecoration none
    , lineHeight (px (buttonHeight - 2 * borderWidth))
    , hover primaryButtonStyleHover
    ]


primaryButtonStyleHover : List Css.Style
primaryButtonStyleHover =
    let
        -- COPIED FROM ABOVE
        lightGray =
            toCssColor Colors.lightGray

        darkGray =
            toCssColor Colors.darkGray

        buttonHeight =
            28
    in
    [ border Css.unset
    , backgroundColor lightGray
    , color darkGray
    , active primaryButtonStyleActive
    , lineHeight (px buttonHeight)
    ]


primaryButtonStyleActive : List Css.Style
primaryButtonStyleActive =
    [ backgroundColor (Colors.toCssColor Colors.activeElementGray) ]


primaryButtonStyled : List Css.Style -> msg -> String -> Html msg
primaryButtonStyled style msg btnText =
    button [ css (primaryButtonStyle ++ style), onClick msg ] [ text btnText ]


primaryButton : msg -> String -> Html msg
primaryButton =
    primaryButtonStyled []


halfStyle : List Css.Style
halfStyle =
    [ minWidth (pct 45), width (pct 45) ]


primaryButtonHalf : msg -> String -> Html msg
primaryButtonHalf =
    primaryButtonStyled halfStyle


primaryButtonSelectable : Bool -> msg -> String -> Html msg
primaryButtonSelectable isSelected =
    primaryButtonStyled
        (if isSelected then
            primaryButtonStyleHover ++ primaryButtonStyleActive

         else
            []
        )


anchorButtonStyled : List Css.Style -> String -> String -> Html msg
anchorButtonStyled style href_ title =
    a [ href href_, css (primaryButtonStyle ++ style) ] [ text title ]


anchorButton : String -> String -> Html msg
anchorButton =
    anchorButtonStyled []


anchorButtonHalf : String -> String -> Html msg
anchorButtonHalf =
    anchorButtonStyled halfStyle


controlBlockStyle : List Css.Style
controlBlockStyle =
    [ padding (px 10), borderBottom3 (px 1) solid (toCssColor Colors.black), fontFamily Css.sansSerif ]


controlBlock : String -> List (Html Msg) -> Html Msg
controlBlock title list =
    div [ css controlBlockStyle ]
        (span [ css [ display block, marginBottom (px 8), cursor default, fontSize (px 18) ] ] [ text title ]
            :: list
        )


controlBlockFlex : List (Html Msg) -> Html Msg
controlBlockFlex =
    div [ css (controlBlockStyle ++ [ displayFlex, flexWrap wrap ]) ]
