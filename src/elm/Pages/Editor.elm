port module Pages.Editor exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , decoder
    , encode
    , initialCmd
    , initialModel
    , subscriptions
    , update
    , urlEncode
    , urlParser
    , view
    , withImage
    , withPartialImage
    , withUrl
    )

import Browser
import Browser.Dom exposing (Element)
import Browser.Events
import ColorWheel
import Colors exposing (Color, offWhite, toCssColor)
import Components as C
import Components.Dropdown as Dropdown
import Components.TopBar as TopBar
import Css
    exposing
        ( absolute
        , after
        , alignItems
        , auto
        , backgroundColor
        , before
        , block
        , border
        , border3
        , borderBottom
        , borderBox
        , borderLeft3
        , borderRadius
        , borderRight3
        , borderTop3
        , bottom
        , boxShadow5
        , boxShadow6
        , boxSizing
        , breakWord
        , calc
        , center
        , color
        , content
        , contentBox
        , cursor
        , default
        , display
        , displayFlex
        , fixed
        , flexDirection
        , flexGrow
        , flexWrap
        , fontSize
        , height
        , hidden
        , hover
        , inset
        , int
        , justifyContent
        , left
        , margin
        , margin2
        , margin3
        , marginBottom
        , marginLeft
        , marginRight
        , marginTop
        , maxWidth
        , middle
        , minus
        , none
        , overflow
        , overflowWrap
        , overflowX
        , overflowY
        , padding
        , padding2
        , paddingBottom
        , paddingRight
        , pct
        , plus
        , pointer
        , position
        , px
        , relative
        , right
        , row
        , scroll
        , solid
        , spaceAround
        , spaceBetween
        , textAlign
        , top
        , unset
        , vw
        , width
        , wrap
        , zero
        )
import Events exposing (ShiftKey, keyPressDecoder, midiEventDecoder, mousePositionDecoder, onKeyDown, onWheel)
import Html.Styled exposing (Html, div, input, label, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (checked, css, id, type_, value)
import Html.Styled.Events
    exposing
        ( on
        , onBlur
        , onCheck
        , onClick
        , onFocus
        , onInput
        , onMouseUp
        )
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
import Icons exposing (withColor, withCss, withOnClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core exposing (Step(..))
import LSystem.Draw exposing (drawBlocks, drawImage)
import LSystem.Image as Image
    exposing
        ( Image
        , PartialImage
        , Polygon(..)
        , defaultImage
        , welcomeImage
        , withImage
        )
import List.Extra
import Midi exposing (adjustInputForStrokeWidth)
import Pages exposing (Page(..), routeFor)
import Random
import Set exposing (Set)
import Svg.Styled exposing (Svg)
import Task
import Time
import Url
import Url.Builder
import Url.Parser as Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query
import Utils exposing (Position, delay, floatModBy)



-- PORTS


port downloadSvg : () -> Cmd msg


port downloadSvgAsJpeg : () -> Cmd msg


port requestFullscreen : () -> Cmd msg


port copyTextToClipboard : String -> Cmd msg


port copyToClipboardResult : (Bool -> msg) -> Sub msg


port midiEvent : (Encode.Value -> msg) -> Sub msg



-- TYPES AND MSG
-- MSG


{-| TODO: Rename Msgs: put all verbs in past tense and choose better words.
-}
type Msg
    = TopBarMsg TopBar.Msg
      -- Global keyboard listener
      -- TODO change to `KeyPress Where String`
    | EditorKeyPress String
    | InputKeyPress String
    | SetKeyboardInput KeyboardInput
      -- Main commands
    | AddSimpleBlock
    | ResetDrawing
    | BasePolygonChanged Polygon
    | DuplicateAndAppendBlock Int
    | SetEditingIndex Int
    | DropBlock Int
      -- Storage
    | SavedToGallery
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
    | ExchangeColors
      -- Angle
    | SetTurnAngleInputValue String
      -- Stroke
    | SetStrokeWidth Float
    | ToggleCurveType
      -- Share
    | CopyUrlToClipboard
    | CopyUrlToClipboardResult Bool
      -- Download
    | DownloadSvg
    | DownloadSvgAsJpeg
      -- Focus
    | SetFocus Focus
      -- Video
    | ToggleVideo
    | ToggleColorVideo
    | VideoTick
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



-- Could be updated to `type alias ExternalMsg = { updatedEditor : Bool, updatedGallery : Bool, ... }`
-- if we want to "send" more than one msg at a time


type ExternalMsg
    = UpdatedEditor
    | UpdatedGallery Image
    | NothingToUpdate



-- MODEL


type alias Model =
    -- Aplication heart
    { image : Image
    , editingIndex : Int

    -- Pan and Zoom
    , panStarted : Bool
    , lastPos : Position

    -- Main Image Div Coordinates
    , imgDivCenter : Position
    , imgDivStart : Position

    -- ColorWheel
    , colorWheel : ColorWheel.State
    , colorTarget : ColorTarget

    -- Browser Focus
    , focus : Focus

    -- Video
    , videoAngleChangeRate : Float
    , slowMotion : SlowMotion
    , videoAngleChangeDirection : Float
    , playingVideo : Bool
    , colorInVideo : Bool

    -- Input controls value
    , turnAngleInputValue : String
    , keyboardInput : KeyboardInput

    -- Top Bar
    , topBar : TopBar.State Msg

    -- Alert Popup
    , alert : Maybe String

    -- URL (for getting protocol and host to make shareable URL)
    , url : Maybe Url.Url
    }



-- AUXILIARY TYPES


type Focus
    = EditorFocus
    | TurnAngleInputFocus


type
    SlowMotion
    -- TODO Change to `type alias SlowMotion = Maybe Float` ?
    = NotSet
    | Slowly Float


slowMotionToBool : SlowMotion -> Bool
slowMotionToBool slowMotion =
    if slowMotion == NotSet then
        False

    else
        True


type ColorTarget
    = Stroke
    | Background


type KeyboardInput
    = ShortcutsMode
    | WritingMode


{-| Those are Strings because we use a Set to check which ones are playing
--- and Sets can't have custom/union types in it, only comparables.
-}
type alias Video =
    String


video =
    { changeAngle = "changeAngle"
    , changeColorLinear = "changeColorLinear"
    , changeColorSinusoidal = "changeColorSinusoidal"
    }



-- INITIAL STUFF


initialModel : Model
initialModel =
    -- Aplication heart
    { image = welcomeImage
    , editingIndex = 1

    -- Pan and Zoom
    , panStarted = False
    , lastPos = ( 0, 0 )

    -- Main Image Div Coordinates
    , imgDivCenter = ( 0, 0 )
    , imgDivStart = ( 0, 0 )

    -- ColorWheel
    , colorWheel = ColorWheel.initialState "ColorWheel1" (Just defaultImage.strokeColor)
    , colorTarget = Stroke

    -- Browser Focus
    , focus = EditorFocus

    -- Video
    , videoAngleChangeRate = 0.0001
    , slowMotion = NotSet

    --, playingVideo = Set.fromList [ video.changeAngle, video.changeColorLinear ]
    , playingVideo = False
    , colorInVideo = False
    , videoAngleChangeDirection = 1

    -- Input controls value
    , turnAngleInputValue = String.fromFloat welcomeImage.turnAngle
    , keyboardInput = ShortcutsMode

    -- Top Bar
    , topBar = TopBar.init TopBarMsg

    -- Alert Popup
    , alert = Nothing

    -- URL
    , url = Nothing
    }


initialCmd : Model -> Cmd Msg
initialCmd model =
    Cmd.batch
        [ getImgDivPosition
        ]



-- WITH* PATTERN


withImage : Image -> Model -> Model
withImage image model =
    { model
        | image = image
        , colorWheel = updateColorWheel image model.colorTarget model.colorWheel
        , turnAngleInputValue = String.fromFloat image.turnAngle
    }


withPartialImage : PartialImage -> Model -> Model
withPartialImage partialImage model =
    let
        image =
            Image.withImage partialImage model.image
    in
    withImage image model


withEditIndexLast : Model -> Model
withEditIndexLast model =
    { model | editingIndex = Image.length model.image - 1 }


withUrl : Url.Url -> Model -> Model
withUrl url model =
    { model | url = Just url }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        alert =
            case model.alert of
                Just message ->
                    [ C.alert message ]

                Nothing ->
                    []
    in
    { title = "Generative Art"
    , body =
        [ Lazy.lazy3 TopBar.view EditorPage (topBarElements model) model.topBar |> toUnstyled
        , div
            [ css [ width (pct 100), height (calc (pct 100) minus (px 41)) ] ]
            ([ Lazy.lazy2 compositionBlocksList model.image model.editingIndex
             , Lazy.lazy mainImg model.image
             , Lazy.lazy turnAngleControl model.turnAngleInputValue
             ]
                ++ alert
            )
            |> toUnstyled
        ]
    }


topBarElements : Model -> List (TopBar.Element Msg)
topBarElements model =
    [ TopBar.Dropdown
        { title = "File"
        , body = fileControls
        }
    , TopBar.Dropdown
        { title = "Color"
        , body = colorControls model.colorTarget model.colorWheel
        }
    , TopBar.Dropdown
        { title = "Video"
        , body = videoControls model.videoAngleChangeRate model.playingVideo model.slowMotion model.colorInVideo
        }
    , TopBar.Dropdown
        { title = "Stroke"
        , body = strokeControl model.image.strokeWidth model.image.curve
        }
    , TopBar.Dropdown
        { title = "Info"
        , body = info model
        }
    ]


fileControls : Html Msg
fileControls =
    div [ css [ width (px 200) ] ]
        -- TODO change all of these to buttons
        [ div [ css (Dropdown.listItemStyle False), onClick SavedToGallery ] [ text "Save to gallery" ]
        , div [ css (Dropdown.listItemStyle False), onClick CopyUrlToClipboard ] [ text "Copy URL to share" ]
        , div [ css (Dropdown.listItemStyle False), onClick DownloadSvg ] [ text "Download as SVG" ]
        , div [ css (Dropdown.listItemStyle False), onClick DownloadSvgAsJpeg ] [ text "Download as JPEG" ]

        -- TODO Maybe change to a "New" dropdown
        , div [ css (Dropdown.listItemStyle False), onClick ResetDrawing ] [ text "Reset Image" ]
        , div [ css (Dropdown.listItemStyle False), onClick RandomRequested ] [ text "Random Image" ]

        -- TODO Remove from here and add a on-screen button
        , div [ css (Dropdown.listItemStyle False), onClick FullscreenRequested ] [ text "Enter fullscreen" ]
        ]


colorControls : ColorTarget -> ColorWheel.State -> Html Msg
colorControls colorTarget colorWheel =
    let
        colorTargetElement target title =
            div
                [ css (Dropdown.flexListItemStyle (colorTarget == target) ++ [ textAlign center ])
                , onClick (SetColorTarget target)
                ]
                [ text title ]

        exchangeBtnStyle =
            Dropdown.listItemStyle False
                ++ [ textAlign center
                   , boxSizing borderBox
                   , borderTop3 (px 1) solid (Colors.toCssColor Colors.theme.active)
                   ]
    in
    div []
        [ div [ css [ displayFlex, flexDirection row, width (px 270) ] ]
            [ colorTargetElement Stroke "Stroke"
            , colorTargetElement Background "Background"
            ]
        , div [ css [ padding (px 12) ] ] [ Html.Styled.map ColorWheelMsg (ColorWheel.view colorWheel) ]
        , div [ css exchangeBtnStyle, onClick ExchangeColors ] [ text "Exchange colors" ]
        ]


videoControls : Float -> Bool -> SlowMotion -> Bool -> Html Msg
videoControls angleChangeRate playingVideo slowMotion colorInVideo =
    let
        playPauseText =
            if playingVideo then
                "Pause"

            else
                "Play"

        btnStyle =
            Dropdown.listItemStyle False ++ [ textAlign center, boxSizing borderBox ]

        checkbox msg isChecked title =
            label [ css [ display block, padding2 (px 10) zero, marginTop (px 10), cursor pointer ] ]
                [ input [ type_ "checkbox", checked isChecked, onCheck msg ] []
                , span [ css [ marginLeft (px 10) ] ] [ text title ]
                ]
    in
    div [ css [ width (px 240) ] ]
        -- TODO change `video.changeAngle` to `video` i.e. the whole set
        [ div [ css btnStyle, onClick ToggleVideo ] [ text playPauseText ]
        , div [ css btnStyle, onClick ReverseAngleChangeDirection ] [ text "Reverse direction" ]
        , div [ css [ padding (px 10) ] ]
            [ span [ css [ display block, margin2 (px 10) zero, cursor default ] ]
                [ text ("Speed: " ++ (truncateFloatString 5 (String.fromFloat (angleChangeRate * 1000 / framesInterval)) ++ "° ∕ second")) ]

            -- Magic values:
            -- min: 0.00005 * 20 = 0.001 degrees/second
            -- max: 2 * 20 = 40 degrees/second
            -- center: 1 * 20 degrees/second at 90% of slider
            , sliderExponentialInput SetVideoAngleChangeRate angleChangeRate videoRateSliderConfig
            , checkbox (always ToggleSlowMotion) (slowMotionToBool slowMotion) "Slow Motion"
            , checkbox (always ToggleColorVideo) colorInVideo "Also change colors"
            ]
        ]


strokeControl : Float -> Image.PathCurve -> Html Msg
strokeControl strokeWidth curve =
    let
        btnStyle =
            -- TODO This is duplicated, move to Components
            Dropdown.listItemStyle False
                ++ [ textAlign center
                   , boxSizing borderBox
                   , borderTop3 (px 1) solid (Colors.toCssColor Colors.theme.active)
                   , borderBottom zero
                   ]

        curveText =
            "Change to "
                ++ (if curve == Image.Line then
                        "organic"

                    else
                        "sharp"
                   )
    in
    div [ css [ width (px 240) ] ]
        [ div [ css [ padding (px 10) ] ]
            [ span [ css [ display block, margin2 (px 10) zero, cursor default ] ]
                [ text <| "Line Width: " ++ truncateFloatString 6 (String.fromFloat strokeWidth) ]

            -- Magic values:
            -- min: 0.0001px
            -- max: 4px
            -- center: 1px at 90% of slider
            , sliderExponentialInput SetStrokeWidth strokeWidth strokeWidthSliderConfig
            ]
        , div [ css btnStyle, onClick (SetStrokeWidth 1) ] [ text "Reset" ]
        , div [ css btnStyle, onClick ToggleCurveType ] [ text curveText ]
        ]


info : Model -> Html Msg
info model =
    div [ css [ width (px 180), padding (px 10) ] ]
        [ p [ css [ overflowWrap breakWord, fontSize (px 14) ] ] [ text (Image.imageStepsLenthString model.image) ]
        , p [ css [ overflowWrap breakWord, fontSize (px 14) ] ] [ text (Image.blockBlueprintString model.editingIndex model.image) ]
        ]


turnAngleControl : String -> Html Msg
turnAngleControl turnAngleInputValue =
    let
        angleNumberSpan value =
            span
                [ css
                    [ width (px angleNumberWidth)
                    , textAlign center
                    , color (Colors.toCssColor Colors.offWhite)
                    ]
                ]
                [ text value ]

        angleNumberWidth =
            40
    in
    div
        [ css
            [ displayFlex
            , flexDirection row
            , Css.property "justify-content" "space-evenly"
            , alignItems center
            , position fixed
            , bottom zero
            , right zero
            , height (px layout.turnAngleControlHeight)
            , width (calc (pct 100) minus (px layout.transformsList))
            , backgroundColor (Colors.toCssColor Colors.theme.backgroundColor)
            , borderLeft3 (px 1) solid (Colors.toCssColor Colors.black)
            , borderTop3 (px 1) solid (Colors.toCssColor Colors.black)
            , boxSizing borderBox
            ]
        ]
        [ turnAngleInput turnAngleInputValue
        , div [ css [ width (calc (pct 80) minus (px layout.turnAngleInputWidth)), flexGrow (int 2), marginRight (px 40) ] ]
            [ turnAngleSlider turnAngleInputValue
            , div
                [ css
                    [ width (calc (pct 100) plus (px (angleNumberWidth - 6)))
                    , displayFlex
                    , flexDirection row
                    , justifyContent spaceBetween
                    , position relative

                    -- `+ 3` = `6 / 2` (see width above), `+ 3` to ignore `•` on centering numbers.
                    , left (px (-angleNumberWidth / 2 + 3 + 3))
                    ]
                ]
                (List.Extra.initialize 5 (\index -> index * turnAngleSliderMaxValue // 4 |> String.fromInt)
                    |> List.map ((\deg -> deg ++ "°") >> angleNumberSpan)
                )
            ]
        ]


turnAngleSliderMaxValue : Int
turnAngleSliderMaxValue =
    180


turnAngleInput : String -> Html Msg
turnAngleInput turnAngleInputValue =
    input
        [ id "TurnAngle" -- See index.js, `id` only exists for use in there.
        , type_ "text"
        , value (truncateFloatString 5 turnAngleInputValue)
        , css
            [ display block
            , width (px layout.turnAngleInputWidth)
            , height (px 18)
            , backgroundColor (Colors.toCssColor Colors.lightGray)
            , color (Colors.toCssColor Colors.darkGray)

            --, border3 (px 1) solid (Colors.toCssColor Colors.lightGray)
            , border unset
            , boxShadow6 inset zero zero (px 1) (px 1) (toCssColor Colors.darkGray)
            , padding (px 8)
            , fontSize (px 14)
            , textAlign center
            , flexGrow (int 1)
            , marginLeft (px 30)
            , marginRight (px 30)
            , maxWidth (px 160)
            ]
        , onInput SetTurnAngleInputValue
        , onKeyDown InputKeyPress
        , onFocus (SetFocus TurnAngleInputFocus)
        , onBlur (SetFocus EditorFocus)
        ]
        []


turnAngleSlider : String -> Html Msg
turnAngleSlider turnAngleInputValue =
    input
        [ type_ "range"
        , Html.Styled.Attributes.min "0.0001"
        , Html.Styled.Attributes.max (String.fromInt turnAngleSliderMaxValue)
        , Html.Styled.Attributes.step "0.0001"
        , value turnAngleInputValue
        , onInput SetTurnAngleInputValue
        , css
            [ display block
            , height (px 27)
            , cursor pointer
            ]
        ]
        []


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
    C.controlBlock "Base"
        [ div [ css [ displayFlex, flexWrap wrap ] ]
            [ C.primaryButton (BasePolygonChanged Triangle) "Triangle"
            , C.primaryButton (BasePolygonChanged Square) "Square"
            , C.primaryButton (BasePolygonChanged Pentagon) "Pentagon"
            , C.primaryButton (BasePolygonChanged Hexagon) "Hexagon"
            ]
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


compositionBlocksList : Image -> Int -> Html Msg
compositionBlocksList image editingIndex =
    let
        compositionBlocks =
            image
                |> drawBlocks
                |> List.indexedMap (blockBox editingIndex image.strokeColor)
                |> List.reverse
    in
    C.fixedDiv
        [ css
            [ backgroundColor (toCssColor Colors.darkGray)
            , height (pct 100)
            , width (px (layout.transformsList + layout.paddingToHideScrollbars))
            , overflowY scroll
            , boxSizing borderBox
            , borderRight3 (px 1) solid (toCssColor Colors.black)
            , padding (px 15)
            , paddingRight (px layout.paddingToHideScrollbars)
            ]
        ]
        (C.primaryButtonStyled [ marginBottom (px 20) ] AddSimpleBlock "Add new block" :: compositionBlocks)


blockBox : Int -> Color -> Int -> Svg Msg -> Html Msg
blockBox editingIndex strokeColor index blockSvg =
    let
        ( borderBottomWidth, borderWidthSelected, borderWidthHover ) =
            ( 20, 3, 5 )

        style =
            [ height (px 150)
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
            |> Icons.toSvgOldAPI
        , Icons.duplicate
            |> withColor strokeColor
            |> withOnClick (DuplicateAndAppendBlock index)
            |> withCss
                [ position absolute
                , right (px 5)
                , bottom (px 2)
                ]
            |> Icons.toSvgOldAPI
        ]


mainImg : Image -> Html Msg
mainImg image =
    C.fixedDiv
        [ css
            [ backgroundColor (toCssColor image.backgroundColor)
            , position fixed
            , height (calc (pct 100) minus (px 40))
            , top (px 40)
            , width (calc (pct 100) minus (px layout.transformsList))
            , right zero
            , overflow hidden
            ]
        , id "mainImg"
        , onWheel Zoom
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
            [ ( "_MainImg", drawImage (Just "MainSVG") False image ) ]
        ]


layout :
    { transformsList : Float
    , paddingToHideScrollbars : Float
    , turnAngleControlHeight : Float
    , turnAngleInputWidth : Float
    }
layout =
    { transformsList = 240
    , paddingToHideScrollbars = 40
    , turnAngleControlHeight = 60
    , turnAngleInputWidth = 110
    }



-- UPDATE


updateSvgPathAndBoundariesIfNeeded : ( Model, Cmd Msg, ExternalMsg ) -> ( Model, Cmd Msg, ExternalMsg )
updateSvgPathAndBoundariesIfNeeded ( model, cmd, externalMsg ) =
    ( { model | image = Image.updateSvgPathAndBoundaries model.image }, cmd, externalMsg )


update : Msg -> Model -> ( Model, Cmd Msg, ExternalMsg )
update msg model =
    updateSvgPathAndBoundariesIfNeeded <|
        case msg of
            TopBarMsg subMsg ->
                let
                    ( updatedTopBar, cmd ) =
                        TopBar.update subMsg model.topBar
                in
                ( { model | topBar = updatedTopBar }, cmd, UpdatedEditor )

            RandomRequested ->
                ( model, Random.generate GotRandomImage Image.random, NothingToUpdate )

            GotRandomImage partialImage ->
                let
                    newImage =
                        Image.withImage partialImage model.image
                in
                ( { model
                    | image = newImage
                    , colorWheel = updateColorWheel newImage model.colorTarget model.colorWheel
                  }
                    |> withEditIndexLast
                , Cmd.none
                , UpdatedEditor
                )

            CopyUrlToClipboard ->
                case model.url of
                    Nothing ->
                        ( model, Cmd.none, NothingToUpdate )

                    Just currentUrl ->
                        let
                            { protocol, host, port_ } =
                                currentUrl

                            url =
                                Utils.protocolToString protocol ++ host ++ Utils.portToString port_ ++ "/" ++ urlEncode model
                        in
                        ( model, copyTextToClipboard url, NothingToUpdate )

            CopyUrlToClipboardResult copiedSuccessfully ->
                let
                    message =
                        if copiedSuccessfully then
                            "URL copied!"

                        else
                            "There was an error :("
                in
                -- `4000` here is enough to make the transition visible until it removes the node from the DOM.
                ( { model | alert = Just message }
                , delay 4000 HideAlert
                , NothingToUpdate
                )

            DownloadSvg ->
                ( model, downloadSvg (), NothingToUpdate )

            DownloadSvgAsJpeg ->
                ( model, downloadSvgAsJpeg (), NothingToUpdate )

            FullscreenRequested ->
                ( model, requestFullscreen (), NothingToUpdate )

            ResetDrawing ->
                ( { model
                    | image = Image.resetImageComposition model.image
                    , editingIndex = 1
                  }
                , Cmd.none
                , UpdatedEditor
                )

            AddSimpleBlock ->
                ( { model | image = Image.appendSimpleBlock model.image }
                    |> withEditIndexLast
                , Cmd.none
                , UpdatedEditor
                )

            DuplicateAndAppendBlock editingIndex ->
                ( duplicateAndAppendBlock model editingIndex
                , Cmd.none
                , UpdatedEditor
                )

            SetEditingIndex index ->
                ( { model | editingIndex = index }
                , Cmd.none
                , NothingToUpdate
                )

            DropBlock index ->
                ( { model
                    | image = Image.dropBlockAtIndex index model.image
                    , editingIndex =
                        if index <= model.editingIndex then
                            max 0 (model.editingIndex - 1)

                        else
                            model.editingIndex
                  }
                , Cmd.none
                , UpdatedEditor
                )

            EditorKeyPress keyString ->
                let
                    maybeNewModelArrowsAndBackspace =
                        processArrowsAndBackspace model keyString

                    maybeNewModelShortcut =
                        processShortcut model keyString

                    maybeNewModelWriting =
                        processWritingInput model keyString
                in
                case maybeNewModelArrowsAndBackspace of
                    Just newModel ->
                        ( newModel, Cmd.none, UpdatedEditor )

                    Nothing ->
                        if model.keyboardInput == WritingMode then
                            case maybeNewModelWriting of
                                Just newModel ->
                                    ( newModel, Cmd.none, UpdatedEditor )

                                Nothing ->
                                    ( model, Cmd.none, NothingToUpdate )

                        else if keyString == "a" then
                            ( model, Task.attempt (always NoOp) (Browser.Dom.focus "TurnAngle"), NothingToUpdate )

                        else if keyString == "q" then
                            ( model, Random.generate GotRandomImage Image.random, UpdatedEditor )

                        else
                            case maybeNewModelShortcut of
                                Just newModel ->
                                    ( newModel, Cmd.none, NothingToUpdate )

                                Nothing ->
                                    ( model, Cmd.none, NothingToUpdate )

            -- also, see `scaleAbout` in https://github.com/ianmackenzie/elm-geometry-svg/blob/master/src/Geometry/Svg.elm
            -- and later check https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/
            Zoom _ deltaY _ mousePos ->
                ( applyZoom deltaY mousePos model, Cmd.none, UpdatedEditor )

            ColorWheelMsg subMsg ->
                let
                    ( updatedColorWheel, maybeColor, cmd ) =
                        ColorWheel.update subMsg model.colorWheel

                    updatedModel =
                        { model | colorWheel = updatedColorWheel }
                in
                case maybeColor of
                    Just color ->
                        ( { updatedModel
                            | image =
                                case model.colorTarget of
                                    Stroke ->
                                        Image.withStrokeColor color model.image

                                    Background ->
                                        Image.withBackgroundColor color model.image
                          }
                        , Cmd.map ColorWheelMsg cmd
                        , UpdatedEditor
                        )

                    Nothing ->
                        ( updatedModel, Cmd.map ColorWheelMsg cmd, NothingToUpdate )

            SetColorTarget target ->
                ( { model
                    | colorTarget = target
                    , colorWheel = updateColorWheel model.image target model.colorWheel
                  }
                , Cmd.none
                , NothingToUpdate
                )

            ExchangeColors ->
                let
                    updatedImage =
                        model.image
                            |> Image.withStrokeColor model.image.backgroundColor
                            |> Image.withBackgroundColor model.image.strokeColor
                in
                ( { model
                    | image = updatedImage
                    , colorWheel = updateColorWheel updatedImage model.colorTarget model.colorWheel
                  }
                , Cmd.none
                , UpdatedEditor
                )

            SetTurnAngleInputValue stringValue ->
                if stringValue == "" then
                    ( { model
                        | image = Image.withTurnAngle 0 model.image
                        , turnAngleInputValue = "0"
                      }
                    , Cmd.none
                    , UpdatedEditor
                    )

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
                                ( newModel, Cmd.none, NothingToUpdate )

                            else
                                ( newModel, Cmd.none, UpdatedEditor )

                        Nothing ->
                            ( { model | turnAngleInputValue = stringValue }, Cmd.none, NothingToUpdate )

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
                        ( { model
                            | image = Image.withTurnAngle (newTurnAngle op) model.image
                            , turnAngleInputValue = newTurnAngleInputValue op
                          }
                        , Cmd.none
                        , UpdatedEditor
                        )
                in
                case key of
                    "Escape" ->
                        ( model, Task.attempt (\_ -> NoOp) (Browser.Dom.blur "TurnAngle"), NothingToUpdate )

                    "ArrowUp" ->
                        updateTurnAngle (+)

                    "ArrowDown" ->
                        updateTurnAngle (-)

                    _ ->
                        ( model, Cmd.none, NothingToUpdate )

            SetKeyboardInput inputMode ->
                ( { model | keyboardInput = inputMode }
                , Cmd.none
                , NothingToUpdate
                )

            SetStrokeWidth width ->
                ( { model | image = Image.withStrokeWidth width model.image }
                , Cmd.none
                , UpdatedEditor
                )

            ToggleCurveType ->
                let
                    curve =
                        case model.image.curve of
                            Image.Curve ->
                                Image.Line

                            Image.Line ->
                                Image.Curve
                in
                ( { model | image = Image.withCurve curve model.image }
                , Cmd.none
                , UpdatedEditor
                )

            PanStarted pos ->
                ( { model | panStarted = True, lastPos = pos }
                , Cmd.none
                , NothingToUpdate
                )

            PanEnded ->
                ( { model | panStarted = False }
                , Cmd.none
                , UpdatedEditor
                )

            MouseMoved pos ->
                ( { model
                    | image = Image.move model.lastPos pos model.image
                    , lastPos = pos
                  }
                , Cmd.none
                , NothingToUpdate
                )

            SetFocus focus ->
                ( { model | focus = focus }, Cmd.none, NothingToUpdate )

            BasePolygonChanged polygon ->
                ( updateCompositionBaseAndAngle model polygon
                , Cmd.none
                , UpdatedEditor
                )

            GotImgDivPosition result ->
                case result of
                    Ok data ->
                        let
                            { x, y, width, height } =
                                data.element
                        in
                        ( { model | imgDivCenter = ( x + width / 2, y + height / 2 ), imgDivStart = ( x, y ) }
                        , Cmd.none
                        , NothingToUpdate
                        )

                    Err _ ->
                        ( model, Cmd.none, NothingToUpdate )

            WindowResized _ _ ->
                ( model
                , Cmd.batch
                    [ getImgDivPosition
                    , Cmd.map ColorWheelMsg (ColorWheel.getElementDimensions model.colorWheel)
                    ]
                , NothingToUpdate
                )

            ToggleVideo ->
                ( { model | playingVideo = not model.playingVideo }, Cmd.none, UpdatedEditor )

            ToggleColorVideo ->
                ( { model | colorInVideo = not model.colorInVideo }, Cmd.none, UpdatedEditor )

            VideoTick ->
                let
                    -- Angle stuff
                    newAngle =
                        if model.playingVideo then
                            case model.slowMotion of
                                NotSet ->
                                    model.image.turnAngle + (model.videoAngleChangeDirection * model.videoAngleChangeRate)

                                Slowly by ->
                                    model.image.turnAngle + (by * model.videoAngleChangeDirection * model.videoAngleChangeRate)

                        else
                            model.image.turnAngle

                    updateAngle angle model_ =
                        { model_
                            | image = Image.withTurnAngle angle model_.image
                            , turnAngleInputValue = String.fromFloat angle
                        }

                    -- Color stuff
                    newColor =
                        let
                            color =
                                case model.colorTarget of
                                    Stroke ->
                                        model.image.strokeColor

                                    Background ->
                                        model.image.backgroundColor

                            { h, s, v, a } =
                                Colors.toHsva color
                        in
                        if model.playingVideo && model.colorInVideo then
                            Colors.hsva (h + 0.01) s v a

                        else
                            color

                    updateColor color model_ =
                        { model_
                            | image =
                                case model.colorTarget of
                                    Stroke ->
                                        Image.withStrokeColor color model_.image

                                    Background ->
                                        Image.withBackgroundColor color model_.image
                            , colorWheel = ColorWheel.withColor color model_.colorWheel
                        }
                in
                ( updateAngle newAngle <| updateColor newColor model, Cmd.none, NothingToUpdate )

            ToggleSlowMotion ->
                case model.slowMotion of
                    Slowly _ ->
                        ( { model | slowMotion = NotSet }, Cmd.none, NothingToUpdate )

                    NotSet ->
                        ( { model | slowMotion = Slowly 0.05 }, Cmd.none, NothingToUpdate )

            SetVideoAngleChangeRate rate ->
                ( { model | videoAngleChangeRate = rate }, Cmd.none, NothingToUpdate )

            ReverseAngleChangeDirection ->
                ( { model | videoAngleChangeDirection = model.videoAngleChangeDirection * -1 }
                , Cmd.none
                , NothingToUpdate
                )

            SavedToGallery ->
                -- `4000` here is enough to make the transition visible until it removes the node from the DOM.
                ( { model | alert = Just "Image saved!" }
                , delay 4000 HideAlert
                , UpdatedGallery model.image
                )

            GotMidiEvent value ->
                -- TODO add throttle and then add updateAndSaveImageAndGallery
                -- TODO make it save when needed (it'll be fine if `processMidi` calls `update` with proper msgs
                ( processMidi value model, Cmd.none, NothingToUpdate )

            HideAlert ->
                ( { model | alert = Nothing }, Cmd.none, NothingToUpdate )

            NoOp ->
                ( model, Cmd.none, NothingToUpdate )


updateColorWheel : Image -> ColorTarget -> ColorWheel.State -> ColorWheel.State
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


processArrowsAndBackspace : Model -> String -> Maybe Model
processArrowsAndBackspace model keyPressed =
    let
        appendStep step =
            { model | image = Image.appendStepAtIndex step model.editingIndex model.image }
    in
    case keyPressed of
        "ArrowLeft" ->
            Just (appendStep L)

        "ArrowRight" ->
            Just (appendStep R)

        "ArrowUp" ->
            Just (appendStep D)

        "ArrowDown" ->
            Just (appendStep S)

        "Backspace" ->
            Just { model | image = Image.dropLastStepAtIndex model.editingIndex model.image }

        _ ->
            Nothing


processWritingInput : Model -> String -> Maybe Model
processWritingInput model keyPressed =
    let
        append step =
            Just { model | image = Image.appendStepAtIndex step model.editingIndex model.image }
    in
    if keyPressed == " " then
        append S

    else
        Maybe.andThen
            (\( key, tail ) ->
                if String.length tail > 0 then
                    -- Key pressed is something like "Shift". TODO What kind of bugs can appear from this?
                    Nothing

                else
                    append (Glyph key)
            )
            (String.uncons keyPressed)


processShortcut : Model -> String -> Maybe Model
processShortcut model keyPressed =
    let
        updateAngle deg =
            Just
                { model
                    | image = Image.withTurnAngle deg model.image
                    , turnAngleInputValue = String.fromFloat deg
                }

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
        -- RESET POSITION AND ZOOM
        --
        "c" ->
            Just { model | image = Image.centralize model.image }

        -- ITERATE / DEITERATE
        --
        "i" ->
            Just (duplicateAndAppendBlock model model.editingIndex)

        "d" ->
            Just (dropLastBlock model)

        -- VIDEO CONTROLS
        --
        -- Play / pause
        " " ->
            Just { model | playingVideo = not model.playingVideo }

        -- Slow motion
        "s" ->
            case model.slowMotion of
                Slowly _ ->
                    Just { model | slowMotion = NotSet }

                NotSet ->
                    Just { model | slowMotion = Slowly 0.05 }

        "," ->
            Just (updateVideoRate (-))

        "." ->
            Just (updateVideoRate (+))

        -- Change turn angle direction
        "r" ->
            Just { model | videoAngleChangeDirection = model.videoAngleChangeDirection * -1 }

        -- ZOOM
        --
        -- Zoom out large
        "-" ->
            Just (applyZoom 30 model.imgDivCenter model)

        -- Zoom out small
        "_" ->
            Just (applyZoom 1 model.imgDivCenter model)

        -- Zoom in Large
        "=" ->
            Just (applyZoom -30 model.imgDivCenter model)

        -- Zoom in small
        "+" ->
            Just (applyZoom -1 model.imgDivCenter model)

        -- STROKE WIDTH
        --
        "[" ->
            Just (updateStrokeWidth (-))

        "]" ->
            Just (updateStrokeWidth (+))

        -- CHANGE CURVE
        --
        "o" ->
            let
                curve =
                    case model.image.curve of
                        Image.Curve ->
                            Image.Line

                        Image.Line ->
                            Image.Curve
            in
            Just { model | image = Image.withCurve curve model.image }

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
            Nothing


strokeWidthSliderConfig : Utils.MinMaxCenterAt
strokeWidthSliderConfig =
    ( ( 0.0001, 4 ), ( 1, 0.9 ) )


videoRateSliderConfig : Utils.MinMaxCenterAt
videoRateSliderConfig =
    ( ( 0.00005, 2.000001 ), ( 1, 0.9 ) )


duplicateAndAppendBlock : Model -> Int -> Model
duplicateAndAppendBlock model editingIndex =
    { model | image = Image.duplicateBlock editingIndex model.image }
        |> withEditIndexLast


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


subscriptions : Model -> Bool -> Sub Msg
subscriptions model isVisible =
    let
        panSubs =
            if model.panStarted then
                Sub.batch
                    [ Browser.Events.onMouseMove (mousePositionDecoder MouseMoved)
                    , Browser.Events.onMouseUp (Decode.succeed PanEnded)
                    ]

            else
                Sub.none

        keyPressSub =
            if model.focus == EditorFocus then
                Browser.Events.onKeyDown (keyPressDecoder EditorKeyPress)

            else
                Sub.none

        videoSub =
            if model.playingVideo then
                Time.every framesInterval (always VideoTick)

            else
                Sub.none

        windowResize =
            Browser.Events.onResize WindowResized
    in
    if isVisible then
        Sub.batch
            [ keyPressSub
            , panSubs
            , videoSub
            , midiEvent GotMidiEvent
            , copyToClipboardResult CopyUrlToClipboardResult
            , windowResize
            , Sub.map ColorWheelMsg (ColorWheel.subscriptions model.colorWheel)
            ]

    else
        windowResize


framesInterval : Float
framesInterval =
    100


queryParser : Query.Parser PartialImage
queryParser =
    Image.queryParser


urlParser : Parser (PartialImage -> a) a
urlParser =
    Parser.s (routeFor EditorPage) <?> Image.queryParser


urlEncode : Model -> String
urlEncode model =
    routeFor EditorPage ++ Image.toQuery model.image



-- ENCODE DECODE
-- DECODE


encode : Model -> Encode.Value
encode { image } =
    Image.encode image


decoder : Decoder Model
decoder =
    Decode.map (\image -> { initialModel | image = image }) <|
        Decode.oneOf
            -- Add new model versions here!
            [ Image.decoder ]
