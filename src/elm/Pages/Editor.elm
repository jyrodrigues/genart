port module Pages.Editor exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , decoder
    , encode
    , encodeIntoUrl
    , initialCmd
    , initialModel
    , subscriptions
    , update
    , view
    , withImage
    , withPartialImage
    )

import Browser
import Browser.Dom exposing (Element)
import Browser.Events
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
        , inset
        , left
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
import LSystem.Draw exposing (drawBlocks, drawImage)
import LSystem.Image as Image
    exposing
        ( Image
        , PartialImage
        , Polygon(..)
        , Position
        , defaultImage
        , withImage
        )
import Midi exposing (adjustInputForStrokeWidth)
import Random
import Routes exposing (Page(..), routeFor)
import Set exposing (Set)
import Svg.Styled exposing (Svg)
import Task
import Time
import Url.Parser as Parser exposing (Parser)
import Utils exposing (delay, floatModBy)



-- PORTS


port downloadSvg : () -> Cmd msg


port downloadSvgAsJpeg : () -> Cmd msg


port requestFullscreen : () -> Cmd msg


port midiEvent : (Encode.Value -> msg) -> Sub msg



-- TYPES AND MSG
-- MSG


{-| TODO: Rename Msgs: put all verbs in past tense and choose better words.
-}
type
    Msg
    -- Global keyboard listener
    -- TODO change to `KeyPress Where String`
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
      -- Stroke width
    | SetStrokeWidth Float
      -- Download
    | DownloadSvg
    | DownloadSvgAsJpeg
      -- Focus
    | SetFocus Focus
      -- URL
      -- Video
    | ToggleVideo Video
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
    , colorWheel : ColorWheel.Model
    , colorTarget : ColorTarget

    -- Browser Focus
    , focus : Focus

    -- Video
    , videoAngleChangeRate : Float
    , slowMotion : SlowMotion
    , videoAngleChangeDirection : Float
    , playingVideo : Set Video

    -- Input controls value
    , turnAngleInputValue : String

    -- Alert Popup
    , alertActive : Bool
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


type ColorTarget
    = Stroke
    | Background


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
    { image = defaultImage
    , editingIndex = 1

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
            |> ColorWheel.withColor defaultImage.strokeColor
    , colorTarget = Stroke

    -- Browser Focus
    , focus = EditorFocus

    -- Video
    , videoAngleChangeRate = 0.01
    , slowMotion = NotSet
    , playingVideo = Set.empty
    , videoAngleChangeDirection = 1

    -- Input controls value
    , turnAngleInputValue = String.fromFloat defaultImage.turnAngle

    -- Alert Popup
    , alertActive = False
    }


initialCmd : Model -> Cmd Msg
initialCmd model =
    Cmd.batch
        [ getImgDivPosition
        , Cmd.map ColorWheelMsg (ColorWheel.getElementDimensions model.colorWheel)
        ]



-- WITH* PATTERN


withImage : Image -> Model -> Model
withImage image model =
    { model
        | image = image
        , colorWheel = updateColorWheel image model.colorTarget model.colorWheel
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



-- VIEW


view : Model -> Browser.Document Msg
view model =
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

             --, fixedControlsButtons
             ]
                ++ alert
            )
            |> toUnstyled
        ]
    }


controlPanel : Model -> Html Msg
controlPanel model =
    C.fixedDiv
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
        , colorControls model.colorTarget model.colorWheel model.playingVideo
        , videoControls model.videoAngleChangeRate model.playingVideo model.slowMotion
        , turnAngleControl model.turnAngleInputValue
        , strokeWidthControl model.image.strokeWidth
        , curatedSettings

        --, controlsList
        , C.controlBlock "Info"
            [ p [ css [ overflowWrap breakWord, fontSize (px 14) ] ] [ text (Image.imageStepsLenthString model.image) ]
            , p [ css [ overflowWrap breakWord, fontSize (px 14) ] ] [ text (Image.blockBlueprintString model.editingIndex model.image) ]
            ]
        ]


infoAndBasicControls : Html Msg
infoAndBasicControls =
    C.controlBlockFlex
        [ C.anchorButtonHalf routeFor.gallery "Gallery"
        , C.primaryButtonHalf SavedToGallery "Save"
        , C.primaryButtonHalf FullscreenRequested "Full"
        , C.primaryButtonHalf DownloadSvg "Down"
        , C.primaryButtonHalf ResetDrawing "Reset"
        , C.primaryButtonHalf RandomRequested "Rand"
        , C.primaryButtonHalf DownloadSvgAsJpeg "JPEG"
        ]


colorControls : ColorTarget -> ColorWheel.Model -> Set Video -> Html Msg
colorControls colorTarget colorWheel videoSet =
    let
        text videoType name =
            if Set.member videoType videoSet then
                "Stop " ++ name

            else
                "Play " ++ name
    in
    C.controlBlock "Color"
        [ div [ css [ width (pct 100) ] ]
            [ div [ css [ displayFlex, flexWrap wrap ] ]
                [ C.primaryButtonSelectable (colorTarget == Stroke) (SetColorTarget Stroke) "Stroke"
                , C.primaryButtonSelectable (colorTarget == Background) (SetColorTarget Background) "Background"
                , C.primaryButton (ToggleVideo video.changeColorLinear) (text video.changeColorLinear "linear")
                , C.primaryButton (ToggleVideo video.changeColorSinusoidal) (text video.changeColorSinusoidal "sinusoidal")
                , C.primaryButtonHalf ExchangeColors "Exchange"
                ]
            , div [ css [ padding2 (px 10) zero ] ] [ Html.Styled.map ColorWheelMsg (ColorWheel.view colorWheel) ]
            ]
        ]


videoControls : Float -> Set Video -> SlowMotion -> Html Msg
videoControls angleChangeRate playingVideo slowMotion =
    let
        playPauseText =
            if Set.member video.changeAngle playingVideo then
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
    C.controlBlock "Video"
        [ span [ css [ display block, marginBottom (px 10) ] ]
            [ text (truncateFloatString 5 (String.fromFloat (angleChangeRate * 1000 / framesInterval)) ++ "x")
            ]
        , div []
            [ span [] [ text "Slow Motion: " ]
            , span [] [ text slowMotionText ]
            ]
        , div [ css [ displayFlex, flexWrap wrap ] ]
            [ C.primaryButton (ToggleVideo video.changeAngle) playPauseText
            , C.primaryButton ToggleSlowMotion "Slow Motion"
            , C.primaryButton ReverseAngleChangeDirection "Reverse"
            ]

        -- Magic values:
        -- min: 0.00005 * 20 = 0.001 degrees/second
        -- max: 2 * 20 = 40 degrees/second
        -- center: 1 * 20 degrees/second at 90% of slider
        , sliderExponentialInput SetVideoAngleChangeRate angleChangeRate videoRateSliderConfig
        ]


turnAngleControl : String -> Html Msg
turnAngleControl turnAngleInputValue =
    C.controlBlock "Angle"
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
    C.controlBlock "Line width"
        -- Magic values:
        -- min: 0.0001px
        -- max: 4px
        -- center: 1px at 90% of slider
        [ span [ css [ display block ] ] [ text <| truncateFloatString 6 (String.fromFloat width) ]
        , sliderExponentialInput SetStrokeWidth width strokeWidthSliderConfig
        , C.primaryButton (SetStrokeWidth 1) "Reset"
        ]


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


compositionBlocksList : Model -> Html Msg
compositionBlocksList model =
    let
        compositionBlocks =
            model.image
                |> drawBlocks
                |> List.indexedMap (blockBox model.editingIndex model.image.strokeColor)
                |> List.reverse
    in
    C.fixedDiv
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
        (C.primaryButtonStyled [ marginBottom (px 20) ] AddSimpleBlock "Add new block" :: compositionBlocks)


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
    C.fixedDiv
        [ css
            [ backgroundColor (toCssColor image.backgroundColor)
            , position fixed
            , height (pct 100)
            , width (pct layout.mainImg)
            , left (pct layout.transformsList)
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



-- UPDATE


updateSvgPathAndBoundariesIfNeeded : ( Model, Cmd Msg, ExternalMsg ) -> ( Model, Cmd Msg, ExternalMsg )
updateSvgPathAndBoundariesIfNeeded ( model, cmd, externalMsg ) =
    ( { model | image = Image.updateSvgPathAndBoundaries model.image }, cmd, externalMsg )


update : Msg -> Model -> ( Model, Cmd Msg, ExternalMsg )
update msg model =
    updateSvgPathAndBoundariesIfNeeded <|
        case msg of
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
                    ( newModel, shouldUpdate ) =
                        processKey model keyString
                in
                if keyString == "a" then
                    ( model, Task.attempt (\_ -> NoOp) (Browser.Dom.focus "TurnAngle"), NothingToUpdate )

                else if keyString == "q" then
                    ( model, Random.generate GotRandomImage Image.random, UpdatedEditor )

                else if shouldUpdate then
                    ( newModel, Cmd.none, NothingToUpdate )

                else
                    ( model, Cmd.none, NothingToUpdate )

            -- also, see `scaleAbout` in https://github.com/ianmackenzie/elm-geometry-svg/blob/master/src/Geometry/Svg.elm
            -- and later check https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/
            Zoom _ deltaY _ mousePos ->
                ( applyZoom deltaY mousePos model, Cmd.none, UpdatedEditor )

            ColorWheelMsg subMsg ->
                let
                    ( updatedColorWheel, _, msgType ) =
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
                        ( { model
                            | colorWheel = updatedColorWheel
                            , image = image
                          }
                        , Cmd.none
                        , UpdatedEditor
                        )

                    ColorWheel.SameColor ->
                        ( { model | colorWheel = updatedColorWheel }, Cmd.none, NothingToUpdate )

            SetColorTarget target ->
                ( { model
                    | colorTarget = target
                    , colorWheel = updateColorWheel model.image target model.colorWheel
                  }
                , Cmd.none
                , NothingToUpdate
                )

            ExchangeColors ->
                ( { model
                    | image =
                        model.image
                            |> Image.withStrokeColor model.image.backgroundColor
                            |> Image.withBackgroundColor model.image.strokeColor
                    , colorWheel = updateColorWheel model.image model.colorTarget model.colorWheel
                  }
                , Cmd.none
                , UpdatedEditor
                )

            SetTurnAngleInputValue stringValue ->
                if stringValue == "" then
                    ( { model
                        | image = Image.withTurnAngle 0 model.image
                        , turnAngleInputValue = stringValue
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
                            if Set.member video.changeAngle model.playingVideo then
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

            SetStrokeWidth width ->
                ( { model | image = Image.withStrokeWidth width model.image }
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

            ToggleVideo videoType ->
                if Set.member videoType model.playingVideo then
                    -- When video stopped save model in URL and Local Storage
                    -- N.B. Copying the URL while a playing video will copy the last image saved
                    -- i.e. before playing video or the last not-angle edit while playing.
                    ( { model | playingVideo = Set.remove videoType model.playingVideo }
                    , Cmd.none
                    , UpdatedEditor
                    )

                else
                    ( { model | playingVideo = Set.insert videoType model.playingVideo }, Cmd.none, NothingToUpdate )

            VideoTick ->
                let
                    -- Angle stuff
                    newAngle =
                        if Set.member video.changeAngle model.playingVideo then
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

                            numberOfSectors =
                                8

                            sectorSize =
                                2 * pi / numberOfSectors

                            sectorPosition =
                                floatModBy sectorSize (h + 0.01) / sectorSize
                        in
                        if Set.member video.changeColorLinear model.playingVideo then
                            Colors.hsva (h + 0.01) s v a

                        else if Set.member video.changeColorSinusoidal model.playingVideo then
                            if sectorPosition < 0.5 then
                                Colors.hsva (h + 0.01) s (1 - sin (sectorPosition / 0.5 * pi) * 0.5) a

                            else
                                Colors.hsva (h + 0.01) (1 - sin ((sectorPosition - 0.5) / 0.5 * pi) * 0.5) v a

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
                ( { model | alertActive = True }
                , delay 4000 HideAlert
                , UpdatedGallery model.image
                )

            GotMidiEvent value ->
                -- TODO add throttle and then add updateAndSaveImageAndGallery
                -- TODO make it save when needed (it'll be fine if `processMidi` calls `update` with proper msgs
                ( processMidi value model, Cmd.none, NothingToUpdate )

            HideAlert ->
                ( { model | alertActive = False }, Cmd.none, NothingToUpdate )

            NoOp ->
                ( model, Cmd.none, NothingToUpdate )


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
                { model
                    | playingVideo =
                        if Set.member video.changeAngle model.playingVideo then
                            Set.remove video.changeAngle model.playingVideo

                        else
                            Set.insert video.changeAngle model.playingVideo
                }

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

        "h" ->
            appendStep (Letter 'h')

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
            ( { model
                | playingVideo =
                    if Set.member video.changeAngle model.playingVideo then
                        Set.remove video.changeAngle model.playingVideo

                    else
                        Set.insert video.changeAngle model.playingVideo
              }
            , True
              -- Saving because we don't save while video is being played
            )

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


strokeWidthSliderConfig : Utils.MinMaxCenterAt
strokeWidthSliderConfig =
    ( ( 0.0001, 4 ), ( 1, 0.9 ) )


videoRateSliderConfig : Utils.MinMaxCenterAt
videoRateSliderConfig =
    ( ( 0.00005, 2 ), ( 1, 0.9 ) )


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
            if Set.isEmpty model.playingVideo then
                Sub.none

            else
                Time.every framesInterval (always VideoTick)

        windowResize =
            Browser.Events.onResize WindowResized
    in
    if isVisible then
        Sub.batch
            [ keyPressSub
            , panSubs
            , videoSub
            , midiEvent GotMidiEvent
            , windowResize
            , Sub.map ColorWheelMsg (ColorWheel.subscriptions model.colorWheel)
            ]

    else
        windowResize


framesInterval : Float
framesInterval =
    50


queryParser : Parser (PartialImage -> a) a
queryParser =
    Parser.query Image.queryParser


encodeIntoUrl : Model -> String
encodeIntoUrl model =
    routeFor.editor ++ Image.toQuery model.image



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
