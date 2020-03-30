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
import Colors exposing (Color, offWhite, toCssColor)
import Css
    exposing
        ( absolute
        , active
        , auto
        , backgroundColor
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
        , color
        , contentBox
        , cursor
        , display
        , fixed
        , fontFamily
        , fontSize
        , height
        , hidden
        , hover
        , inlineBlock
        , inset
        , left
        , margin
        , margin2
        , margin3
        , minWidth
        , none
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
        , sansSerif
        , scroll
        , solid
        , transparent
        , vw
        , width
        , zero
        )
import Css.Media as Media exposing (withMedia)
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
import Svg.Styled exposing (Svg)
import Task
import Time
import Url
import Url.Parser as Parser exposing (Parser, string)



-- PORTS


port saveEncodedModelToLocalStorage : Encode.Value -> Cmd msg


port downloadSvg : () -> Cmd msg


port midiEvent : (Encode.Value -> msg) -> Sub msg



-- TYPES
-- MSG


{-| TODO: Rename Msgs: put all verbs in past tense and choose better words.
-}
type Msg
    = ViewingPage Page
      -- Global keyboard listener
    | KeyPress String
      -- Main commands
    | AddSimpleBlock
    | ResetDrawing
    | BasePolygonChanged Polygon
    | DuplicateAndAppendBlock Int
    | DropLastBlock
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
      -- Stroke width
    | SetStrokeWidth Float
      -- Download
    | DownloadSvg
      -- Focus
    | SetFocus Focus
      -- URL
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
      -- Video
    | TogglePlayingVideo
    | SetVideoAngleChangeRate Float
    | ReverseAngleChangeDirection
      -- MIDI
    | GotMidiEvent Encode.Value



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
    = KeyboardEditing
    | TurnAngleInput


type SlowMotion
    = NotSet
    | Slowly Float



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

    -- Browser Focus
    , focus = KeyboardEditing

    -- Url
    , url = url
    , navKey = navKey

    -- Video
    , videoAngleChangeRate = 0.01
    , slowMotion = NotSet
    , playingVideo = False
    , videoAngleChangeDirection = 1
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
                    , boxSizing borderBox
                    , overflow scroll
                    ]
                ]
                (List.indexedMap imageBox model.gallery)
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
    { title = "Generative Art"
    , body =
        [ div
            [ css [ width (pct 100), height (pct 100) ] ]
            [ compositionBlocksList model
            , lazy mainImg model.image
            , controlPanel model
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
            , backgroundColor (toCssColor Colors.darkGray)
            , color (toCssColor offWhite)
            ]
        ]
        [ infoAndBasicControls model
        , colorControls model.image.backgroundColor model.image.strokeColor
        , videoControls model.videoAngleChangeRate model.playingVideo
        , turnAngleControl model.image.turnAngle
        , strokeWidthControl model.image.strokeWidth
        , curatedSettings
        , controlsList
        ]


infoAndBasicControls : Model -> Html Msg
infoAndBasicControls model =
    controlBlock
        [ button [ onClick (ViewingPage GalleryPage) ] [ text "Go to Gallery" ]
        , button [ onClick ResetDrawing ] [ text "ResetDrawing" ]
        , button [ onClick (DuplicateAndAppendBlock model.editingIndex) ] [ text "Duplicate selected block" ]
        , button [ onClick DropLastBlock ] [ text "Delete top block" ]
        , p [] [ text (Image.imageStepsLenthString model.image) ]
        , p [] [ text (Image.blockBlueprintString model.editingIndex model.image) ]
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
                , text (Colors.toString color)
                ]
    in
    controlBlock
        [ colorControl "BgColor" SetBackgroundColor "Change background color" backgroundColor
        , br [] []
        , colorControl "StrokeColor" SetStrokeColor "Change stroke color" strokeColor
        ]


videoControls : Float -> Bool -> Html Msg
videoControls angleChangeRate playingVideo =
    let
        playPauseText =
            if playingVideo then
                "Stop"

            else
                "Play"
    in
    controlBlock
        [ span [ css [ display block ] ]
            [ text
                ("Video Playback:"
                    ++ String.fromFloat (angleChangeRate * 1000 / framesInterval)
                    ++ "x"
                )
            ]
        , button [ onClick TogglePlayingVideo ] [ text playPauseText ]
        , button [ onClick ReverseAngleChangeDirection ] [ text "Reverse" ]
        -- Magic values:
        -- min: 0.00005 * 20 = 0.001 degrees/second
        -- max: 2 * 20 = 40 degrees/second
        -- center: 1 * 20 degrees/second at 90% of slider
        , sliderExponentialInput SetVideoAngleChangeRate angleChangeRate 0.00005 1 2 0.9
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


strokeWidthControl : Float -> Html Msg
strokeWidthControl width =
    controlBlock
        [ text ("Line width: " ++ String.fromFloat width)
        -- Magic values:
        -- min: 0.0001px
        -- max: 4px
        -- center: 1px at 90% of slider
        , sliderExponentialInput SetStrokeWidth width 0.0001 1 4 0.9
        , button [ onClick (SetStrokeWidth 1) ] [ text "Reset" ]
        ]


sliderInput : (Float -> msg) -> Float -> Float -> Float -> Float -> Html msg
sliderInput msg oldValue min_ max_ step_ =
    let
        onInputCallback stringValue =
            case String.toFloat stringValue of
                Just newValue ->
                    msg newValue

                Nothing ->
                    msg oldValue
    in
    input
        [ type_ "range"
        , Html.Styled.Attributes.min (String.fromFloat min_)
        , Html.Styled.Attributes.max (String.fromFloat max_)
        , Html.Styled.Attributes.step (String.fromFloat step_)
        , value <| String.fromFloat oldValue
        , onInput onInputCallback
        , css [ display block, Css.width (pct 100) ]
        ]
        []


{-|

    - minValue and maxValue are always enforced; while
    - centerValue can be thought of as a center of gravity
    - centerAt should be a percentage (0 ~ 1), the point in the slider where centerValue will target

-}
sliderExponentialInput : (Float -> msg) -> Float -> Float -> Float -> Float -> Float -> Html msg
sliderExponentialInput msg oldValue minValue centerValue maxValue centerAt =
    let
        {--
            Based on `centerAt` and `maxValue`:
            Convert from linear (0 ~ 1) to exponential (0.000001 ~ 4)

            Take f(x) = a*b^x;
            Where f(centerAt) = centerValue;
            And f(1) = maxValue;
        --}
        exponent =
            if centerAt == 1 then
                1 / 1.0e-8
            else
                1 / (1 - centerAt)

        b =
            (maxValue / centerValue) ^ exponent

        a =
            maxValue ^ (1 - exponent) * centerValue ^ exponent

        toExponential zeroToOne =
            a * b ^ zeroToOne

        fromExponential value =
            logBase b (value / a)

        {--
            Linearly adjusting from e.g. (0.000001 ~ 4) into (0.01 ~ 4)

            N.B. `fromExponential` isn't defined for values below or equal to 0.
            *But* we restrict it even further with `toExponential 0`.
        --}
        magicN =
            fromExponential minValue

        magicAdjust value =
            (value * (1 - magicN)) + magicN

        magicReverse value =
            (value - magicN) / (1 - magicN)

        onInputCallback stringValue =
            case String.toFloat stringValue of
                Just newValue ->
                    msg (toExponential <| magicAdjust newValue)

                Nothing ->
                    msg oldValue
    in
    input
        [ type_ "range"
        , Html.Styled.Attributes.min "0.0001"
        , Html.Styled.Attributes.max "1"
        , Html.Styled.Attributes.step "0.0001"
        , value <| String.fromFloat <| magicReverse <| fromExponential oldValue
        , onInput onInputCallback
        , css [ display block, Css.width (pct 100) ]
        ]
        []


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


controlBlock : List (Html Msg) -> Html Msg
controlBlock =
    div [ css [ padding (px 10), borderBottom3 (px 1) solid (toCssColor Colors.black), fontFamily Css.monospace ] ]


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
            ]
        ]
        (primaryButton AddSimpleBlock "Add new block" :: compositionBlocks)


primaryButton : Msg -> String -> Html Msg
primaryButton msg btnText =
    let
        lightGray =
            toCssColor Colors.lightGray

        darkGray =
            toCssColor Colors.darkGray
    in
    button
        [ css
            [ color lightGray
            , backgroundColor transparent
            , border3 (px 2) solid lightGray
            , borderRadius (px 3)
            , height (px 32)
            , width (pct 50)
            , minWidth (px 150)
            , withMedia [ Media.all [ Media.maxWidth (px 1160), Media.minWidth (px 650) ] ]
                [ minWidth (px 85)
                , height (px 60)
                ]
            , withMedia [ Media.all [ Media.maxWidth (px 650) ] ]
                [ minWidth (px 55)
                , height (px 70)
                ]
            , margin2 (px 17) auto
            , display block
            , cursor pointer

            -- TODO add font files and @font-face:
            -- https://stackoverflow.com/questions/107936/how-to-add-some-non-standard-font-to-a-website
            -- https://fonts.google.com/specimen/Roboto?selection.family=Roboto
            -- Research best fallback option
            --, fontFamilies [ "Roboto" ]
            , fontFamily sansSerif
            , fontSize (px 16)
            , hover
                [ border Css.unset
                , backgroundColor lightGray
                , color darkGray
                , active [ boxShadow6 inset zero zero (px 2) (px 1) darkGray ]
                ]
            ]
        , onClick msg
        ]
        [ text btnText ]


blockBox : Int -> Color -> Int -> Svg Msg -> Html Msg
blockBox editingIndex strokeColor index blockSvg =
    let
        borderOnSelected =
            if editingIndex == index then
                [ border3 (px 3) solid (toCssColor strokeColor)
                , boxSizing borderBox
                ]

            else
                []
    in
    div
        [ css
            ([ height (vw 8)
             , width (pct 85)
             , margin3 zero auto (px 20)
             , cursor pointer
             , borderRadius (px 3)
             , hover
                [ border3 (px 5) solid (toCssColor strokeColor)

                -- Changing the border style for aesthetic reasons.
                -- This rule is more specific so it overrides `borderOnSelected`.
                , boxSizing contentBox

                -- Compensate top and bottom borders.
                , margin3 (px -5) auto (px 15)
                ]

             -- Making position relative to allow for Icon placement to the right.
             , position relative
             ]
                ++ borderOnSelected
            )
        , onClick (SetEditingIndex index)
        ]
        [ blockSvg
        , Icons.trash
            |> withColor Colors.red_
            |> withOnClick (DropFromState index)
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


{-| N.B. This function has 7 arguments because otherwise it wouldn't be lazily evaluated since `lazy*` woks via
reference equality and not deep equality.
-}
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
        [ drawImage (Just "MainSVG") Nothing False image
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
            updateAndSaveImageAndGallery newModel =
                ( newModel
                , Cmd.batch
                    [ saveEncodedModelToLocalStorage (encodeModel newModel)
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

            DropLastBlock ->
                updateAndSaveImageAndGallery <| dropLastBlock model

            KeyPress keyString ->
                let
                    ( newModel, shouldUpdate ) =
                        processKey model keyString
                in
                if shouldUpdate then
                    updateAndSaveImageAndGallery newModel

                else
                    ( model, Cmd.none )

            -- also, see `scaleAbout` in https://github.com/ianmackenzie/elm-geometry-svg/blob/master/src/Geometry/Svg.elm
            -- and later check https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/
            Zoom _ deltaY _ mousePos ->
                updateAndSaveImageAndGallery <| applyZoom deltaY mousePos model

            SetEditingIndex index ->
                ( { model | editingIndex = index }, Cmd.none )

            DropFromState index ->
                updateAndSaveImageAndGallery <|
                    { model
                        | image = Image.dropBlockAtIndex index model.image
                        , editingIndex =
                            if index <= model.editingIndex then
                                max 0 (model.editingIndex - 1)

                            else
                                model.editingIndex
                    }

            SetBackgroundColor color ->
                updateAndSaveImageAndGallery <| { model | image = Image.withBackgroundColor color model.image }

            SetStrokeColor color ->
                updateAndSaveImageAndGallery <| { model | image = Image.withStrokeColor color model.image }

            SetTurnAngle turn ->
                let
                    newModel =
                        { model | image = Image.withTurnAngle turn model.image }
                in
                if model.playingVideo then
                    -- Don't update URL and Local Storage on each video step
                    ( newModel, Cmd.none )

                else
                    updateAndSaveImageAndGallery newModel

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

            SetVideoAngleChangeRate rate ->
                ( { model | videoAngleChangeRate = rate }, Cmd.none )

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
                    image =
                        ListExtra.getAt index model.gallery
                            |> Maybe.withDefault model.image
                in
                updateAndSaveImageAndGallery <|
                    { model
                        | viewingPage = EditorPage
                        , image = image
                    }

            GotMidiEvent value ->
                if model.viewingPage == GalleryPage then
                    ( model, Cmd.none )

                else
                    -- TODO add throttle and then add updateAndSaveImageAndGallery
                    ( processMidi value model, Cmd.none )


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
                -- Turn Angle
                { model
                    | image =
                        model.image
                            -- Magic numbers, after testing with Arturia MiniLab mk2
                            |> Image.withTurnAngle
                                (model.image.turnAngle
                                    + (velocityPosition - 64)
                                    * model.videoAngleChangeRate
                                )
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
            { model | image = Image.appendStepAtIndex step model.editingIndex model.image }
    in
    case keyPressed of
        "ArrowLeft" ->
            ( appendStep L, True )

        "ArrowRight" ->
            ( appendStep R, True )

        "ArrowUp" ->
            ( appendStep D, True )

        "ArrowDown" ->
            ( appendStep S, True )

        " " ->
            ( { model | image = Image.centralize model.image }, True )

        "Backspace" ->
            ( { model | image = Image.dropLastStepAtIndex model.editingIndex model.image }, True )

        "i" ->
            ( duplicateAndAppendBlock model model.editingIndex, True )

        "d" ->
            ( dropLastBlock model, True )

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
                Browser.Events.onKeyDown keyPressDecoder

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
    Sub.batch [ keyPressSub, panSubs, playingVideoSub, midiEvent GotMidiEvent ]


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


galleryParser : Parser (Route -> a) a
galleryParser =
    Parser.map Gallery (Parser.s "gallery")


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
    Nav.replaceUrl key (Image.toQuery image)



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
