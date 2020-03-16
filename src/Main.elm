-- Anything other than `main` is exposed for testing only
-- TODO research how we could change this in order to only expose `main`
-- Question: Should Main module be tested? Should every test-deserving function live
--           outside Main module?


port module Main exposing
    ( Route(..)
    , main
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
import Html.Styled.Lazy exposing (lazy7)
import Icons exposing (withColor, withCss, withOnClick)
import ImageEssentials
    exposing
        ( Gallery
        , ImageAndGallery
        , ImageEssentials
        , Polygon(..)
        , Position
        , defaultImage
        , encodeImageAndGallery
        , extractImage
        , mergeAllVersions_ImageAndGalleryDecoder
        , polygonAngle
        , polygonBlock
        , replaceImage
        )
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core as LCore exposing (Block, Composition, Step(..))
import LSystem.Draw as LDraw
    exposing
        ( drawImage
        , drawImageEssentials
        , image
        , withBackgroundColor
        , withStrokeColor
        , withTurnAngle
        )
import ListExtra exposing (pairExec, pairMap)
import Task
import Time
import Url
import Url.Parser as Parser exposing (Parser, string)



-- PORTS


port saveEncodedModelToLocalStorage : Encode.Value -> Cmd msg


port saveMergedModelsVersionsAndDeleteOldOnes : Encode.Value -> Cmd msg


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

    -- Stroke Width
    , strokeWidth : Float

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
    = Editor (Maybe ImageEssentials)
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



-- IMPLEMENTATION, LOGIC, FUNCTIONS
-- MODEL


initialModel : ImageEssentials -> Gallery -> Url.Url -> Nav.Key -> Model
initialModel image gallery url navKey =
    -- Aplication heart
    { composition = image.composition
    , editingIndex = 1

    -- Storage
    , gallery = gallery
    , viewingPage = EditorPage

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

    -- Stroke Width
    , strokeWidth = 1

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
    { model | editingIndex = LCore.length model.composition - 1 }



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

        decodedLocalStorage =
            Decode.decodeValue mergeAllVersions_ImageAndGalleryDecoder localStorage

        imageAndGallery =
            combineUrlAndStorage decodedLocalStorage route

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
         , saveMergedModelsVersionsAndDeleteOldOnes (encodeImageAndGallery imageAndGallery)
         ]
            ++ updateUrl
        )
    )



-- TODO Check that new version of mergeToV3 (which returns a defaultImage sometimes)
--      doesn't affect this function AND doesn't generate redudancy


combineUrlAndStorage : Result Decode.Error ImageAndGallery -> Route -> ImageAndGallery
combineUrlAndStorage resultImageAndGalleryFromStorage route =
    case ( route, resultImageAndGalleryFromStorage ) of
        ( Editor (Just urlImage), Ok storedImageAndGallery ) ->
            { image = urlImage, gallery = storedImageAndGallery.gallery }

        ( Editor (Just urlImage), Err _ ) ->
            { image = urlImage, gallery = [] }

        ( _, Ok storedImageAndGallery ) ->
            storedImageAndGallery

        ( _, Err _ ) ->
            { image = defaultImage, gallery = [] }



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



-- EDITOR VIEW


editorView : Model -> Browser.Document Msg
editorView model =
    let
        { composition, turnAngle, strokeColor, strokeWidth, scale, translate } =
            model

        backgroundColor_ =
            model.backgroundColor
    in
    { title = "Generative Art"
    , body =
        [ div
            [ css [ width (pct 100), height (pct 100) ] ]
            [ compositionBlocksList model

            -- TODO BUG this probably doesn't work. To get around it we should have a nested record for ImageEssentials
            --      inside the model :(
            , lazy7 mainImg composition turnAngle scale translate strokeColor backgroundColor_ strokeWidth
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
            , backgroundColor (toCssColor model.backgroundColor)
            , color (toCssColor offWhite)
            ]
        ]
        [ infoAndBasicControls model
        , colorControls model.backgroundColor model.strokeColor
        , videoControls model.videoAngleChangeRate model.playingVideo
        , turnAngleControl model.turnAngle
        , strokeWidthControl model.strokeWidth
        , curatedSettings
        , controlsList
        ]


infoAndBasicControls : Model -> Html Msg
infoAndBasicControls model =
    let
        ( countD, countOthers ) =
            LCore.stepsLength model.composition

        stateLengthString =
            String.fromInt countD ++ ", " ++ String.fromInt countOthers

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
        , button [ onClick (DuplicateAndAppendBlock model.editingIndex) ] [ text "Duplicate selected block" ]
        , button [ onClick DropLastBlock ] [ text "Delete top block" ]
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
        [ span [ css [ display block ] ] [ text ("Video Playback:" ++ String.fromFloat (angleChangeRate * 20) ++ "x") ]
        , button [ onClick TogglePlayingVideo ] [ text playPauseText ]
        , button [ onClick ReverseAngleChangeDirection ] [ text "Reverse" ]
        , sliderInput SetVideoAngleChangeRate angleChangeRate 0.0001 0.0501 0.0005
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
        , sliderInput SetStrokeWidth width 0.001 2.001 0.005
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
            model.composition
                |> LCore.toList
                |> List.indexedMap
                    (blockBox
                        model.editingIndex
                        model.turnAngle
                        model.strokeColor
                        model.backgroundColor
                    )
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


blockBox : Int -> Float -> Color -> Color -> Int -> Block -> Html Msg
blockBox editingIndex turnAngle strokeColor backgroundColor index transform =
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
        [ image (LCore.fromList [ transform ])
            |> withTurnAngle turnAngle
            |> withStrokeColor strokeColor
            |> withBackgroundColor backgroundColor
            |> drawImage
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
mainImg : Composition -> Float -> Float -> Position -> Color -> Color -> Float -> Html Msg
mainImg composition turnAngle scale translate strokeColor backgroundColor_ strokeWidth =
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
        [ drawImageEssentials
            (ImageEssentials
                composition
                turnAngle
                backgroundColor_
                strokeColor
                strokeWidth
                translate
                scale
            )
            (Just "MainSVG")
            Nothing
            False
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
            let
                newImage =
                    extractImage newModel

                newImageAndGallery =
                    ImageAndGallery newImage newModel.gallery
            in
            ( newModel
            , Cmd.batch
                [ saveEncodedModelToLocalStorage (encodeImageAndGallery newImageAndGallery)
                , replaceUrl newModel.navKey newImage
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

        AddSimpleBlock ->
            updateAndSaveImageAndGallery <| appendBlock [ D ] model

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
                    | composition = LCore.dropBlockAtIndex index model.composition
                    , editingIndex =
                        if index <= model.editingIndex then
                            max 0 (model.editingIndex - 1)

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

        SetStrokeWidth width ->
            updateAndSaveImageAndGallery <| { model | strokeWidth = width }

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

        SetVideoAngleChangeRate rate ->
            ( { model | videoAngleChangeRate = rate }, Cmd.none )

        ReverseAngleChangeDirection ->
            ( { model | videoAngleChangeDirection = model.videoAngleChangeDirection * -1 }, Cmd.none )

        SavedToGallery ->
            updateAndSaveImageAndGallery <| { model | gallery = extractImage model :: model.gallery }

        ViewingPage page ->
            ( { model | viewingPage = page }, Cmd.none )

        RemovedFromGallery index ->
            updateAndSaveImageAndGallery <| { model | gallery = ListExtra.dropIndex index model.gallery }

        DuplicateToEdit index ->
            let
                image =
                    ListExtra.getAt index model.gallery
                        |> Maybe.withDefault (extractImage model)

                newModel =
                    replaceImage image model
            in
            updateAndSaveImageAndGallery <| { newModel | viewingPage = EditorPage }


processKey : Model -> String -> ( Model, Bool )
processKey model keyPressed =
    let
        appendStep step =
            { model | composition = LCore.appendStepAtIndex step model.editingIndex model.composition }
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
            ( { model | scale = 1, translate = ( 0, 0 ) }, True )

        "Backspace" ->
            ( { model | composition = LCore.dropLastStepAtIndex model.editingIndex model.composition }, True )

        "i" ->
            ( duplicateAndAppendBlock model model.editingIndex, True )

        "d" ->
            ( dropLastBlock model, True )

        _ ->
            ( model, False )


duplicateAndAppendBlock : Model -> Int -> Model
duplicateAndAppendBlock model editingIndex =
    let
        maybeDuplicatedBlock =
            LCore.getBlockAtIndex editingIndex model.composition
    in
    case maybeDuplicatedBlock of
        Just newBlock ->
            { model | composition = LCore.appendBlock newBlock model.composition }
                |> modelWithEditIndexLast

        Nothing ->
            model


appendBlock : Block -> Model -> Model
appendBlock block model =
    { model | composition = LCore.appendBlock block model.composition }
        |> modelWithEditIndexLast


dropLastBlock : Model -> Model
dropLastBlock model =
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
    Parser.map Editor ImageEssentials.urlParser


{-| Current version of Url building and parsing
-}
replaceUrl : Nav.Key -> ImageEssentials -> Cmd msg
replaceUrl key image =
    {--
            Note about Nav.replaceUrl: Browsers may rate-limit this function by throwing an
            exception. The discussion here suggests that the limit is 100 calls per 30 second
            interval in Safari in 2016. It also suggests techniques for people changing the
            URL based on scroll position.

            https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl
    --}
    Nav.replaceUrl key (ImageEssentials.toUrlPathString image)



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
