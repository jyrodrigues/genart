port module Main exposing (main)

import Browser
import Browser.Dom exposing (Element)
import Browser.Events
import Browser.Navigation as Nav
import Colors exposing (Color, offWhite, toCssColor)
import Css
    exposing
        ( backgroundColor
        , block
        , borderBottom3
        , borderBox
        , borderLeft3
        , borderRight3
        , boxSizing
        , color
        , display
        , fixed
        , fontFamily
        , fontSize
        , height
        , hidden
        , left
        , margin
        , overflow
        , overflowX
        , overflowY
        , padding
        , pct
        , position
        , px
        , right
        , scroll
        , solid
        , width
        , zero
        )
import Html
import Html.Styled exposing (Html, b, br, button, div, h2, input, label, p, span, text, toUnstyled)
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
import Icons exposing (withColor, withConditionalColor, withOnClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core as LCore exposing (Block, Composition, Step(..))
import LSystem.Draw as LDraw
    exposing
        ( drawImage
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
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


port saveModelToLocalStorage : Encode.Value -> Cmd msg


port downloadSvg : () -> Cmd msg



-- MSG, ROUTE AND FLAGS


{-| TODO: Rename Msgs: put all verbs in past tense and choose better words.
-}
type
    Msg
    -- Global keyboard listener
    = KeyPress String
      -- Main commands
    | ResetDrawing
    | BasePolygonChanged Polygon
    | Iterate Int
    | Deiterate
    | SetEditingIndex Int
    | DropFromState Int
      -- Pan and Zoom
    | GotImgDivPosition (Result Browser.Dom.Error Element)
    | PanStarted ( Float, Float )
    | PanEnded
    | MouseMoved ( Float, Float )
    | Zoom Float Float ShiftKey ( Float, Float )
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


type alias ShiftKey =
    Bool


type Route
    = Home Composition


type alias Flags =
    Encode.Value



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


{-|

    - Parse URL
    - Parse localStorage
        - If URL is invalid then tryGetLastestFromStorage
        - Else create new.

-}
init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init localStorage url navKey =
    case Maybe.map (\(Home composition) -> composition) (parseUrlToRoute url) of
        -- Got a valid Composition from the URL
        Just composition ->
            let
                model =
                    defaultModel url navKey
                        |> withComposition composition
                        |> withEditingIndexOnTopBlock
            in
            ( model, getImgDivPosition )

        Nothing ->
            case Decode.decodeValue modelDecoder localStorage of
                -- Got a valid composition from localStorage
                Ok modelWithoutUrlAndKey ->
                    let
                        model =
                            modelWithoutUrlAndKey url navKey
                    in
                    ( model
                    , Cmd.batch
                        [ replaceUrl navKey model.image.composition
                        , getImgDivPosition
                        ]
                    )

                -- Didn't found a valid composition: default to squareComposition
                Err err ->
                    let
                        _ =
                            Debug.log "Debug.log - Error while decoding localStorage" err

                        model =
                            defaultModel url navKey
                    in
                    ( model
                    , Cmd.batch
                        [ saveModelToLocalStorage (encodeModel model)
                        , replaceUrl navKey model.image.composition
                        , getImgDivPosition
                        ]
                    )



-- MODEL AND STORAGE


type alias Model =
    { image : ImageEssentials
    , editState : EditState
    , gallery : Gallery
    , videoSettings : VideoSettings
    , support : ModelSupport
    }



{--N.B.
    There are (at least) three alternatives as to how organize the Model type:
    - A plain flat Record;
    - Extensible Records;
    - Nesting.

    The plain flat option was working great but got too cumbersome to keep on going.
    Then I tested with extensible records, but as per Evan's comment on
    https://groups.google.com/forum/#!msg/elm-discuss/AaL8iLjhEdU/pBe29vQdCgAJ
    I decided to go back to nesting.
--}


type alias ImageEssentials =
    { composition : Composition

    -- Colors
    , backgroundColor : Color
    , strokeColor : Color

    -- Angle
    , turnAngle : Float
    }


type alias EditState =
    -- Edit block index
    { editingIndex : Int

    -- Position
    , scale : Float
    , translate : ( Float, Float )
    }


type alias Gallery =
    List Composition


type alias VideoSettings =
    { videoAngleChangeDirection : Float
    , videoAngleChangeRate : Float
    , playingVideo : Bool
    }


type alias ModelSupport =
    -- Useful for 'ResetDrawing' operation (or is it? TODO)
    { basePolygon : Polygon

    -- Pan Stuff
    , panStarted : Bool
    , lastPos : ( Float, Float )

    -- Main Image Div Coordinates
    , imgDivCenter : ( Float, Float )
    , imgDivStart : ( Float, Float )

    -- Browser Focus
    , focus : Focus

    -- Url
    , url : Url.Url
    , navKey : Nav.Key
    }


type Polygon
    = Triangle
    | Square
    | Pentagon
    | Hexagon


type Focus
    = KeyboardEditing
    | TurnAngleInput



-- MODEL DEFAULT AND HELPERS


defaultModel : Url.Url -> Nav.Key -> Model
defaultModel url navKey =
    { image = defaultImageEssentials
    , editState = defaultEditState
    , gallery = defaultGallery
    , videoSettings = defaultVideoSettings
    , support = defaultModelSupport url navKey
    }


defaultImageEssentials : ImageEssentials
defaultImageEssentials =
    { composition = polygonComposition Square

    -- Colors
    , backgroundColor = Colors.darkGray
    , strokeColor = Colors.defaultGreen

    -- Angle
    , turnAngle = 90
    }


defaultEditState =
    -- Edit block index
    { editingIndex = LCore.length (polygonComposition Square) - 1

    -- Position
    , scale = 1
    , translate = ( 0, 0 )
    }


defaultGallery : Gallery
defaultGallery =
    []


defaultVideoSettings : VideoSettings
defaultVideoSettings =
    { videoAngleChangeDirection = 1
    , videoAngleChangeRate = 0.01
    , playingVideo = False
    }


defaultModelSupport : Url.Url -> Nav.Key -> ModelSupport
defaultModelSupport url navKey =
    -- Useful for 'ResetDrawing' operation (or is it? TODO)
    { basePolygon = Square

    -- Pan Stuff
    , lastPos = ( 0, 0 )
    , panStarted = False

    -- Main Image Div Coordinates
    , imgDivCenter = ( 0, 0 )
    , imgDivStart = ( 0, 0 )

    -- Browser Focus
    , focus = KeyboardEditing

    -- Url
    , url = url
    , navKey = navKey
    }


storageToModel : ImageEssentials -> EditState -> Gallery -> VideoSettings -> Url.Url -> Nav.Key -> Model
storageToModel image editState gallery videoSettings url navKey =
    Model image editState gallery videoSettings (defaultModelSupport url navKey)


updateImageEssentials : (ImageEssentials -> ImageEssentials) -> Model -> Model
updateImageEssentials fn model =
    { model | image = fn model.image }


updateEditState : (EditState -> EditState) -> Model -> Model
updateEditState fn model =
    { model | editState = fn model.editState }


updateVideoSettings : (VideoSettings -> VideoSettings) -> Model -> Model
updateVideoSettings fn model =
    { model | videoSettings = fn model.videoSettings }


updateSupport : (ModelSupport -> ModelSupport) -> Model -> Model
updateSupport fn model =
    { model | support = fn model.support }


withComposition : Composition -> Model -> Model
withComposition composition model =
    model |> updateImageEssentials (\image -> { image | composition = composition })


withEditingIndex : Int -> Model -> Model
withEditingIndex index model =
    model |> updateEditState (\editState -> { editState | editingIndex = index })


withEditingIndexOnTopBlock : Model -> Model
withEditingIndexOnTopBlock model =
    model |> withEditingIndex (LCore.length model.image.composition - 1)


resetModelDrawingToPolygon : Model -> Model
resetModelDrawingToPolygon model =
    let
        newComposition =
            polygonComposition model.support.basePolygon
    in
    model
        |> updateImageEssentials
            -- TODO experiment with this approach
            --[ (.composition, newComposition)
            --, (.turnAngle, polygonAngle model.support.basePolygon)
            --]
            (\image -> { image | composition = newComposition, turnAngle = polygonAngle model.support.basePolygon })
        |> withEditingIndexOnTopBlock



-- MODEL ENCODER and DECODER


modelDecoder : Decoder (Url.Url -> Nav.Key -> Model)
modelDecoder =
    Decode.map4 storageToModel
        (Decode.field "image" imageEssentialsDecoder)
        (Decode.field "editState" editStateDecoder)
        (Decode.field "gallery" galleryDecoder)
        (Decode.field "videoSettings" videoSettingsDecoder)


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "image", encodeImageEssentials model.image )
        , ( "editState", encodeEditState model.editState )
        , ( "gallery", encodeGallery model.gallery )
        , ( "videoSettings", encodeVideoSettings model.videoSettings )
        ]


imageEssentialsDecoder : Decoder ImageEssentials
imageEssentialsDecoder =
    Decode.map4 ImageEssentials
        (Decode.field "composition" LCore.compositionDecoder)
        (Decode.field "backgroundColor" Colors.decoder)
        (Decode.field "strokeColor" Colors.decoder)
        (Decode.field "turnAngle" Decode.float)


encodeImageEssentials : ImageEssentials -> Encode.Value
encodeImageEssentials image =
    Encode.object
        [ ( "composition", LCore.encodeComposition image.composition )
        , ( "backgroundColor", Colors.encode image.backgroundColor )
        , ( "strokeColor", Colors.encode image.strokeColor )
        , ( "turnAngle", Encode.float image.turnAngle )
        ]


editStateDecoder : Decoder EditState
editStateDecoder =
    Decode.map3 EditState
        (Decode.field "editingIndex" Decode.int)
        (Decode.field "scale" Decode.float)
        (Decode.map2 Tuple.pair
            (Decode.field "translateX" Decode.float)
            (Decode.field "translateY" Decode.float)
        )


encodeEditState : EditState -> Encode.Value
encodeEditState editState =
    Encode.object
        [ ( "editingIndex", Encode.int editState.editingIndex )
        , ( "scale", Encode.float editState.scale )
        , ( "translateX", Encode.float (Tuple.first editState.translate) )
        , ( "translateY", Encode.float (Tuple.second editState.translate) )
        ]


galleryDecoder : Decoder (List Composition)
galleryDecoder =
    Decode.list LCore.compositionDecoder


encodeGallery : Gallery -> Encode.Value
encodeGallery =
    Encode.list LCore.encodeComposition


videoSettingsDecoder : Decoder VideoSettings
videoSettingsDecoder =
    Decode.map3 VideoSettings
        (Decode.field "videoAngleChangeDirection" Decode.float)
        (Decode.field "videoAngleChangeRate" Decode.float)
        (Decode.field "playingVideo" Decode.bool)


encodeVideoSettings : VideoSettings -> Encode.Value
encodeVideoSettings videoSettings =
    Encode.object
        [ ( "videoAngleChangeDirection", Encode.float videoSettings.videoAngleChangeDirection )
        , ( "videoAngleChangeRate", Encode.float videoSettings.videoAngleChangeRate )
        , ( "playingVideo", Encode.bool videoSettings.playingVideo )
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Generative Art"
    , body =
        [ div
            [ css [ width (pct 100), height (pct 100) ] ]
            [ controlPanel model
            , transformsList model
            , mainImg model.image model.editState
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
            , backgroundColor (toCssColor model.image.backgroundColor)
            , color (toCssColor offWhite)
            ]
        ]
        [ infoAndBasicControls model.image.composition model.editState.editingIndex
        , colorControls model.image.backgroundColor model.image.strokeColor
        , videoControls model.videoSettings.playingVideo
        , turnAngleControl model.image.turnAngle
        , curatedSettings
        , controlsList
        ]


infoAndBasicControls : Composition -> Int -> Html Msg
infoAndBasicControls composition editingIndex =
    let
        stateLengthString =
            Debug.toString (LCore.stepsLength composition)

        editingTransformBlueprint =
            case LCore.getBlockAtIndex editingIndex composition of
                Nothing ->
                    ""

                Just block ->
                    LCore.blockToString block
    in
    controlBlock
        [ button [ onClick ResetDrawing ] [ text "ResetDrawing" ]
        , button [ onClick (Iterate editingIndex) ] [ text "Iterate" ]
        , button [ onClick Deiterate ] [ text "Deiterate" ]
        , p [] [ text stateLengthString ]
        , p [] [ text editingTransformBlueprint ]
        , button [ onClick DownloadSvg ] [ text "Download Image" ]
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
            model.image.composition
                |> LCore.toList
                |> List.indexedMap
                    (transformBox
                        model.editState.editingIndex
                        model.image.turnAngle
                        model.image.strokeColor
                        model.image.backgroundColor
                    )
                |> List.reverse
    in
    fixedDiv
        [ css
            [ backgroundColor (toCssColor model.image.backgroundColor)
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
        [ LDraw.image (LCore.fromList [ transform ])
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


mainImg : ImageEssentials -> EditState -> Html Msg
mainImg image { editingIndex, translate, scale } =
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
        [ LDraw.image image.composition
            |> withTurnAngle image.turnAngle
            |> withStrokeColor image.strokeColor
            |> withBackgroundColor image.backgroundColor
            |> withScale scale
            |> withTranslation translate
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
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.replaceUrl model.support.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( model
            , if
                Maybe.map ((++) "#") (Maybe.andThen Url.percentDecode url.fragment)
                    == Just (compositionToUrlString model.image.composition)
              then
                Cmd.none

              else
                replaceUrl model.support.navKey model.image.composition
            )

        -- Already getting ugly:
        DownloadSvg ->
            ( model, downloadSvg () )

        -- TODO Don't save to localStorage neither replaceUrl on every msg!
        {--TODO
            Maybe create a type like UrlMsg = LinkClicked | UrlChanged | AppMsg Msg
            then remove wildcard here and on updateModel.
        --}
        _ ->
            (\newModel ->
                ( newModel
                , Cmd.batch
                    [ saveModelToLocalStorage (encodeModel newModel)
                    , replaceUrl newModel.support.navKey newModel.image.composition
                    ]
                )
            )
            <|
                updateModel msg model


updateModel : Msg -> Model -> Model
updateModel msg model =
    case Debug.log "msg" msg of
        ResetDrawing ->
            -- TODO: Change this hacky implementation; add basePolygon to localStorage; and add a button to clear
            -- localStorage.
            resetModelDrawingToPolygon model

        Iterate editingIndex ->
            iterate model editingIndex

        Deiterate ->
            deiterate model

        KeyPress keyString ->
            processKey model keyString

        -- also, see `scaleAbout` in https://github.com/ianmackenzie/elm-geometry-svg/blob/master/src/Geometry/Svg.elm
        -- and later check https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/
        Zoom _ deltaY _ mousePos ->
            applyZoom deltaY mousePos model

        SetEditingIndex index ->
            model |> withEditingIndex index

        DropFromState index ->
            model
                |> withComposition (LCore.dropBlockAtIndex index model.image.composition)
                |> withEditingIndex
                    (if model.editState.editingIndex >= index then
                        model.editState.editingIndex - 1

                     else
                        model.editState.editingIndex
                    )

        SetBackgroundColor color ->
            model |> updateImageEssentials (\image -> { image | backgroundColor = color })

        SetStrokeColor color ->
            model |> updateImageEssentials (\image -> { image | strokeColor = color })

        SetTurnAngle turn ->
            model |> updateImageEssentials (\image -> { image | turnAngle = turn })

        PanStarted pos ->
            model |> updateSupport (\support -> { support | panStarted = True, lastPos = pos })

        PanEnded ->
            model |> updateSupport (\support -> { support | panStarted = False })

        MouseMoved pos ->
            let
                updatedTranslate =
                    pos |> pairExec (-) model.support.lastPos |> pairExec (+) model.editState.translate
            in
            model
                |> updateEditState (\a -> { a | translate = updatedTranslate })
                |> updateSupport (\a -> { a | lastPos = pos })

        SetFocus focus ->
            model |> updateSupport (\a -> { a | focus = focus })

        BasePolygonChanged polygon ->
            updateCompositionBasePolygon polygon model

        GotImgDivPosition result ->
            case result of
                Ok data ->
                    let
                        { x, y, width, height } =
                            data.element

                        imgDivCenter =
                            ( x + width / 2, y + height / 2 )
                    in
                    model |> updateSupport (\a -> { a | imgDivCenter = imgDivCenter, imgDivStart = ( x, y ) })

                Err _ ->
                    model

        TogglePlayingVideo ->
            model |> updateVideoSettings (\a -> { a | playingVideo = not model.videoSettings.playingVideo })

        VideoSpeedFaster ->
            model |> updateVideoSettings (\a -> { a | videoAngleChangeRate = min 1 (model.videoSettings.videoAngleChangeRate * 1.4) })

        VideoSpeedSlower ->
            model |> updateVideoSettings (\a -> { a | videoAngleChangeRate = max 0.001 (model.videoSettings.videoAngleChangeRate / 1.4) })

        ReverseAngleChangeDirection ->
            model |> updateVideoSettings (\a -> { a | videoAngleChangeDirection = model.videoSettings.videoAngleChangeDirection * -1 })

        -- TODO remove this
        _ ->
            model


processKey : Model -> String -> Model
processKey model keyPressed =
    let
        appendStep step =
            model |> withComposition (LCore.appendStepAtIndex step model.editState.editingIndex model.image.composition)
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
            model |> updateEditState (\editState -> { editState | scale = 1, translate = ( 0, 0 ) })

        "Backspace" ->
            model |> withComposition (LCore.dropLastStepAtIndex model.editState.editingIndex model.image.composition)

        "i" ->
            iterate model model.editState.editingIndex

        "d" ->
            deiterate model

        _ ->
            model


iterate : Model -> Int -> Model
iterate model editingIndex =
    model
        |> withComposition (LCore.duplicateBlockAndAppendAsLast editingIndex model.image.composition)
        |> withEditingIndexOnTopBlock


deiterate : Model -> Model
deiterate model =
    let
        updatedComposition =
            LCore.dropLastBlock model.image.composition

        updatedEditingIndex =
            min model.editState.editingIndex (LCore.length updatedComposition - 1)
    in
    model
        |> withComposition updatedComposition
        |> withEditingIndex updatedEditingIndex


applyZoom : Float -> ( Float, Float ) -> Model -> Model
applyZoom deltaY mousePos model =
    let
        scale =
            max (model.editState.scale - 0.01 * deltaY) 0.1

        vecMouseToImgDivCenter =
            model.support.imgDivCenter
                |> pairExec (-) mousePos
    in
    model
        |> updateEditState
            (\editState ->
                { editState
                    | scale = scale
                    , translate =
                        vecMouseToImgDivCenter
                            |> pairExec (+) model.editState.translate
                            |> pairMap (\value -> value * scale / model.editState.scale)
                            |> pairExec (-) vecMouseToImgDivCenter
                }
            )


updateCompositionBasePolygon : Polygon -> Model -> Model
updateCompositionBasePolygon polygon model =
    model
        |> updateSupport (\support -> { support | basePolygon = polygon })
        |> updateImageEssentials
            (\image ->
                { image
                    | turnAngle = polygonAngle polygon
                    , composition = LCore.changeBase (polygonBlock polygon) model.image.composition
                }
            )



-- COMMANDS


getImgDivPosition : Cmd Msg
getImgDivPosition =
    Task.attempt GotImgDivPosition (Browser.Dom.getElement "mainImg")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        panSubs =
            if model.support.panStarted then
                Sub.batch
                    [ Browser.Events.onMouseMove mouseMoveDecoder
                    , Browser.Events.onMouseUp (Decode.succeed PanEnded)
                    ]

            else
                Sub.none

        keyPressSub =
            if model.support.focus == KeyboardEditing then
                Browser.Events.onKeyUp keyPressDecoder

            else
                Sub.none

        playingVideoSub =
            if model.videoSettings.playingVideo then
                Time.every 50
                    (always
                        (SetTurnAngle
                            (model.image.turnAngle
                                + (model.videoSettings.videoAngleChangeDirection * model.videoSettings.videoAngleChangeRate)
                            )
                        )
                    )

            else
                Sub.none
    in
    Sub.batch [ keyPressSub, panSubs, playingVideoSub ]



-- URL


parseUrlToRoute : Url.Url -> Maybe Route
parseUrlToRoute url =
    -- Based on the RealWorld Example we treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    -- TODO change to query param!
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse urlParser


urlParser : Parser (Route -> a) a
urlParser =
    -- Can be transformed into multiple Routes like this:
    -- oneOf
    -- [ Parser.map Blank Parser.top
    Parser.map Home
        (Parser.custom "COMPOSITION" <|
            Url.percentDecode
                >> Maybe.andThen (Decode.decodeString LCore.compositionDecoder >> Result.toMaybe)
        )


compositionToUrlString : Composition -> String
compositionToUrlString composition =
    composition
        |> LCore.encodeComposition
        |> Encode.encode 0
        -- goes in front, like "#[['D','R'],['D']]"
        |> (++) "#"


replaceUrl : Nav.Key -> Composition -> Cmd msg
replaceUrl key composition =
    {--
            Note about Nav.replaceUrl: Browsers may rate-limit this function by throwing an
            exception. The discussion here suggests that the limit is 100 calls per 30 second
            interval in Safari in 2016. It also suggests techniques for people changing the
            URL based on scroll position.

            https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl
        --}
    Nav.replaceUrl key (compositionToUrlString composition)



-- KEYBOARD, MOUSE and WHEEL


keyPressDecoder : Decoder Msg
keyPressDecoder =
    Decode.map KeyPress
        (Decode.field "key" Decode.string)


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    mousePositionDecoder MouseMoved


mousePositionDecoder : (( Float, Float ) -> msg) -> Decoder msg
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



-- POLYGON AND COMPOSITIONS


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


polygonComposition : Polygon -> Composition
polygonComposition polygon =
    LCore.fromList [ polygonBlock polygon, [ D ] ]
