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
import Icons exposing (withColor, withConditionalColor, withOnClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LSystem.Core as LCore exposing (Block, Composition, Step(..))
import LSystem.Draw
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


port saveStateToLocalStorage : Encode.Value -> Cmd msg


port downloadSvg : () -> Cmd msg



-- FUNDAMENTAL TYPES
-- TYPES
-- MSG


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



-- MODEL


type alias Model =
    -- Aplication heart
    { composition : Composition
    , editingIndex : Int

    -- Pan and Zoom
    , scale : Float
    , panStarted : Bool
    , lastPos : ( Float, Float )
    , translate : ( Float, Float )

    -- Main Image Div Coordinates
    , imgDivCenter : ( Float, Float )
    , imgDivStart : ( Float, Float )

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



-- OTHER TYPES


type Route
    = Home UrlDataVersions


type alias UrlDataVersions =
    { v1_dataOnQueryParams : Maybe ImageEssentials
    , v0_dataOnHash : Maybe Composition
    }


type alias ImageEssentials =
    { composition : Composition
    , turnAngle : Float
    , backgroundColor : Color
    , strokeColor : Color
    }


type alias Flags =
    Encode.Value


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



-- MODEL STUFF


expandMinimalModel : Composition -> Color -> Color -> Float -> Float -> Float -> Float -> Url.Url -> Nav.Key -> Model
expandMinimalModel state bgColor strokeColor turnAngle scale translateX translateY url navKey =
    Model
        -- Aplication heart
        state
        1
        -- Pan and Zoom
        scale
        False
        ( 0, 0 )
        ( translateX, translateY )
        -- Main Image Div Coordinates
        ( 0, 0 )
        ( 0, 0 )
        -- Colors
        bgColor
        strokeColor
        -- Angle
        turnAngle
        -- Browser Focus
        KeyboardEditing
        -- Url
        url
        navKey
        -- Video
        0.1
        False
        1


basicModelFrom : ImageEssentials -> Url.Url -> Nav.Key -> Model
basicModelFrom { composition, turnAngle, backgroundColor, strokeColor } url navKey =
    expandMinimalModel composition backgroundColor strokeColor turnAngle 1 0 0 url navKey


squareImage : ImageEssentials
squareImage =
    [ polygonBlock Square, [ D ] ]
        |> LCore.fromList
        |> compositionToBasicImage


modelToImageEssentials : Model -> ImageEssentials
modelToImageEssentials model =
    { composition = model.composition
    , turnAngle = model.turnAngle
    , backgroundColor = model.backgroundColor
    , strokeColor = model.strokeColor
    }


modelWithEditIndexLast : Model -> Model
modelWithEditIndexLast model =
    { model | editingIndex = LCore.length model.composition - 1 }


compositionToBasicImage : Composition -> ImageEssentials
compositionToBasicImage composition =
    ImageEssentials composition 90 Colors.darkGray Colors.defaultGreen



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


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init savedModel url navKey =
    case parseUrlToImageEssentials url of
        Just image ->
            -- Got a valid Composition from the URL
            ( modelWithEditIndexLast <| basicModelFrom image url navKey, getImgDivPosition )

        Nothing ->
            case Decode.decodeValue modelDecoder savedModel of
                -- Got a valid composition from localStorage
                Ok modelWithoutUrlAndKey ->
                    let
                        model =
                            modelWithoutUrlAndKey url navKey
                    in
                    ( model
                    , Cmd.batch
                        [ replaceUrl navKey (modelToImageEssentials model)
                        , getImgDivPosition
                        ]
                    )

                -- Didn't found a valid composition: default to squareComposition
                Err err ->
                    let
                        _ =
                            Debug.log "Debug.log - Error while decoding localStorage" err

                        model =
                            basicModelFrom squareImage url navKey
                    in
                    ( model
                    , Cmd.batch
                        [ saveStateToLocalStorage (encodeModel model)
                        , replaceUrl navKey (modelToImageEssentials model)
                        , getImgDivPosition
                        ]
                    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
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
        [ button [ onClick ResetDrawing ] [ text "ResetDrawing" ]
        , button [ onClick (Iterate model.editingIndex) ] [ text "Iterate" ]
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
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.replaceUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( model
            , if
                -- This if-statement prevents a bug with infinite loop
                -- TODO Refactor and remove this.
                parseUrlToImageEssentials url == Just (modelToImageEssentials model)
              then
                Cmd.none

              else
                replaceUrl model.navKey (modelToImageEssentials model)
            )

        --}
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
                    [ saveStateToLocalStorage (encodeModel newModel)
                    , replaceUrl newModel.navKey (modelToImageEssentials newModel)
                    ]
                )
            )
            <|
                updateModel msg model


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        -- TODO: Add a button to clear localStorage.
        ResetDrawing ->
            { model
                | composition = model.composition |> LCore.dropAllBlocksButBase |> LCore.appendBlock [ D ]
                , editingIndex = 1
            }

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
            { model | editingIndex = index }

        DropFromState index ->
            { model
                | composition = LCore.dropBlockAtIndex index model.composition
                , editingIndex =
                    if model.editingIndex >= index then
                        model.editingIndex - 1

                    else
                        model.editingIndex
            }

        SetBackgroundColor color ->
            { model | backgroundColor = color }

        SetStrokeColor color ->
            { model | strokeColor = color }

        SetTurnAngle turn ->
            { model | turnAngle = turn }

        PanStarted pos ->
            { model | panStarted = True, lastPos = pos }

        PanEnded ->
            { model | panStarted = False }

        MouseMoved pos ->
            { model
                | translate =
                    pos
                        |> pairExec (-) model.lastPos
                        |> pairExec (+) model.translate
                , lastPos = pos
            }

        SetFocus focus ->
            { model | focus = focus }

        BasePolygonChanged polygon ->
            updateCompositionBaseAndAngle model polygon

        GotImgDivPosition result ->
            case result of
                Ok data ->
                    let
                        { x, y, width, height } =
                            data.element
                    in
                    { model | imgDivCenter = ( x + width / 2, y + height / 2 ), imgDivStart = ( x, y ) }

                Err _ ->
                    model

        TogglePlayingVideo ->
            { model | playingVideo = not model.playingVideo }

        VideoSpeedFaster ->
            { model | videoAngleChangeRate = min 1 (model.videoAngleChangeRate * 1.4) }

        VideoSpeedSlower ->
            { model | videoAngleChangeRate = max 0.001 (model.videoAngleChangeRate / 1.4) }

        ReverseAngleChangeDirection ->
            { model | videoAngleChangeDirection = model.videoAngleChangeDirection * -1 }

        -- TODO remove this
        _ ->
            model


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


applyZoom : Float -> ( Float, Float ) -> Model -> Model
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


parseUrlToImageEssentials : Url.Url -> Maybe ImageEssentials
parseUrlToImageEssentials url =
    parseUrlToRoute url
        |> Maybe.andThen
            (\(Home data) ->
                if isJust data.v1_dataOnQueryParams then
                    data.v1_dataOnQueryParams

                else if isJust data.v0_dataOnHash then
                    -- Migrating from old URL format
                    Maybe.map compositionToBasicImage data.v0_dataOnHash

                else
                    Nothing
            )


parseUrlToRoute : Url.Url -> Maybe Route
parseUrlToRoute url =
    Parser.parse urlParser url


urlParser : Parser (Route -> a) a
urlParser =
    Parser.map Home
        (Parser.map
            UrlDataVersions
            (Parser.query queryToImageEssentials </> fragmentToCompositionParser)
        )


{-| Current version of Url building and parsing
-}
queryToImageEssentials : Query.Parser (Maybe ImageEssentials)
queryToImageEssentials =
    let
        queryMapFromDecoder decoder =
            Query.map (Maybe.andThen (Result.toMaybe << Decode.decodeString decoder))
    in
    Query.map4 (Maybe.map4 ImageEssentials)
        (Query.string "composition" |> queryMapFromDecoder LCore.compositionDecoder)
        (Query.string "angle" |> Query.map (Maybe.andThen String.toFloat))
        (Query.string "bg" |> queryMapFromDecoder Colors.decoder)
        (Query.string "stroke" |> queryMapFromDecoder Colors.decoder)


{-| Backwards compatibility: Old version of Url building and parsing.
-}
fragmentToCompositionParser : Parser (Maybe Composition -> a) a
fragmentToCompositionParser =
    Parser.fragment <|
        Maybe.andThen
            (Url.percentDecode
                >> Maybe.andThen (Decode.decodeString LCore.compositionDecoder >> Result.toMaybe)
            )


imageEssentialsToUrlString : ImageEssentials -> String
imageEssentialsToUrlString image =
    Url.Builder.absolute []
        [ Url.Builder.string "composition" (image.composition |> LCore.encodeComposition |> Encode.encode 0)
        , Url.Builder.string "angle" (String.fromFloat image.turnAngle)
        , Url.Builder.string "bg" (image.backgroundColor |> Colors.encode |> Encode.encode 0)
        , Url.Builder.string "stroke" (image.strokeColor |> Colors.encode |> Encode.encode 0)
        ]


replaceUrl : Nav.Key -> ImageEssentials -> Cmd msg
replaceUrl key imageEssentials =
    {--
            Note about Nav.replaceUrl: Browsers may rate-limit this function by throwing an
            exception. The discussion here suggests that the limit is 100 calls per 30 second
            interval in Safari in 2016. It also suggests techniques for people changing the
            URL based on scroll position.

            https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl
        --}
    Nav.replaceUrl key (imageEssentialsToUrlString imageEssentials)



-- MODEL ENCODER AND DECODER


modelDecoder : Decoder (Url.Url -> Nav.Key -> Model)
modelDecoder =
    Decode.map7 expandMinimalModel
        (Decode.field "state" LCore.compositionDecoder)
        (Decode.field "bgColor" Colors.decoder)
        (Decode.field "strokeColor" Colors.decoder)
        (Decode.field "turnAngle" Decode.float)
        (Decode.field "scale" Decode.float)
        (Decode.field "translateX" Decode.float)
        (Decode.field "translateY" Decode.float)


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "state", LCore.encodeComposition model.composition )
        , ( "bgColor", Colors.encode model.backgroundColor )
        , ( "strokeColor", Colors.encode model.strokeColor )
        , ( "turnAngle", Encode.float model.turnAngle )
        , ( "scale", Encode.float model.scale )
        , ( "translateX", Encode.float (Tuple.first model.translate) )
        , ( "translateY", Encode.float (Tuple.second model.translate) )
        ]



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
