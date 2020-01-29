port module Main exposing (main)

import Browser
import Browser.Dom exposing (Element)
import Browser.Events
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
        , height
        , hidden
        , left
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
import Html.Styled exposing (Html, br, button, div, input, label, p, span, text, toUnstyled)
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
import LSystem.Core as LCore
    exposing
        ( State
        , Step(..)
        , Transformation
        , appendToStateAt
        , buildState
        , dropLastStepFromStateAt
        , dropStateAt
        , getTransformAt
        , stateLength
        )
import LSystem.Draw
    exposing
        ( drawImage
        , image
        , withScale
        , withStrokeColor
        , withTranslation
        , withTurnAngle
        )
import ListExtra exposing (appendIf, dropLast)
import Task


port saveStateToLocalStorage : Encode.Value -> Cmd msg



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Flags =
    Encode.Value


init : Flags -> ( Model, Cmd Msg )
init savedModel =
    let
        getImgDivPosition =
            Task.attempt GotImgDivPosition (Browser.Dom.getElement "mainImg")
    in
    case Decode.decodeValue modelDecoder savedModel of
        Ok model ->
            ( model, getImgDivPosition )

        Err err ->
            let
                _ =
                    Debug.log "Debug.log - Error while decoding localStorage" err

                model =
                    expandMinimalModel squareState Colors.darkGray Colors.defaultGreen 90 1 0 0
            in
            ( model
            , Cmd.batch
                [ saveStateToLocalStorage (encodeModel model)
                , getImgDivPosition
                ]
            )



-- MODEL


type alias Model =
    { state : State
    , editingIndex : Int
    , dir : String

    -- Pan and Zoom
    , scale : Float
    , wDelta : Float
    , hDelta : Float
    , panStarted : Bool
    , lastPos : ( Float, Float )
    , translate : ( Float, Float )
    , imgDivCenter : ( Float, Float )
    , imgDivStart : ( Float, Float )

    -- Color
    , backgroundColor : Color
    , strokeColor : Color

    -- Angle
    , turnAngle : Float
    , focus : Focus
    }


type Focus
    = KeyboardEditing
    | TurnAngleInput


squareState : State
squareState =
    { base = [ D, L, D, L, D, L, D ]
    , transforms = [ [ D ] ]
    }


expandMinimalModel : State -> Color -> Color -> Float -> Float -> Float -> Float -> Model
expandMinimalModel state bgColor strokeColor turnAngle scale translateX translateY =
    Model
        state
        1
        --
        ""
        -- Pan and Zoom
        scale
        0
        0
        False
        ( 0, 0 )
        ( translateX, translateY )
        ( 0, 0 )
        ( 0, 0 )
        -- Colors
        bgColor
        strokeColor
        -- Angle
        turnAngle
        KeyboardEditing


modelDecoder : Decoder Model
modelDecoder =
    Decode.map7 expandMinimalModel
        (Decode.field "state" LCore.stateDecoder)
        (Decode.field "bgColor" Colors.decoder)
        (Decode.field "strokeColor" Colors.decoder)
        (Decode.field "turnAngle" Decode.float)
        (Decode.field "scale" Decode.float)
        (Decode.field "translateX" Decode.float)
        (Decode.field "translateY" Decode.float)


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "state", LCore.encodeState model.state )
        , ( "bgColor", Colors.encode model.backgroundColor )
        , ( "strokeColor", Colors.encode model.strokeColor )
        , ( "turnAngle", Encode.float model.turnAngle )
        , ( "scale", Encode.float model.scale )
        , ( "translateX", Encode.float (Tuple.first model.translate) )
        , ( "translateY", Encode.float (Tuple.second model.translate) )
        ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    div
        [ css [ width (pct 100), height (pct 100) ] ]
        [ controlPanel model
        , transformsList model
        , mainImg model
        ]
        |> toUnstyled


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
        , turnAngleControl model.turnAngle
        , curatedSettings
        ]


infoAndBasicControls : Model -> Html Msg
infoAndBasicControls model =
    let
        editingTransform =
            LCore.getTransformAt model.editingIndex model.state

        stateLengthString =
            Debug.toString (stateLength model.state)

        dir =
            -- To do: refactor "dir" variable inside model and here
            "{" ++ model.dir ++ "}"

        editingTransformBlueprint =
            LCore.transformToString editingTransform
    in
    controlBlock
        [ button [ onClick ClearSvg ] [ text "ClearSvg" ]
        , button [ onClick (Iterate editingTransform) ] [ text "Iterate" ]
        , button [ onClick Deiterate ] [ text "Deiterate" ]
        , p [] [ text stateLengthString ]
        , p [] [ text dir ]
        , p [] [ text editingTransformBlueprint ]
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
        , button [ onClick (StateBaseChanged Triangle) ] [ text "Triangle" ]
        , button [ onClick (StateBaseChanged Square) ] [ text "Square" ]
        , button [ onClick (StateBaseChanged Pentagon) ] [ text "Pentagon" ]
        , button [ onClick (StateBaseChanged Hexagon) ] [ text "Hexagon" ]
        ]


controlBlock : List (Html Msg) -> Html Msg
controlBlock =
    div [ css [ padding (px 10), borderBottom3 (px 1) solid (toCssColor Colors.black) ] ]


transformsList : Model -> Html Msg
transformsList model =
    let
        transforms =
            model.state
                |> LCore.toList
                |> List.indexedMap (transformBox model.editingIndex model.turnAngle model.strokeColor)
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
        transforms


transformBox : Int -> Float -> Color -> Int -> Transformation -> Html Msg
transformBox editingIndex turnAngle strokeColor index transform =
    div
        [ css
            [ height (px 200)
            , width (pct 100)
            , borderBottom3 (px 1) solid (toCssColor Colors.black)
            ]
        ]
        [ image transform
            |> withTurnAngle turnAngle
            |> withStrokeColor strokeColor
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
        [ image (buildState model.state)
            |> withTurnAngle model.turnAngle
            |> withStrokeColor model.strokeColor
            |> withScale model.scale
            |> withTranslation model.translate
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



-- MSG


type Msg
    = KeyPress String
      -- Main commands
    | ClearSvg
    | StateBaseChanged Polygon
    | Iterate Transformation
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
      -- Focus
    | SetFocus Focus


type alias ShiftKey =
    Bool


type Polygon
    = Triangle
    | Square
    | Pentagon
    | Hexagon



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        pairExec op secondArg firstArg =
            ( op (Tuple.first firstArg) (Tuple.first secondArg)
            , op (Tuple.second firstArg) (Tuple.second secondArg)
            )

        pairMap op pair =
            ( op (Tuple.first pair)
            , op (Tuple.second pair)
            )

        _ =
            Debug.log "Debug.log - From update, model" model
    in
    (\newModel ->
        ( newModel
        , saveStateToLocalStorage (encodeModel newModel)
        )
    )
    <|
        case Debug.log "msg" msg of
            ClearSvg ->
                { model | state = squareState }

            Iterate transform ->
                iterate model transform

            Deiterate ->
                deiterate model

            KeyPress keyString ->
                processKey model keyString

            -- also, see `scaleAbout` in https://github.com/ianmackenzie/elm-geometry-svg/blob/master/src/Geometry/Svg.elm
            -- and later check https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/
            Zoom _ deltaY _ mousePos ->
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

            SetEditingIndex index ->
                { model | editingIndex = index }

            DropFromState index ->
                let
                    newEditingIndex =
                        if model.editingIndex == index then
                            List.length model.state.transforms - 1

                        else if model.editingIndex > index then
                            model.editingIndex - 1

                        else
                            model.editingIndex
                in
                { model | state = LCore.dropStateAt index model.state, editingIndex = newEditingIndex }

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

            StateBaseChanged polygon ->
                let
                    state =
                        model.state

                    updateModel stateBase turnAngle =
                        { model
                            | state = { state | base = stateBase }
                            , turnAngle = turnAngle
                        }
                in
                case polygon of
                    Triangle ->
                        updateModel [ D, L, D, L, D ] 120

                    Square ->
                        updateModel [ D, L, D, L, D, L, D ] 90

                    Pentagon ->
                        updateModel [ D, L, D, L, D, L, D, L, D ] 72

                    Hexagon ->
                        updateModel [ D, L, D, L, D, L, D, L, D, L, D ] 60

            GotImgDivPosition result ->
                case result of
                    Ok data ->
                        let
                            { x, y, width, height } =
                                data.element

                            imgDivStart =
                                ( x, y )

                            imgDivCenter =
                                ( x + width / 2, y + height / 2 )
                        in
                        { model | imgDivCenter = imgDivCenter, imgDivStart = imgDivStart }

                    Err _ ->
                        model


processKey : Model -> String -> Model
processKey model dir =
    case dir of
        "ArrowLeft" ->
            { model | state = LCore.appendToStateAt [ L ] model.editingIndex model.state }

        "ArrowRight" ->
            { model | state = LCore.appendToStateAt [ R ] model.editingIndex model.state }

        "ArrowUp" ->
            { model | state = LCore.appendToStateAt [ D ] model.editingIndex model.state }

        "ArrowDown" ->
            { model | state = LCore.appendToStateAt [ S ] model.editingIndex model.state }

        " " ->
            { model | scale = 1, translate = ( 0, 0 ) }

        "Backspace" ->
            { model | state = LCore.dropLastStepFromStateAt model.editingIndex model.state }

        "i" ->
            let
                newModel =
                    iterate model (LCore.getTransformAt model.editingIndex model.state)
            in
            { newModel | dir = dir }

        "d" ->
            let
                newModel =
                    deiterate model
            in
            { newModel | dir = dir }

        _ ->
            { model | dir = dir }


iterate : Model -> Transformation -> Model
iterate model transform =
    let
        state =
            model.state

        newState =
            { state
                | transforms = state.transforms ++ [ transform ]
            }
    in
    { model | state = newState, editingIndex = List.length newState.transforms }


deiterate : Model -> Model
deiterate model =
    let
        newTransforms =
            dropLast model.state.transforms

        newEditingIndex =
            if model.editingIndex > List.length newTransforms then
                List.length newTransforms

            else
                model.editingIndex

        state =
            model.state
    in
    { model
        | state = { state | transforms = newTransforms }
        , editingIndex = newEditingIndex
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([]
            |> appendIf (model.focus == KeyboardEditing) [ Browser.Events.onKeyUp keyPressDecoder ]
            |> appendIf model.panStarted
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed PanEnded)
                ]
        )



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
