port module Main exposing (main)

import Browser
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
    case Decode.decodeValue modelDecoder savedModel of
        Ok model ->
            ( model, Cmd.none )

        Err err ->
            let
                _ =
                    Debug.log "Err:" err

                model =
                    expandMinimalModel squareState Colors.darkGray Colors.defaultGreen 90 1 0 0
            in
            ( model, saveStateToLocalStorage (encodeModel model) )



-- MODEL


type alias Model =
    { state : State
    , editingIndex : Int
    , dir : String

    -- Pan and Zoom
    , zoomLevel : Float
    , wDelta : Float
    , hDelta : Float
    , panStarted : Bool
    , lastX : Int
    , lastY : Int
    , translateX : Int
    , translateY : Int

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


expandMinimalModel : State -> Color -> Color -> Float -> Float -> Int -> Int -> Model
expandMinimalModel state bgColor strokeColor turnAngle zoomLevel translateX translateY =
    Model
        state
        1
        --
        ""
        -- Pan and Zoom
        zoomLevel
        0
        0
        False
        0
        0
        translateX
        translateY
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
        (Decode.field "zoomLevel" Decode.float)
        (Decode.field "translateX" Decode.int)
        (Decode.field "translateY" Decode.int)


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "state", LCore.encodeState model.state )
        , ( "bgColor", Colors.encode model.backgroundColor )
        , ( "strokeColor", Colors.encode model.strokeColor )
        , ( "turnAngle", Encode.float model.turnAngle )
        , ( "zoomLevel", Encode.float model.zoomLevel )
        , ( "translateX", Encode.int model.translateX )
        , ( "translateY", Encode.int model.translateY )
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
        [ span [] [ text "Change Angle and Base" ]
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
        , zoomOnWheel
        , on "mousedown" (mousePositionDecoder PanStarted)
        ]
        [ image (buildState model.state)
            |> withTurnAngle model.turnAngle
            |> withStrokeColor model.strokeColor
            |> withScale model.zoomLevel
            |> withTranslation model.translateX model.translateY
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
    | PanStarted Int Int
    | PanEnded
    | MouseMoved Int Int
    | Zoom Float Float ShiftKey
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
            Zoom _ deltaY _ ->
                { model | zoomLevel = max (model.zoomLevel - 0.01 * deltaY) 0 }

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

            PanStarted x y ->
                { model | panStarted = True, lastX = x, lastY = y }

            PanEnded ->
                { model | panStarted = False }

            MouseMoved x y ->
                { model
                    | translateX = x - model.lastX + model.translateX
                    , translateY = y - model.lastY + model.translateY
                    , lastX = x
                    , lastY = y
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
            { model | zoomLevel = 1, translateX = 0, translateY = 0 }

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


mousePositionDecoder : (Int -> Int -> msg) -> Decoder msg
mousePositionDecoder msg =
    Decode.map2 msg
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)


zoomOnWheel : Html.Styled.Attribute Msg
zoomOnWheel =
    preventDefaultOn "wheel" (Decode.map alwaysPreventDefault wheelDecoder)


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


wheelDecoder : Decoder Msg
wheelDecoder =
    Decode.map3 Zoom
        (Decode.field "deltaX" Decode.float)
        (Decode.field "deltaY" Decode.float)
        (Decode.field "shiftKey" Decode.bool)
