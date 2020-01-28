module Main exposing (main)

--import Json.Encode as Encode

import Browser
import Browser.Events
import Colors exposing (Color, offWhite, toCssColor)
import Css
    exposing
        ( backgroundColor
        , color
        , fixed
        , height
        , hidden
        , left
        , overflow
        , overflowX
        , overflowY
        , pct
        , position
        , px
        , right
        , scale
        , scroll
        , transform
        , width
        , zero
        )
import Html
import Html.Styled exposing (Html, button, div, input, label, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css, for, id, type_, value)
import Html.Styled.Events
    exposing
        ( on
        , onBlur
        , onClick
        , onFocus
        , onInput
        , onMouseDown
        , onMouseUp
        , preventDefaultOn
        )
import Icons exposing (withColor, withConditionalColor, withOnClick)
import Json.Decode as Decode exposing (Decoder)
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
import LSystem.String
import ListExtra exposing (appendIf, dropLast)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- type alias Flags =
--     List (List String)
-- init : Flags -> ( Model, Cmd Msg )
-- init localStorage =
--     ( createInitialModelWith localStorage, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultInitialModel, Cmd.none )



-- MODEL


type Focus
    = KeyboardEditing
    | TurnAngleInput


type alias Model =
    { state : State
    , editingIndex : Int
    , savedStates : List State
    , baseState : State

    -- , savedTransforms : List Transform
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


squareState : State
squareState =
    { base = [ D, L, D, L, D, L, D ]
    , transforms = [ [ D ] ]
    }


createInitialModelWith : List (List String) -> Model
createInitialModelWith localStorage =
    let
        -- Todo: next version of storage, i.e. 'genart/v1/savedStates' or something in those lines
        -- savedStates =
        --     localStorage
        --         |> List.map (\state -> List.map stringToStep state)
        savedStates =
            []
    in
    Model
        squareState
        1
        savedStates
        squareState
        --
        ""
        -- Pan and Zoom
        1
        0
        0
        False
        0
        0
        0
        0
        -- Colors
        Colors.darkGray
        Colors.defaultGreen
        -- Angle
        90
        KeyboardEditing


defaultInitialModel : Model
defaultInitialModel =
    createInitialModelWith []



{--[D L D D L D D D D L D D L D D D D D D R D L D L D D D D D D D L] --}
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
            , overflowY scroll
            , overflowX hidden
            , backgroundColor (toCssColor model.backgroundColor)
            , color (toCssColor offWhite)
            ]
        ]
        [ infoAndBasicControls model
        , colorControls model.backgroundColor model.strokeColor
        , turnAngleControl model.turnAngle
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
            LSystem.String.fromTransform editingTransform
    in
    div []
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
                [ input
                    [ type_ "color"
                    , id inputId
                    , onInput (Colors.fromHexString >> msg)
                    , value (Colors.toHexString color)
                    ]
                    []
                , label [ for inputId ] [ text inputLabel ]
                ]
    in
    div []
        [ colorControl "BgColor" SetBackgroundColor "Change background color" backgroundColor
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
    div []
        [ input
            [ id "TurnAngle" -- See index.js, `id` only exists for use in there.
            , type_ "number"
            , value (String.fromFloat turnAngle)
            , onInput onInputCallback
            , onFocus (SetFocus TurnAngleInput)
            , onBlur (SetFocus KeyboardEditing)
            ]
            []
        , label [ for "TurnAngle" ] [ text "Change angle" ]
        ]


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
            ]
        ]
        transforms


transformBox : Int -> Float -> Color -> Int -> Transformation -> Html Msg
transformBox editingIndex turnAngle strokeColor index transform =
    div
        [ css
            [ height (px 200)
            , width (pct 100)
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


type
    Msg
    -- Keyboard Listening
    = KeyPress String
      -- Main commands
    | ClearSvg
    | Iterate Transformation
    | Deiterate
    | SetEditingIndex Int
    | DropFromState Int
      -- Storage
    | SaveState
    | Exclude Int
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



-- | UpdateSaved (List (List State))


type alias ShiftKey =
    Bool



-- port cache : Encode.Value -> Cmd msg
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        ClearSvg ->
            ( { model | state = squareState }, Cmd.none )

        Iterate transform ->
            ( iterate model transform, Cmd.none )

        Deiterate ->
            ( deiterate model, Cmd.none )

        KeyPress keyString ->
            ( processKey model keyString, Cmd.none )

        SaveState ->
            ( { model | savedStates = model.state :: model.savedStates }, Cmd.none )

        Exclude index ->
            let
                newSavedStates =
                    List.take index model.savedStates ++ List.drop (index + 1) model.savedStates
            in
            ( { model | savedStates = newSavedStates }, Cmd.none )

        -- also, see `scaleAbout` in https://github.com/ianmackenzie/elm-geometry-svg/blob/master/src/Geometry/Svg.elm
        -- and later check https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/
        Zoom deltaX deltaY shiftKey ->
            --if shiftKey then
            --( { model | wDelta = model.wDelta + deltaX, hDelta = model.hDelta + deltaY }, Cmd.none )
            --else
            ( { model
                | zoomLevel =
                    Debug.log "\noldZoomLevel: " model.zoomLevel
                        + Debug.log " + " 0.01
                        * Debug.log " * deltaY "
                            deltaY
              }
            , Cmd.none
            )

        SetEditingIndex index ->
            ( { model | editingIndex = index }, Cmd.none )

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
            ( { model | state = LCore.dropStateAt index model.state, editingIndex = newEditingIndex }, Cmd.none )

        SetBackgroundColor color ->
            ( { model | backgroundColor = color }, Cmd.none )

        SetStrokeColor color ->
            ( { model | strokeColor = color }, Cmd.none )

        SetTurnAngle turn ->
            ( { model | turnAngle = turn }, Cmd.none )

        PanStarted x y ->
            ( { model | panStarted = True, lastX = x, lastY = y }, Cmd.none )

        PanEnded ->
            ( { model | panStarted = False }, Cmd.none )

        MouseMoved x y ->
            ( { model
                | translateX = x - model.lastX + model.translateX
                , translateY = y - model.lastY + model.translateY
                , lastX = x
                , lastY = y
              }
            , Cmd.none
            )

        SetFocus focus ->
            ( { model | focus = focus }, Cmd.none )


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



{--
cacheSavedStates savedStates =
    savedStates
        |> List.map (\state -> List.map LSystem.String.fromStep state)
        |> Encode.list (\state -> Encode.list Encode.string state)
        |> cache
--}


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



-- ZOOM


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
