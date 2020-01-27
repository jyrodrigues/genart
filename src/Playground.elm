module Playground exposing (Model, Msg(..), squareState, update, view)

import Auxiliary exposing (dropLast, getLast)
import Colors exposing (Color, toCssColor)
import Css
    exposing
        --( Color
        ( backgroundColor
        , fixed
        , height
          --, hex
        , left
        , overflow
        , pct
        , position
          --, rgb
        , scroll
        , top
        , width
        )
import Html
import Html.Styled exposing (Html, div, fromUnstyled, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Icons exposing (withColor, withConditionalColor, withOnClick)
import Json.Encode as Encode
import LSystem.Core as LCore
    exposing
        ( State
        , Step(..)
        , Transformation
        , appendToStateAt
        , applyRule
        , buildState
        , dropLastStepFromStateAt
        , dropStateAt
        , getTransformAt
        , stateLength
        )
import LSystem.Draw exposing (drawSvgFixed, drawSvgFixedWithColor)
import LSystem.String



-- MODEL


type alias Model =
    { state : State
    , editingIndex : Int
    , savedStates : List State
    , baseState : State

    -- , savedTransforms : List Transform
    , isShowingNextIteration : Bool
    , dir : String
    , zoomLevel : Float
    , wDelta : Float
    , hDelta : Float

    -- Todo: remove fixed. "space" should just center the image
    , fixed : Bool

    -- Testing color change
    , backgroundColor : Color
    , drawColor : Color
    }


squareState : State
squareState =
    { base = [ D, L, D, L, D, L, D ]
    , transforms = [ [ D ] ]
    }



-- VIEW


view : Model -> Html.Html Msg
view model =
    div
        [ css [ width (pct 100), height (pct 100) ] ]
        [ topRow model
        , leftPane model
        , rightPane model
        ]
        |> toUnstyled


topRow : Model -> Html Msg
topRow model =
    let
        editingTransform =
            LCore.getTransformAt model.editingIndex model.state

        fixedOrZoomStatus =
            " Fixed: "
                ++ (if model.fixed then
                        "On"

                    else
                        "Off"
                   )

        stateLengthString =
            Debug.toString (stateLength model.state)

        dir =
            -- To do: refactor "dir" variable inside model and here
            "{" ++ model.dir ++ "}"

        editingTransformBlueprint =
            LSystem.String.fromTransform editingTransform
    in
    fixedDiv
        [ css
            [ height (pct layout.topRow)
            , width (pct 100)
            , overflow scroll
            , backgroundColor (toCssColor model.backgroundColor)
            ]
        ]
        [ div []
            [ Html.Styled.button [ onClick ClearSvg ] [ text "ClearSvg" ]
            , Html.Styled.button [ onClick (Iterate editingTransform) ] [ text "Iterate" ]
            , Html.Styled.button [ onClick Deiterate ] [ text "Deiterate" ]
            , span [] [ text fixedOrZoomStatus ]
            ]
        , div []
            [ p [] [ text stateLengthString ]
            , p [] [ text dir ]
            , p [] [ text editingTransformBlueprint ]
            ]
        ]


leftPane : Model -> Html Msg
leftPane model =
    let
        transforms =
            model.state
                |> LCore.toList
                |> List.indexedMap (transformBox model.editingIndex)
                |> List.reverse
    in
    fixedDiv
        [ css
            [ backgroundColor (toCssColor model.backgroundColor)
            , height (pct layout.middleRow)
            , width (pct layout.leftPane)
            , top (pct layout.topRow)
            , overflow scroll
            ]
        ]
        transforms


transformBox : Int -> Int -> Transformation -> Html Msg
transformBox editingIndex index transform =
    div []
        [ div [] [ drawSvgFixed transform ]
        , Icons.trash
            |> withColor Colors.red_
            |> withOnClick (DropFromState index)
            |> Icons.toSvg
        , Icons.pen
            |> withConditionalColor (index == editingIndex) Colors.green_
            |> withOnClick (SetEditingIndex index)
            |> Icons.toSvg
        ]


rightPane : Model -> Html Msg
rightPane model =
    fixedDiv
        [ css
            [ backgroundColor (toCssColor model.backgroundColor)
            , position fixed
            , height (pct layout.middleRow)
            , width (pct layout.rightPane)
            , top (pct layout.topRow)
            , left (pct layout.leftPane)
            ]
        ]
        [ drawSvgFixedWithColor model.drawColor (buildState model.state)
        ]


layout :
    { topRow : Float
    , middleRow : Float
    , leftPane : Float
    , rightPane : Float
    }
layout =
    { topRow = 20
    , middleRow = 70
    , leftPane = 20
    , rightPane = 80
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
    | ClearSvg
    | SetAsBase State
    | Iterate Transformation
    | Deiterate
    | ToggleShowNextIteration
    | SaveState
    | Exclude Int
    | Zoom Float Float ShiftKey
      --
    | SetEditingIndex Int
    | DropFromState Int
    | SetBackgroundColor Color
    | SetDrawColor Color



-- | UpdateSaved (List (List State))


type alias ShiftKey =
    Bool



-- port cache : Encode.Value -> Cmd msg
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg:" msg
    in
    case msg of
        ClearSvg ->
            ( { model | state = squareState }, Cmd.none )

        SetAsBase state ->
            ( { model | state = state }, Cmd.none )

        Iterate transform ->
            ( iterate model transform, Cmd.none )

        Deiterate ->
            ( deiterate model, Cmd.none )

        ToggleShowNextIteration ->
            ( { model | isShowingNextIteration = not model.isShowingNextIteration }, Cmd.none )

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

        -- use svg property `transform="scale(2 3.5)"` see: https://developer.mozilla.org/pt-BR/docs/Web/SVG/Attribute/transform
        -- also, see `scaleAbout` in https://github.com/ianmackenzie/elm-geometry-svg/blob/master/src/Geometry/Svg.elm
        -- and later check https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/
        Zoom deltaX deltaY shiftKey ->
            if shiftKey then
                ( { model | wDelta = model.wDelta + deltaX, hDelta = model.hDelta + deltaY }, Cmd.none )

            else
                ( { model | zoomLevel = model.zoomLevel + deltaY }, Cmd.none )

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

        SetDrawColor color ->
            ( { model | drawColor = color }, Cmd.none )


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
            { model | fixed = not model.fixed }

        "Backspace" ->
            { model | state = LCore.dropLastStepFromStateAt model.editingIndex model.state }

        "i" ->
            let
                newModel =
                    Debug.log "newModel after `i`:" <| iterate model <| LCore.getTransformAt model.editingIndex model.state
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
        state =
            model.state

        newState =
            { state
                | transforms = dropLast state.transforms
            }
    in
    { model | state = newState }
