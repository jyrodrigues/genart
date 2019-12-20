module View exposing (view)

import Colors
import Components exposing (..)
import Element as El exposing (Color, Element)
import Html
import Html.Events exposing (preventDefaultOn)
import Icons exposing (icons)
import Json.Decode as Decoder exposing (Decoder, bool, field, float)
import LSystem.Core as LCore
    exposing
        ( State
        , Step(..)
        , Transformation
        , buildState
        , stateLength
        )
import LSystem.Draw exposing (drawSvg, drawSvgFixed, drawSvgFixedWithColor)
import LSystem.String
import Models exposing (Model)
import Update exposing (Msg(..))


view : Model -> Html.Html Msg
view model =
    layout model.backgroundColor
        (column
            [ topRow model
            , row
                [ transformsColumn model.state model.editingIndex
                , mainResultSvg model
                ]
                |> withBorder
                |> withWidthAndHeight 1 5
                |> withSpacing 5
                |> toElement
            , colorsRow SetBackgroundColor
            , colorsRow SetDrawColor
            ]
            |> withBgColor model.backgroundColor
            |> withPadding 20
            |> withScrollbars
            |> toElement
        )


topRow : Model -> Element Msg
topRow model =
    let
        editingTransform =
            LCore.getTransformAt model.editingIndex model.state

        recState =
            { base = [ D ]
            , transforms = [ editingTransform ]
            }
    in
    styledColumn
        [ styledRow
            [ toElement <| styledButton ClearSvg "ClearSvg"
            , toElement <| styledButton (Iterate editingTransform) "Iterate"
            , toElement <| styledButton Deiterate "Deiterate"
            , toElement <| styledBox (" Fixed: " ++ boolToOnOffString model.fixed)
            ]
            |> toElement
        , styledRow
            [ column
                [ toElement <| styledBox (stateLengthToString model.state)
                , toElement <| styledBox ("{" ++ model.dir ++ "}")
                , toElement <| styledBox (LSystem.String.fromTransform editingTransform)
                ]
                |> withBorder
                |> withWidthAndHeight 1 1
                |> withScrollbars
                |> toElement
            , box (El.html <| drawSvg recState 60 60 0 0)
                |> withBorder
                |> withWidthAndHeight 1 1
                |> withScrollbars
                |> toElement
            ]
            |> withWidthAndHeight 1 4
            |> toElement
        ]
        |> toElement


mainResultSvg : Model -> Element Msg
mainResultSvg model =
    let
        { state, drawColor, zoomLevel, wDelta, hDelta } =
            model

        drawnSvg =
            if model.fixed then
                svgDivFixed state drawColor

            else
                svgDiv state zoomLevel wDelta hDelta
    in
    box (El.html drawnSvg)
        |> withScrollbars
        |> withBorder
        |> withWidthAndHeight 7 1
        |> withAttributes [ modifyWheelEvent ]
        |> toElement



-- TRANSFORMS COLUMN (left sidebar)


transformsColumn : State -> Int -> Element Msg
transformsColumn state editingIndex =
    let
        transforms =
            state
                |> LCore.toList
                |> List.indexedMap (styledTransformBox editingIndex)
                |> List.reverse
    in
    column transforms
        |> withWidthAsLength (El.minimum 200 El.fill)
        |> withHeightFill
        |> withScrollbars
        |> withBorder
        |> toElement


styledTransformBox : Int -> Int -> Transformation -> Element Msg
styledTransformBox editingIndex index transform =
    row
        [ box (El.html (drawSvgFixed transform))
            |> withWidthAndHeight 1 1
            |> toElement
        , toElement <|
            column
                -- [ El.html <| Icons.draw24px icons.eye
                [ penIcon index editingIndex
                , trashIcon index
                ]
        ]
        --|> withWidthAsLength (El.fill |> El.minimum 80 |> El.maximum 80)
        |> withHeightAsLength (El.px 80)
        |> withWidthFill
        |> withBgColor Colors.gray
        |> withBorder
        |> toElement


penIcon : Int -> Int -> Element Msg
penIcon index editingIndex =
    icon
        (SetEditingIndex index)
        (index == editingIndex)
        Colors.green_
        icons.pen


trashIcon : Int -> Element Msg
trashIcon index =
    icon
        (DropFromState index)
        True
        Colors.red_
        icons.trash



-- TO STRING


boolToOnOffString : Bool -> String
boolToOnOffString bool =
    if bool then
        "On"

    else
        "Off"


stateLengthToString : State -> String
stateLengthToString state =
    let
        length =
            stateLength state
    in
    String.fromInt (Tuple.first length)
        ++ ", "
        ++ String.fromInt (Tuple.second length)



-- SVG DRAWING


svgDivFixed : State -> Color -> Html.Html Msg
svgDivFixed state drawColor =
    drawSvgFixedWithColor drawColor <| buildState state


svgDiv : State -> Float -> Float -> Float -> Html.Html Msg
svgDiv state zoomLevel widthDelta heightDelta =
    Html.div []
        [ drawSvg
            state
            (mapZoomLevelToSize zoomLevel)
            (mapZoomLevelToSize zoomLevel)
            widthDelta
            heightDelta
        ]


mapZoomLevelToSize : Float -> Float
mapZoomLevelToSize zl =
    max 10.0 (zl * 4.0)



-- ZOOM


modifyWheelEvent : El.Attribute Msg
modifyWheelEvent =
    -- Refactor modifyWheelEvent/alwaysPreventDefault/wheelDecoder out of here
    El.htmlAttribute <| preventDefaultOn "wheel" (Decoder.map alwaysPreventDefault wheelDecoder)


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


wheelDecoder : Decoder Msg
wheelDecoder =
    Decoder.map3 Zoom
        (field "deltaX" float)
        (field "deltaY" float)
        (field "shiftKey" bool)
