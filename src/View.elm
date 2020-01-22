module View exposing (..)

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



-- TRANSFORMS COLUMN (left sidebar)


penIcon : Int -> Bool -> Element Msg
penIcon index isEditing =
    icon
        (SetEditingIndex index)
        isEditing
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
