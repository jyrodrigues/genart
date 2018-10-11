module View exposing (view)

-- import Html exposing (Html, button, div, text)
-- import Html.Attributes exposing (style)
-- import Html.Events exposing (onClick)

import Draw exposing (drawSvg)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input exposing (button)
import Html
import Html.Attributes
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decoder exposing (Decoder, bool, field, float)
import LSystem exposing (apply, stateToString)
import Models exposing (Model)
import Msgs exposing (Msg(..))


size : Float
size =
    1500.0


addBorder =
    [ Border.color <| rgba 0 0 0 0.8
    , Border.solid
    , Border.width 1
    ]


filling w h =
    [ width <| fillPortion w
    , height <| fillPortion h
    ]


bf11 =
    addBorder ++ filling 1 1


view : Model -> Html.Html Msg
view model =
    layout
        [ width fill
        , height fill
        , Background.color <| rgba255 0 100 200 0.5
        , padding 20
        ]
    <|
        column
            [ width fill
            , height fill
            , Background.color <| rgb255 250 250 220
            , padding 20
            , scrollbars
            ]
            [ column (bf11 ++ [ scrollbars, spacing 5 ])
                [ row (bf11 ++ [ scrollbars, spacing 5 ])
                    [ button bf11 { onPress = Just SaveState, label = text "Save State" }
                    , button bf11 { onPress = Just Backspace, label = text "Backspace" }
                    , button bf11 { onPress = Just ClearStep, label = text "ClearStep" }
                    , button bf11 { onPress = Just ClearSvg, label = text "ClearSvg" }
                    , button bf11 { onPress = Just Iterate, label = text "Iterate" }
                    , button bf11 { onPress = Just Deiterate, label = text "Deiterate" }
                    , button bf11 { onPress = Just ToggleShowNextIteration, label = text "ToggleShowNextIteration" }
                    , el bf11 (text <| "Status: " ++ onOff model.isShowingNextIteration)
                    , el bf11 (text <| " Rec: " ++ onOff model.recOn)
                    ]
                , row (filling 1 4 ++ [ scrollbars, spacing 5 ])
                    [ column (bf11 ++ [ scrollbars ])
                        [ el (filling 1 1) (text (String.fromInt <| List.length model.state))
                        , el (filling 1 1) (text <| "->" ++ model.dir ++ "<-")
                        , el (filling 1 1) (text (String.fromInt <| (*) (List.length model.recording) <| List.length model.state))
                        , el (filling 1 1) (text <| stateToString model.recording)
                        ]
                    , el (addBorder ++ filling 1 1 ++ [ scrollbars ]) (html <| drawSvg model.recording 120 80 0 0)
                    ]
                ]
            , row (addBorder ++ filling 1 5 ++ [ scrollbars, spacing 5 ])
                [ column (addBorder ++ filling 1 1)
                    [ el (filling 1 1) (text "whoa")
                    , el (filling 1 1) (text (String.fromFloat model.zoomLevel))
                    ]
                , el (addBorder ++ filling 7 1 ++ [ scrollbars, modifyWheelEvent ]) (html <| svgDiv model)
                ]
            ]


onOff bool =
    if bool then
        "On"

    else
        "Off"


modifyWheelEvent =
    htmlAttribute <| preventDefaultOn "wheel" (Decoder.map alwaysPreventDefault myDecoder)


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


myDecoder : Decoder Msg
myDecoder =
    Decoder.map3 Zoom
        (field "deltaX" float)
        (field "deltaY" float)
        (field "shiftKey" bool)


svgDiv : Model -> Html.Html Msg
svgDiv model =
    Html.div []
        [ drawSvg
            (if model.isShowingNextIteration then
                apply model.recording model.state

             else
                model.state
            )
            (mapZoomLevelToSize model.zoomLevel)
            (mapZoomLevelToSize model.zoomLevel)
            model.wDelta
            model.hDelta
        ]


mapZoomLevelToSize zl =
    max 10.0 (zl * 4.0)
