module View exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input exposing (button)
import Html
import Html.Attributes
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decoder exposing (Decoder, bool, field, float)
import LSystem.Core
    exposing
        ( State
        , Step(..)
        , applyRule
        , buildState
        , countSize
        , getLastTransform
        , stateLength
        , stateToString
        , transformToString
        )
import LSystem.Draw exposing (drawSvg, drawSvgFixed)
import Models exposing (Model)
import Update exposing (Msg(..))


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


styledButton :
    { onPress : Maybe Msg
    , label : Element Msg
    }
    -> Element Msg
styledButton =
    button <| bf11 ++ [ Background.color <| rgb 200 200 200 ]


styledEl : List (Attribute Msg) -> Element Msg -> Element Msg
styledEl attr =
    el <| attr ++ [ Background.color <| rgb 200 200 200 ]


view : Model -> Html.Html Msg
view model =
    layout
        [ width fill
        , height fill
        , Background.color <| rgba255 10 80 180 0.5
        , padding 20
        ]
    <|
        column
            [ width fill
            , height fill
            , Background.color <| rgba255 15 15 60 0.95
            , padding 20
            , scrollbars
            ]
            [ topRow model
            , row (addBorder ++ filling 1 5 ++ [ spacing 5 ])
                [ column (addBorder ++ filling 1 1 ++ [ scrollbars ])
                    (List.indexedMap elFromState model.savedStates)
                , el (addBorder ++ filling 7 1 ++ [ scrollbars, modifyWheelEvent ])
                    (html <|
                        if model.fixed then
                            svgDivFixed model

                        else
                            svgDiv model
                    )
                ]
            ]


topRow : Model -> Element Msg
topRow model =
    let
        recState =
            { base = [ D ]
            , transforms = [ getLastTransform model.state ]
            }
    in
    column (bf11 ++ [ scrollbars, spacing 5 ])
        [ row (bf11 ++ [ scrollbars, spacing 5 ])
            [ styledButton { onPress = Just SaveState, label = text "Save State" }
            , styledButton { onPress = Just ResetStep, label = text "ResetStep" }
            , styledButton { onPress = Just ClearSvg, label = text "ClearSvg" }
            , styledButton { onPress = Just (Iterate <| getLastTransform model.state), label = text "Iterate" }
            , styledButton { onPress = Just Deiterate, label = text "Deiterate" }
            , styledButton { onPress = Just ToggleShowNextIteration, label = text "ToggleShowNextIteration" }
            , styledEl bf11 (text <| "Status: " ++ onOff model.isShowingNextIteration)
            , styledEl bf11 (text <| " Fixed: " ++ onOff model.fixed)
            ]
        , row (filling 1 4 ++ [ scrollbars, spacing 5 ])
            [ column (bf11 ++ [ scrollbars ])
                [ styledEl (filling 1 1)
                    (text <|
                        (String.fromInt <| Tuple.first <| stateLength model.state)
                            ++ ", "
                            ++ (String.fromInt <| Tuple.second <| stateLength model.state)
                    )
                , styledEl (filling 1 1) (text <| "->" ++ model.dir ++ "<-")
                , styledEl (filling 1 1) (text <| transformToString <| getLastTransform model.state)
                ]
            , styledEl (addBorder ++ filling 1 1 ++ [ scrollbars ]) (html <| drawSvg recState 60 60 0 0)
            ]
        ]


onOff bool =
    if bool then
        "On"

    else
        "Off"


elFromState : Int -> State -> Element Msg
elFromState index state =
    row (addBorder ++ [ height (fill |> minimum 100), width fill, Background.color <| rgb 170 170 170 ])
        [ column []
            [ styledButton { onPress = Just (Exclude index), label = text "Exclude" }
            , styledButton { onPress = Just (SetAsBase state), label = text "Use this svg" }
            , styledButton { onPress = Just (Iterate <| buildState state), label = text "Iterate" }
            , styledEl (filling 1 1)
                (text
                    ((++) "Size: " <|
                        String.fromInt <|
                            let
                                ( ds, os ) =
                                    stateLength state
                            in
                            ds + os
                    )
                )
            ]
        , el (filling 1 1) <| html <| drawSvgFixed state
        ]



-- Todo: refactor modifyWheelEvent/alwaysPreventDefault/wheelDecoder out of here


modifyWheelEvent =
    htmlAttribute <| preventDefaultOn "wheel" (Decoder.map alwaysPreventDefault wheelDecoder)


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


wheelDecoder : Decoder Msg
wheelDecoder =
    Decoder.map3 Zoom
        (field "deltaX" float)
        (field "deltaY" float)
        (field "shiftKey" bool)


svgDiv : Model -> Html.Html Msg
svgDiv model =
    Html.div []
        [ drawSvg
            -- (if model.isShowingNextIteration then
            --     applyRule model.recording model.state
            --  else
            model.state
            -- )
            (mapZoomLevelToSize model.zoomLevel)
            (mapZoomLevelToSize model.zoomLevel)
            model.wDelta
            model.hDelta
        ]


mapZoomLevelToSize zl =
    max 10.0 (zl * 4.0)


svgDivFixed : Model -> Html.Html Msg
svgDivFixed model =
    drawSvgFixed
        -- (if model.isShowingNextIteration then
        --     let
        --         state =
        --             model.state
        --     in
        --     { state | transforms = state.transforms ++ [ model.recording ] }
        --  else
        model.state



-- )
