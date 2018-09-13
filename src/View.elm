module View exposing (view)

-- import Html exposing (Html, button, div, text)
-- import Html.Attributes exposing (style)
-- import Html.Events exposing (onClick)

import Draw exposing (drawSvg)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html
import Html.Attributes
import LSystem exposing (apply, stateToString)
import Models exposing (Model)
import Msgs exposing (Msg(..))


size : Float
size =
    1500.0


withBorder e a b =
    e
        ([ Border.color <| rgb 0 0 0
         , Border.solid
         , Border.width 1
         ]
            ++ a
        )
        b


hiel w h =
    let
        wf =
            fillPortion w

        hf =
            fillPortion h
    in
    withBorder el [ width wf, height hf ] (text "Hi")


svghere model =
    el
        [ width (fillPortion 3)
        , height (fillPortion 1)
        , scrollbars
        ]
        (html <| svgDiv model)


view model =
    withBorder layout
        [ width fill
        , height fill
        , Background.color <| rgba255 255 0 0 0.2
        ]
    <|
        column [ width fill, height fill ]
            [ hiel 1 1
            , row [ width fill, height (fillPortion 10), scrollbars ]
                [ hiel 1 1
                , column [ width (fillPortion 3), scrollbars ]
                    [ hiel 1 20
                    , svghere model
                    , hiel 1 20
                    ]
                ]
            , hiel 1 1
            ]



{--
view2 : Model -> Html Msg
view2 model =
    div []
        [ div mystyle
            [ button [ onClick <| SaveState ] [ text "Save State" ]
            , button [ onClick Backspace ] [ text "Backspace" ]
            , button [ onClick ClearStep ] [ text "Clear Step" ]
            , button [ onClick ClearSvg ] [ text "Clear Svg" ]
            , button [ onClick Iterate ] [ text "Iterate" ]
            , button [ onClick Deiterate ] [ text "Deiterate" ]
            , button [ onClick ToggleShowNextIteration ] [ text "Toggle" ]
            , text <|
                "Status: "
                    ++ (if model.isShowingNextIteration then
                            "On"

                        else
                            "Off"
                       )
            , text <|
                "  Rec: "
                    ++ (if model.recOn then
                            "On"

                        else
                            "Off"
                       )
            , div []
                [ text (String.fromInt <| List.length model.state)
                , text <| "->" ++ model.dir ++ "<-"
                , text (String.fromInt <| (*) (List.length model.recording) <| List.length model.state)
                ]
            , div [ Html.Attributes.style "display" "inline-block" ] [ text <| stateToString model.recording ]
            , div
                [ Html.Attributes.style "display" "block "
                ]
                [ drawSvg model.recording 120 80
                ]
            ]
        , svgDiv model
        ]
--}


svgDiv model =
    Html.div
        [ Html.Attributes.style "display" "block "
        , Html.Attributes.style "border" "1px solid black"
        , Html.Attributes.style "height" "100%"
        ]
        [ drawSvg
            (if model.isShowingNextIteration then
                apply model.recording model.state

             else
                model.state
            )
            size
            size
        ]



{--
mystyle : List (Html.Attribute msg)
mystyle =
    [ Html.Attributes.style "width" "100%"
    , Html.Attributes.style "height" "130px"
    , Html.Attributes.style "border" "1px solid black"
    ]
--}
