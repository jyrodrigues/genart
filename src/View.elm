module View exposing (view)

-- import Html exposing (Html, button, div, text)
-- import Html.Attributes exposing (style)
-- import Html.Events exposing (onClick)

import Draw exposing (drawSvg)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import LSystem exposing (apply, stateToString)
import Models exposing (Model)
import Msgs exposing (Msg(..))


size : Float
size =
    1200.0


hiel wf =
    el
        [ width wf
        , height (fillPortion 1)
        , Border.color <| rgb 0 0 0
        , Border.solid
        , Border.width 1
        ]
        (text "Hi")


svghere =
    el [ width (fillPortion 3), height (fillPortion 3) ] (text "SVG")


view model =
    layout
        [ width fill
        , height fill
        , Background.color <| rgba255 255 0 0 0.2
        , Border.color <| rgb 0 0 0
        , Border.solid
        , Border.width 1
        ]
    <|
        column [ width fill, height fill ]
            [ hiel <| fillPortion 1
            , row [ width fill, height <| fillPortion 7 ]
                [ hiel (fillPortion 1)
                , svghere
                , hiel (fillPortion 1)
                ]
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
        , div
            [ Html.Attributes.style "display" "block "
            , Html.Attributes.style "border" "1px solid black"
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
        ]


mystyle : List (Html.Attribute msg)
mystyle =
    [ Html.Attributes.style "width" "100%"
    , Html.Attributes.style "height" "130px"
    , Html.Attributes.style "border" "1px solid black"
    ]

--}
