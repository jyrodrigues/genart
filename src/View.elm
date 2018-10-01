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


addBorder =
    [ Border.color <| rgba 0 0 0 0.8
    , Border.solid
    , Border.width 1
    ]


filling w h =
    [ width <| fillPortion w
    , height <| fillPortion h
    ]


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
            , Background.color <| rgba255 200 100 0 0.5
            , padding 20
            , scrollbars
            ]
            [ el (addBorder ++ filling 1 1 ++ [ scrollbars ]) (genText 50)
            , row (addBorder ++ filling 1 7 ++ [ scrollbars, spacing 5, padding 5 ])
                [ el (addBorder ++ filling 1 1) (text "whoa")
                , el (addBorder ++ filling 7 1 ++ [ scrollbars ]) (genText 50) --(html <| svgDiv model)
                ]
            ]


genText textSize =
    List.repeat textSize "abcdefghijklmnopqrstuvwxyz\n12345678901234567890123456\n"
        |> List.foldl (++) ""
        |> text



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
    Html.div []
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
