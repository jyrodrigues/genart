module Experiment exposing (experiment)

import Browser
import Draw exposing (drawSvg)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html
import Html.Attributes
import LSystem exposing (Step(..))
import Msgs


type alias Model =
    Int


update : Msgs.Msg -> Model -> Model
update msg model =
    model



-- VIEW


view model =
    withBorder layout
        [ width fill
        , height fill
        , Background.color <| rgba255 255 0 0 0.2
        ]
    <|
        column [ width fill, height fill ]
            [ elShouldNotScroll [ width fill, height (fillPortion 2) ]
            , row [ width fill, height (fillPortion 2), scrollbars ]
                [ elShouldNotScroll [ width (fillPortion 1), height fill ]
                , column [ width fill, height fill, scrollbars ]
                    [ elShouldNotScroll [ width fill, height (fillPortion 2) ]
                    , withBorder el [ width fill, height (fillPortion 5 |> maximum 300), scrollbars ] testDiv
                    , elShouldNotScroll [ width fill, height (fillPortion 2) ]
                    ]
                , elShouldNotScroll [ width (fillPortion 1), height fill ]
                ]
            , elShouldNotScroll [ width fill, height (fillPortion 2) ]
            ]


elShouldNotScroll attr =
    withBorder el attr (text "This should *not* scroll")


withBorder e a b =
    e
        ([ Border.color <| rgb 0 0 0
         , Border.solid
         , Border.width 1
         ]
            ++ a
        )
        b



-- Uncomment here to see changes
-- testDiv = text "Parent layout changed!"


{--}
testDiv =
    html <|
        Html.div
            [ Html.Attributes.style "height" "2000px"
            , Html.Attributes.style "background-color" "rgba(0, 255, 0, 0.2)"
            ]
        <|
            [ svgMock ]



{--
            List.repeat 50 (Html.div [] [ Html.text "This should scroll ........................................>>>>>>>>>>>>>>>>>>>>>" ])
            --}
--}



-- MAIN


experiment : Program () Model Msgs.Msg
experiment =
    Browser.sandbox
        { init = 0
        , view = view
        , update = update
        }



-- SVG MOCK


svgMock =
    drawSvg (List.foldr (++) [] <| List.repeat 1000 [ D, R, D, L, D, L, D, R ]) 1200 1200
