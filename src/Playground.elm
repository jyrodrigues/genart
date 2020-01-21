module Playground exposing (view)

import Colors exposing (toElmCssColor)
import Components exposing (..)
import Css exposing (..)
import Element as El
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import LSystem.Core
    exposing
        ( State
        , Step(..)
        , Transformation
        , buildState
        , stateLength
        )
import LSystem.Draw exposing (drawSvg, drawSvgFixed, drawSvgFixedWithColor)
import Models exposing (Model)
import Update exposing (Msg(..))
import View exposing (..)


view : Model -> Html.Html Msg
view =
    styledView >> toUnstyled


styledView : Model -> Html Msg
styledView model =
    div
        [ css [ width (pct 100), height (pct 100) ] ]
        [ topRow
        , leftPane model
        , rightPane model
        , bottomRow
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


topRow : Html msg
topRow =
    fixedDiv
        [ css
            [ backgroundColor theme.r
            , height (pct layout.topRow)
            , width (pct 100)
            ]
        ]
        [ text "top row" ]


leftPane : Model -> Html Msg
leftPane model =
    fixedDiv
        [ css
            [ backgroundColor theme.g
            , height (pct layout.middleRow)
            , width (pct layout.leftPane)
            , top (pct layout.topRow)
            ]
        ]
        [ transformsColumn model.state model.editingIndex
            |> withBgColor model.backgroundColor
            |> toElement
            |> elToStyled
        ]


elToStyled =
    fromUnstyled << El.layout []


rightPane : Model -> Html Msg
rightPane model =
    fixedDiv
        [ css
            [ backgroundColor (toElmCssColor model.backgroundColor)
            , position fixed
            , height (pct layout.middleRow)
            , width (pct layout.rightPane)
            , top (pct layout.topRow)
            , left (pct layout.leftPane)
            ]
        ]
        [ drawSvgFixedWithColor model.drawColor (buildState model.state)
            |> fromUnstyled
        ]


bottomRow : Html Msg
bottomRow =
    fixedDiv
        [ css
            [ backgroundColor theme.y
            , height (pct (100 - layout.topRow - layout.middleRow))
            , width (pct 100)
            , bottom zero
            ]
        ]
        [ fixedDiv
            [ css
                [ height (pct 5)
                , width (pct 100)
                ]
            ]
            [ elToStyled
                (colorsRow
                    SetBackgroundColor
                )
            ]
        , fixedDiv
            [ css
                [ height (pct 5)
                , width (pct 100)
                , bottom zero
                ]
            ]
            [ elToStyled
                (colorsRow
                    SetDrawColor
                )
            ]
        ]


{-| A plain old record holding a couple of theme colors.
-}
theme :
    { secondary : Color
    , primary : Color
    , r : Color
    , g : Color
    , b : Color
    , y : Color
    , c : Color
    , m : Color
    }
theme =
    { primary = hex "55af6a"
    , secondary = rgb 250 240 230
    , r = rgb 251 150 150
    , g = rgb 150 251 150
    , b = rgb 150 150 251
    , y = rgb 251 251 150
    , c = rgb 150 251 251
    , m = rgb 251 150 251
    }
