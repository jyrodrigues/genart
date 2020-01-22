module Playground exposing (view)

import Colors exposing (toElmCssColor)
import Components exposing (..)
import Css exposing (..)
import Element as El
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
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
import View exposing (..)


view : Model -> Html.Html Msg
view =
    styledView >> toUnstyled


styledView : Model -> Html Msg
styledView model =
    div
        [ css [ width (pct 100), height (pct 100) ] ]
        [ topRow model
        , leftPane model
        , rightPane model
        , bottomRow
        ]


topRow : Model -> Html Msg
topRow model =
    let
        editingTransform =
            LCore.getTransformAt model.editingIndex model.state

        fixedOrZoomStatus =
            " Fixed: " ++ boolToOnOffString model.fixed

        stateLength =
            stateLengthToString model.state

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
            ]
        ]
        [ div []
            [ Html.Styled.button [ onClick ClearSvg ] [ text "ClearSvg" ]
            , Html.Styled.button [ onClick (Iterate editingTransform) ] [ text "Iterate" ]
            , Html.Styled.button [ onClick Deiterate ] [ text "Deiterate" ]
            , span [] [ text fixedOrZoomStatus ]
            ]
        , div []
            [ p [] [ text stateLength ]
            , p [] [ text dir ]
            , p [] [ text editingTransformBlueprint ]
            ]
        ]


leftPane : Model -> Html Msg
leftPane model =
    let
        intoDiv el =
            div [] [ el ]

        transforms =
            model.state
                |> LCore.toList
                |> List.indexedMap (transformBox model.editingIndex)
                |> List.reverse
    in
    fixedDiv
        [ css
            [ backgroundColor theme.g
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
        [ div [] [ fromUnstyled (drawSvgFixed transform) ]
        , elToStyled (trashIcon index)
        , elToStyled (penIcon index (index == editingIndex))
        ]


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


elToStyled =
    fromUnstyled << El.layout []


fixedDiv : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
fixedDiv attrs children =
    div
        (css
            [ position fixed ]
            :: attrs
        )
        children
