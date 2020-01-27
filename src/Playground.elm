module Playground exposing (view)

import Colors exposing (Color, toCssColor)
import Css
    exposing
        --( Color
        ( backgroundColor
        , fixed
        , height
          --, hex
        , left
        , overflow
        , pct
        , position
          --, rgb
        , scroll
        , top
        , width
        )
import Html
import Html.Styled exposing (Html, div, fromUnstyled, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Icons exposing (withColor, withConditionalColor, withOnClick)
import LSystem.Core as LCore
    exposing
        ( Step(..)
        , Transformation
        , buildState
        , stateLength
        )
import LSystem.Draw exposing (drawSvgFixed, drawSvgFixedWithColor)
import LSystem.String
import Models exposing (Model)
import Update exposing (Msg(..))


view : Model -> Html.Html Msg
view model =
    div
        [ css [ width (pct 100), height (pct 100) ] ]
        [ topRow model
        , leftPane model
        , rightPane model
        ]
        |> toUnstyled


topRow : Model -> Html Msg
topRow model =
    let
        editingTransform =
            LCore.getTransformAt model.editingIndex model.state

        fixedOrZoomStatus =
            " Fixed: "
                ++ (if model.fixed then
                        "On"

                    else
                        "Off"
                   )

        stateLengthString =
            Debug.toString (stateLength model.state)

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
            , backgroundColor (toCssColor model.backgroundColor)
            ]
        ]
        [ div []
            [ Html.Styled.button [ onClick ClearSvg ] [ text "ClearSvg" ]
            , Html.Styled.button [ onClick (Iterate editingTransform) ] [ text "Iterate" ]
            , Html.Styled.button [ onClick Deiterate ] [ text "Deiterate" ]
            , span [] [ text fixedOrZoomStatus ]
            ]
        , div []
            [ p [] [ text stateLengthString ]
            , p [] [ text dir ]
            , p [] [ text editingTransformBlueprint ]
            ]
        ]


leftPane : Model -> Html Msg
leftPane model =
    let
        transforms =
            model.state
                |> LCore.toList
                |> List.indexedMap (transformBox model.editingIndex)
                |> List.reverse
    in
    fixedDiv
        [ css
            [ backgroundColor (toCssColor model.backgroundColor)
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
        [ div [] [ drawSvgFixed transform ]
        , Icons.trash
            |> withColor Colors.red_
            |> withOnClick (DropFromState index)
            |> Icons.toSvg
        , Icons.pen
            |> withConditionalColor (index == editingIndex) Colors.green_
            |> withOnClick (SetEditingIndex index)
            |> Icons.toSvg
        ]


rightPane : Model -> Html Msg
rightPane model =
    fixedDiv
        [ css
            [ backgroundColor (toCssColor model.backgroundColor)
            , position fixed
            , height (pct layout.middleRow)
            , width (pct layout.rightPane)
            , top (pct layout.topRow)
            , left (pct layout.leftPane)
            ]
        ]
        [ drawSvgFixedWithColor model.drawColor (buildState model.state)
        ]



{- A plain old record holding a couple of theme colors.
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
-}


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
