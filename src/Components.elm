module Components exposing (..)

import Colors exposing (toCssColor)
import Css
    exposing
        ( backgroundColor
        , border3
        , borderRadius
        , boxShadow5
        , calc
        , center
        , color
        , fixed
        , fontFamily
        , fontSize
        , height
        , left
        , lineHeight
        , minus
        , pct
        , position
        , px
        , sansSerif
        , solid
        , textAlign
        , top
        , width
        , zero
        )
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class, css)


alert : String -> Html msg
alert text_ =
    let
        height_ =
            60

        width_ =
            200
    in
    div
        [ css
            [ position fixed
            , left (calc (pct 50) minus (px (width_ / 2)))
            , top (px 30)
            , width (px width_)
            , height (px height_)
            , backgroundColor (toCssColor Colors.darkGray)
            , color (toCssColor Colors.offWhite)
            , border3 (px 1) solid (toCssColor Colors.black)
            , boxShadow5 zero zero (px 5) (px 1) (toCssColor Colors.blackShadow)
            , borderRadius (px 10)
            , textAlign center
            , lineHeight (px height_)
            , fontFamily sansSerif
            , fontSize (px 20)
            ]
        , class "alert"
        ]
        [ text text_ ]
