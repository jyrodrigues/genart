module Components exposing (..)

import Colors exposing (toCssColor)
import Css exposing (active, auto, backgroundColor, block, border, border3, borderBottom3, borderBox, borderRadius, boxShadow5, boxSizing, calc, center, color, cursor, default, display, displayFlex, fixed, flexWrap, fontFamily, fontSize, height, hover, left, lineHeight, margin2, marginBottom, minWidth, minus, none, padding, pct, pointer, position, px, relative, right, sansSerif, solid, textAlign, textDecoration, top, transparent, width, wrap, zIndex, zero)
import Html.Styled exposing (Html, a, button, div, span, text)
import Html.Styled.Attributes exposing (class, css, href)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed



-- HTML ATTRIBUTES HELPERS


fixedDiv : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
fixedDiv attrs children =
    div
        (css
            [ position fixed ]
            :: attrs
        )
        children


{-| ALERT
-}
alert : String -> Html msg
alert text_ =
    let
        height_ =
            60

        width_ =
            200
    in
    -- Adding a keyed node here is a trick:
    -- It allows us to force a re-render and therefore restart the animation
    -- (bound to the class "alert" in `Transition.css`)
    Keyed.node "div"
        []
        [ ( text_
          , div
                [ css
                    [ position fixed
                    , left (calc (pct 50) minus (px (width_ / 2)))
                    , top (px 50)

                    -- , top (calc (pct 50) minus (px (height_ / 2)))
                    -- , right (px 10)
                    , zIndex (Css.int 10)
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
                    , fontSize (px 16)
                    ]
                , class "alert"
                ]
                [ text text_ ]
          )
        ]


{-| BUTTONS
-}
primaryButtonStyle : List Css.Style
primaryButtonStyle =
    let
        lightGray =
            toCssColor Colors.lightGray

        buttonHeight =
            28

        borderWidth =
            1
    in
    [ color lightGray
    , backgroundColor transparent
    , border3 (px borderWidth) solid lightGray
    , borderRadius (px 6)
    , height (px buttonHeight)
    , width (pct 95)

    {--
    , withMedia [ Media.all [ Media.maxWidth (px 1160), Media.minWidth (px 650) ] ]
        [ minWidth (px 85)
        , height (px 60)
        ]
    --}
    --, withMedia [ Media.all [ Media.minWidth (px 1700) ] ] [ width (px 106) ]
    , margin2 (px 5) auto
    , display block
    , cursor pointer
    , boxSizing borderBox

    -- TODO add font files and @font-face:
    -- https://stackoverflow.com/questions/107936/how-to-add-some-non-standard-font-to-a-website
    -- https://fonts.google.com/specimen/Roboto?selection.family=Roboto
    -- Research best fallback option
    --, fontFamilies [ "Roboto" ]
    , fontFamily sansSerif
    , fontSize (px 16)
    , textAlign center
    , textDecoration none
    , lineHeight (px (buttonHeight - 2 * borderWidth))
    , hover primaryButtonStyleHover
    ]


primaryButtonStyleHover : List Css.Style
primaryButtonStyleHover =
    let
        -- COPIED FROM ABOVE
        lightGray =
            toCssColor Colors.lightGray

        darkGray =
            toCssColor Colors.darkGray

        buttonHeight =
            28
    in
    [ border Css.unset
    , backgroundColor lightGray
    , color darkGray
    , active primaryButtonStyleActive
    , lineHeight (px buttonHeight)
    ]


primaryButtonStyleActive : List Css.Style
primaryButtonStyleActive =
    [ backgroundColor (Colors.toCssColor Colors.activeElementGray) ]


primaryButtonStyled : List Css.Style -> msg -> String -> Html msg
primaryButtonStyled style msg btnText =
    button [ css (primaryButtonStyle ++ style), onClick msg ] [ text btnText ]


primaryButton : msg -> String -> Html msg
primaryButton =
    primaryButtonStyled []


halfStyle : List Css.Style
halfStyle =
    [ minWidth (pct 45), width (pct 45) ]


primaryButtonHalf : msg -> String -> Html msg
primaryButtonHalf =
    primaryButtonStyled halfStyle


primaryButtonSelectable : Bool -> msg -> String -> Html msg
primaryButtonSelectable isSelected =
    primaryButtonStyled
        (if isSelected then
            primaryButtonStyleHover ++ primaryButtonStyleActive

         else
            []
        )


anchorButtonStyled : List Css.Style -> String -> String -> Html msg
anchorButtonStyled style href_ title =
    a [ href href_, css (primaryButtonStyle ++ style) ] [ text title ]


anchorButton : String -> String -> Html msg
anchorButton =
    anchorButtonStyled []


anchorButtonHalf : String -> String -> Html msg
anchorButtonHalf =
    anchorButtonStyled halfStyle


{-| CONTROL BLOCK
-}
controlBlockStyle : List Css.Style
controlBlockStyle =
    [ padding (px 10), borderBottom3 (px 1) solid (toCssColor Colors.black), fontFamily Css.sansSerif ]


controlBlock : String -> List (Html msg) -> Html msg
controlBlock title list =
    div [ css controlBlockStyle ]
        (span [ css [ display block, marginBottom (px 8), cursor default, fontSize (px 18) ] ] [ text title ]
            :: list
        )


controlBlockFlex : List (Html msg) -> Html msg
controlBlockFlex =
    div [ css (controlBlockStyle ++ [ displayFlex, flexWrap wrap ]) ]



-- MISC


separator : Html msg
separator =
    div
        [ css
            [ width (px 2)
            , height (pct 80)
            , position relative
            , top (pct 10)
            , backgroundColor
                (Colors.black
                    |> Colors.updateAlpha 0.3
                    |> Colors.toCssColor
                )
            ]
        ]
        []
