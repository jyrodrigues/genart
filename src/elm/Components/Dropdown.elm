module Components.Dropdown exposing
    ( Config
    , Msg
    , State
    , View
    , close
    , customView
    , flexListItemStyle
    , init
    , listItemStyle
    , update
    , view
    )

import Colors
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onMouseEnter, onMouseLeave)
import Utils


type alias State =
    { isOpen : Bool
    , mouseIsInside : Bool
    }


type alias Config msg =
    { title : String
    , body : Html msg
    , toMsg : Msg -> msg
    }


type Msg
    = MouseEnter
    | MouseLeave
    | Close


init : Bool -> State
init isOpen =
    { isOpen = isOpen
    , mouseIsInside = False
    }


type alias View msg =
    Config msg -> State -> Html msg


view : View msg
view =
    customView []


customView : List Style -> View msg
customView styles { title, body, toMsg } state =
    div
        [ css (wrapperCss state.isOpen ++ styles)
        , onMouseEnter (toMsg MouseEnter)
        , onMouseLeave (toMsg MouseLeave)
        ]
        (div
            [ css
                [ color (Colors.toCssColor Colors.white)
                , cursor default
                ]
            ]
            [ text title ]
            :: wrapBody toMsg body state.isOpen
        )


wrapperCss : Bool -> List Style
wrapperCss isOpen =
    let
        activeStyle =
            if isOpen then
                [ backgroundColor (Colors.toCssColor Colors.black)

                {--
                box-shadow: 12px 0 15px -4px #111, -12px 0 15px -4px #111;
                border-radius: 5px;
                --}
                ]

            else
                []
    in
    [ position relative
    , height (pct 100)
    , padding2 zero (px 15)
    , displayFlex
    , alignItems center
    , justifyContent center
    ]
        ++ activeStyle


wrapBody : (Msg -> msg) -> Html msg -> Bool -> List (Html msg)
wrapBody toMsg element isOpen =
    if isOpen then
        [ div
            [ css
                [ position absolute
                , top (calc (pct 100) plus (px 8))
                , zIndex (int 1)
                , overflow hidden
                , backgroundColor (Colors.toCssColor Colors.darkGray)
                , color (Colors.toCssColor Colors.offWhite)
                , boxShadow5 zero zero (px 7) (px -1) (Colors.toCssColor Colors.black)
                , border3 (px 1.5) solid (Colors.toCssColor Colors.black)
                , borderRadius (px 5)
                ]
            , onMouseEnter (toMsg MouseEnter)
            , onMouseLeave (toMsg MouseLeave)
            ]
            [ element ]
        ]

    else
        []


update : (Msg -> msg) -> Msg -> State -> ( State, Cmd msg )
update toMsg msg state =
    case msg of
        MouseEnter ->
            ( { isOpen = True, mouseIsInside = True }, Cmd.none )

        MouseLeave ->
            ( { state | mouseIsInside = False }, Utils.delay 200 (toMsg Close) )

        Close ->
            ( { state
                | isOpen =
                    if not state.mouseIsInside then
                        False
                        --True

                    else
                        state.isOpen
              }
            , Cmd.none
            )


close : State -> State
close state =
    { state | isOpen = False }



-- STYLES PREDEFINED


listItemStyle : Bool -> List Style
listItemStyle isActive =
    let
        activeAttrs =
            if isActive then
                [ backgroundColor (Colors.toCssColor Colors.theme.active) ]

            else
                []
    in
    [ display block
    , width (pct 100)
    , padding2 (px 15) (px 20)
    , color (Colors.toCssColor Colors.offWhite)
    , cursor pointer
    , borderBottom3 (px 1) solid (Colors.toCssColor Colors.theme.active)
    , textAlign start
    , textDecoration none
    , hover
        [ backgroundColor (Colors.toCssColor Colors.theme.hover) ]
    , active
        [ backgroundColor (Colors.toCssColor Colors.theme.active) ]
    ]
        ++ activeAttrs


flexListItemStyle : Bool -> List Style
flexListItemStyle isActive =
    listItemStyle isActive ++ [ Css.flexGrow (Css.int 1) ]
