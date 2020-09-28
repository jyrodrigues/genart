module Components.Dropdown exposing (Config, Msg, State, View, close, customView, init, update, view)

import Colors
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, tabindex)
import Html.Styled.Events exposing (on, onBlur, onClick, onMouseEnter, onMouseLeave)
import Json.Decode as Decode
import Utils


type alias State =
    { isOpen : Bool
    , mouseIsInside : Bool
    }


type alias Config msg =
    { title : String
    , elements : List (Html msg)
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
customView styles { title, elements, toMsg } state =
    div
        [ css (wrapperCss state.isOpen ++ styles)
        , onMouseEnter (toMsg MouseEnter)
        , onMouseLeave (toMsg MouseLeave)
        ]
        (div
            [ css
                [ color (Colors.toCssColor Colors.white)
                , cursor pointer
                ]
            ]
            [ text title ]
            :: body toMsg elements state.isOpen
        )


wrapperCss : Bool -> List Style
wrapperCss isOpen =
    let
        activeStyle =
            if isOpen then
                [ backgroundColor (Colors.toCssColor Colors.black)
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


body : (Msg -> msg) -> List (Html msg) -> Bool -> List (Html msg)
body toMsg elements isOpen =
    if isOpen then
        [ div
            [ css
                [ position absolute
                , top (calc (pct 100) plus (px 10))
                , zIndex (int 1)
                , backgroundColor (Colors.toCssColor Colors.darkGray)
                , borderRadius (px 5)
                , color (Colors.toCssColor Colors.offWhite)
                , border3 (px 1) solid (Colors.toCssColor Colors.darkGray)
                , overflow hidden
                ]
            , onMouseEnter (toMsg MouseEnter)
            , onMouseLeave (toMsg MouseLeave)
            ]
            elements
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
