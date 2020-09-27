module Components.Dropdown exposing (Config, Msg, State, init, update, view)

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


view : Config msg -> State -> Html msg
view { title, elements, toMsg } state =
    div
        [ css
            [ position relative
            , maxHeight (pct 100)
            ]
        , onMouseEnter (toMsg MouseEnter)
        , onMouseLeave (toMsg MouseLeave)
        ]
        (div
            [ css
                [ color (Colors.toCssColor Colors.offWhite)
                , backgroundColor <|
                    if state.isOpen then
                        Colors.toCssColor Colors.black

                    else
                        Colors.toCssColor Colors.darkGray
                , cursor pointer
                , padding (px 10)
                ]
            ]
            [ text title ]
            :: body toMsg elements state.isOpen
        )


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
            ( { state | mouseIsInside = False }, Utils.delay 300 (toMsg Close) )

        Close ->
            ( { state
                | isOpen =
                    if not state.mouseIsInside then
                        False

                    else
                        state.isOpen
              }
            , Cmd.none
            )
