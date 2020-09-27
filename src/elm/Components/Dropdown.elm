module Components.Dropdown exposing (Config, Msg, State, init, update, view)

import Colors
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onBlur, onClick)


type alias State =
    Bool


type alias Config msg =
    { title : String
    , elements : List (Html msg)
    , toMsg : Msg -> msg
    }


type Msg
    = ToggleOpen
    | Close


init : Bool -> State
init isOpen =
    isOpen


view : Config msg -> State -> Html msg
view { title, elements, toMsg } isOpen =
    div
        [ css
            [ position relative
            , maxHeight (pct 100)
            ]
        , onBlur (toMsg Close)
        ]
        (div
            [ onClick (toMsg ToggleOpen)
            , css
                [ color (Colors.toCssColor Colors.offWhite)
                , backgroundColor <|
                    if isOpen then
                        Colors.toCssColor Colors.black

                    else
                        Colors.toCssColor Colors.darkGray
                , cursor pointer
                , padding (px 10)
                ]

            --TODO not working
            --, onBlur (toMsg Close)
            ]
            [ text title ]
            :: body elements isOpen
        )


body : List (Html msg) -> Bool -> List (Html msg)
body elements isOpen =
    if isOpen then
        [ div
            [ css
                [ position absolute
                , top (calc (pct 100) plus (px 10))
                , zIndex (int 1)
                , backgroundColor (Colors.toCssColor Colors.darkGray)
                , padding (px 10)
                , borderRadius (px 5)
                , color (Colors.toCssColor Colors.offWhite)
                ]
            ]
            elements
        ]

    else
        []


update : Msg -> State -> State
update msg isOpen =
    case msg of
        ToggleOpen ->
            not isOpen

        Close ->
            False
