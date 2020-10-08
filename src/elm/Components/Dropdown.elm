module Components.Dropdown exposing
    ( Config
    , Model
    , Msg
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
import Html.Styled.Attributes exposing (css, tabindex)
import Html.Styled.Events exposing (on, onBlur, onClick, onMouseEnter, onMouseLeave)
import Json.Decode as Decode
import Utils


type alias Model msg =
    { isOpen : Bool
    , toMsg : Msg -> msg
    , id : String
    }


type alias Config msg =
    { title : String
    , body : Html msg
    }


type Msg
    = TitleClicked
    | OnMouseDown OutsideTarget


type alias OutsideTarget =
    Bool


init : (Msg -> msg) -> Int -> Model msg
init toMsg idNumber =
    { isOpen = False
    , toMsg = toMsg
    , id = "dropdown-id-" ++ String.fromInt idNumber
    }


type alias View msg =
    Config msg -> Model msg -> Html msg


view : View msg
view =
    customView []


customView : List Style -> View msg
customView styles { title, body } { isOpen, toMsg } =
    div
        [ css (wrapperCss isOpen ++ styles) ]
        (div
            -- This msg doesn't have the same semantic meaning as a click, but it's ok!
            [ onClick (toMsg TitleClicked)
            , css
                [ color (Colors.toCssColor Colors.white)
                , cursor pointer
                ]
            ]
            [ text title ]
            :: wrapBody toMsg body isOpen
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
            ]
            [ element ]
        ]

    else
        []


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        TitleClicked ->
            ( { model | isOpen = not model.isOpen }, Cmd.none )

        OnMouseDown outsideTarget ->
            if outsideTarget then
                ( { model | isOpen = False }, Cmd.none )

            else
                ( model, Cmd.none )


close : Model msg -> Model msg
close model =
    { model | isOpen = False }



-- DECODER OnMouseDown
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
