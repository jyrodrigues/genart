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
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Colors
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Utils



-- MODEL


type alias Model msg =
    { isOpen : Bool
    , toMsg : Msg -> msg
    , id : String
    }


type alias Config msg =
    { title : String
    , body : Html msg
    }



-- MSG


type Msg
    = TitleClicked
    | OnMouseDown OutsideTarget


type alias OutsideTarget =
    Bool



-- INIT


init : (Msg -> msg) -> Int -> Model msg
init toMsg idNumber =
    { isOpen = False
    , toMsg = toMsg
    , id = "dropdown-id-" ++ String.fromInt idNumber
    }



-- SUBSCRIPTIONS


subscriptions : Model msg -> Sub msg
subscriptions { isOpen, toMsg, id } =
    if isOpen then
        Browser.Events.onMouseDown (Decode.map (OnMouseDown >> toMsg) (Utils.clickAwayDecoder id))

    else
        Sub.none


type alias View msg =
    Config msg -> Model msg -> Html msg



-- VIEW


view : View msg
view =
    customView []


customView : List Style -> View msg
customView styles { title, body } { isOpen, toMsg, id } =
    div
        [ css (wrapperCss isOpen ++ styles)
        , Attributes.id id
        ]
        (div
            [ onClick (toMsg TitleClicked)
            , css
                [ color (Colors.toCssColor Colors.white)
                , cursor pointer
                ]
            ]
            [ text title ]
            :: wrapBody body isOpen
        )


wrapperCss : Bool -> List Style
wrapperCss isOpen =
    let
        activeStyle =
            if isOpen then
                [ backgroundColor (Colors.toCssColor Colors.black) ]

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


wrapBody : Html msg -> Bool -> List (Html msg)
wrapBody element isOpen =
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



-- UPDATE


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
    , hover [ backgroundColor (Colors.toCssColor Colors.theme.hover) ]
    , active [ backgroundColor (Colors.toCssColor Colors.theme.active) ]
    ]
        ++ activeAttrs


flexListItemStyle : Bool -> List Style
flexListItemStyle isActive =
    listItemStyle isActive ++ [ Css.flexGrow (Css.int 1) ]
