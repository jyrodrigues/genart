module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (Svg, polyline, svg)
import Svg.Attributes exposing (fill, height, stroke, viewBox, width)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Step
    = D
    | R
    | L
    | S


type alias State =
    List Step


type alias Model =
    { state : State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddD
    | AddL
    | AddR
    | Iterate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddD ->
            ( { model | state = model.state ++ [ D ] }, Cmd.none )

        AddL ->
            ( { model | state = model.state ++ [ L ] }, Cmd.none )

        AddR ->
            ( { model | state = model.state ++ [ R ] }, Cmd.none )

        Iterate ->
            ( { model | state = apply rule1 model.state }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddD ] [ text "Add D" ]
        , button [ onClick AddL ] [ text "Add L" ]
        , button [ onClick AddR ] [ text "Add R" ]
        , button [ onClick Iterate ] [ text "Iterate" ]
        , div [] [ text (String.fromInt <| List.length model.state) ]
        , div
            []
            [ drawSvg model.state
            ]
        ]



-- DRAWING


drawSvg : State -> Svg Msg
drawSvg state =
    svg
        [ width (String.fromFloat size)
        , height (String.fromFloat size)
        , viewBox <| "0 0 " ++ String.fromFloat (size / scale) ++ " " ++ String.fromFloat (size / scale)
        ]
        [ polyline
            [ Svg.Attributes.points <| .path <| stateToSvgPath state
            , stroke "black"
            , fill "none"
            ]
            []
        ]


type alias Position =
    { x : Float
    , y : Float
    }


type alias Drawing =
    { path : String
    , pos : Position
    , deg : Float
    }


scale =
    1.0


size =
    120.0


idp =
    (size / scale) / 2


initialPosition =
    Position idp idp


posToStr pos =
    String.fromFloat pos.x ++ " " ++ String.fromFloat pos.y


stateToSvgPath state =
    List.foldl stepToPath (Drawing (posToStr initialPosition) initialPosition 0) state


stepToPath : Step -> Drawing -> Drawing
stepToPath step drawing =
    let
        move pos rad =
            Position (pos.x + cos rad * 10) (pos.y + sin rad * 10)

        newPos =
            move drawing.pos (degrees drawing.deg)

        newPath =
            drawing.path ++ ", " ++ String.fromFloat newPos.x ++ " " ++ String.fromFloat newPos.y
    in
    case step of
        L ->
            { drawing | deg = drawing.deg + 90 }

        R ->
            { drawing | deg = drawing.deg - 90 }

        D ->
            Drawing newPath newPos drawing.deg

        _ ->
            drawing



-- AUXILIARY


stateToString state =
    state
        |> List.map stepToString
        |> String.join " "


stepToString step =
    case step of
        D ->
            "D"

        R ->
            "R"

        L ->
            "L"

        S ->
            "S"


rule1 step =
    case step of
        D ->
            [ D, D, L, D, L, D ]

        _ ->
            [ step ]


apply rule state =
    List.concatMap rule state
