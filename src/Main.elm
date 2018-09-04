module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Svg exposing (Svg, polyline, rect, svg)
import Svg.Attributes exposing (height, rx, ry, stroke, viewBox, width, x, y)



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddD ->
            ( { model | state = model.state ++ [ D ] }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddD ] [ text "Add D" ]
        , div [] [ text (stateToString model.state) ]
        , div
            []
            [ drawSvg model.state
            ]
        ]


drawSvg : State -> Svg Msg
drawSvg state =
    svg
        [ width "120", height "120", viewBox "0 0 120 120" ]
        [ polyline
            [ Svg.Attributes.points <| .path <| stateToSvgPath state
            , stroke "black"
            ]
            []
        ]


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Position =
    { x : Int
    , y : Int
    }


type alias Drawing =
    { path : String
    , pos : Position
    , dir : Direction
    }


stateToSvgPath state =
    List.foldr stepToPath (Drawing "0 0, " (Position 0 0) Left) state


stepToPath : Step -> Drawing -> Drawing
stepToPath step drawing =
    let
        newPos =
            Position (drawing.pos.x + 10) (drawing.pos.y + 10)

        newX =
            String.fromInt <| newPos.x + 10

        newY =
            String.fromInt <| newPos.y + 10

        newPath =
            drawing.path ++ newX ++ " " ++ newY ++ ", "
    in
    Drawing newPath newPos drawing.dir



-- AUXILIARY


stateToString state =
    state
        |> List.map stepToChar
        |> List.map String.fromChar
        |> String.join " "


stepToChar step =
    case step of
        D ->
            'D'

        R ->
            'R'

        L ->
            'L'

        S ->
            'S'
