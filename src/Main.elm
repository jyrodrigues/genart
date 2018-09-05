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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddD ->
            ( { model | state = model.state ++ [ D ] }, Cmd.none )

        AddL ->
            ( { model | state = model.state ++ [ L ] }, Cmd.none )

        AddR ->
            ( { model | state = model.state ++ [ R ] }, Cmd.none )



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
        , div [] [ text (stateToString model.state) ]
        , div
            []
            [ drawSvg model.state
            ]
        ]


drawSvg : State -> Svg Msg
drawSvg state =
    svg
        [ width "120", height "120", viewBox "0 0 60 60" ]
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
    , deg : Int
    }


stateToSvgPath state =
    List.foldl stepToPath (Drawing "0 0" (Position 0 0) 0) state


stepToPath : Step -> Drawing -> Drawing
stepToPath step drawing =
    let
        toRad deg =
            toFloat deg / 360 * 2 * pi

        move pos rad =
            Position (pos.x + cos rad) (pos.y + sin rad)

        newPos =
            move drawing.pos (toRad drawing.deg)

        newX =
            String.fromFloat newPos.x

        newY =
            String.fromFloat newPos.y

        newPath =
            drawing.path ++ ", " ++ newX ++ " " ++ newY
    in
    case step of
        L ->
            { drawing | deg = modBy 360 (drawing.deg + 90) }

        R ->
            { drawing | deg = modBy 360 (drawing.deg - 90) }

        D ->
            Drawing newPath newPos drawing.deg

        _ ->
            drawing



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
