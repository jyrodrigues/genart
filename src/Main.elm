module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (Svg, polyline, svg)
import Svg.Attributes exposing (fill, height, stroke, style, viewBox, width)



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
    , recording : State
    , recOn : Bool
    }


initialState =
    [ D, L, D, L, D, L, D ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initialState [] False
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddD
    | AddL
    | AddR
    | Backspace
    | ClearStep
    | ClearSvg
    | Iterate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddD ->
            ( { model | recording = model.recording ++ [ D ] }, Cmd.none )

        AddL ->
            ( { model | recording = model.recording ++ [ L ] }, Cmd.none )

        AddR ->
            ( { model | recording = model.recording ++ [ R ] }, Cmd.none )

        Backspace ->
            ( { model
                | recording =
                    model.recording
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
              }
            , Cmd.none
            )

        ClearStep ->
            ( { model | recording = [] }, Cmd.none )

        ClearSvg ->
            ( { model | state = initialState }, Cmd.none )

        Iterate ->
            ( { model | state = apply (makeRule model.recording) model.state }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


mystyle =
    [ Html.Attributes.style "width" "100%"
    , Html.Attributes.style "height" "130px"
    , Html.Attributes.style "border" "1px solid black"
    ]



--[ ( "width", "100%" ), ( "height", "120px" ) ]


view : Model -> Html Msg
view model =
    div []
        [ div mystyle
            [ button [ onClick AddD ] [ text "Add D" ]
            , button [ onClick AddL ] [ text "Add L" ]
            , button [ onClick AddR ] [ text "Add R" ]
            , button [ onClick Backspace ] [ text "Backspace" ]
            , button [ onClick ClearStep ] [ text "Clear Step" ]
            , button [ onClick ClearSvg ] [ text "Clear Svg" ]
            , button [ onClick Iterate ] [ text "Iterate" ]
            , div [] [ text (String.fromInt <| List.length model.state) ]
            , div [ Html.Attributes.style "display" "inline-block" ] [ text <| stateToString model.recording ]
            , div
                [ Html.Attributes.style "display" "block "
                ]
                [ drawSvg model.recording 120 80
                ]
            ]
        , div
            [ Html.Attributes.style "display" "block "
            , Html.Attributes.style "border" "1px solid black"
            ]
            [ drawSvg model.state size size
            ]
        ]



-- DRAWING


drawSvg : State -> Float -> Float -> Svg Msg
drawSvg state w h =
    svg
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox <| "0 0 " ++ String.fromFloat w ++ " " ++ String.fromFloat h
        , style "border: 1px dashed black; display: block"
        ]
        [ polyline
            [ Svg.Attributes.points <| .path <| stateToSvgPath state w h
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


size =
    1200.0


initialPosition w h =
    Position (w / 2) (h / 2)


posToStr pos =
    String.fromFloat pos.x ++ " " ++ String.fromFloat pos.y


stateToSvgPath state w h =
    List.foldl stepToPath (Drawing (posToStr <| initialPosition w h) (initialPosition w h) 0) state


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
            { drawing | deg = drawing.deg - 90 }

        R ->
            { drawing | deg = drawing.deg + 90 }

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


makeRule state step =
    case step of
        D ->
            state

        _ ->
            [ step ]


apply rule state =
    List.concatMap rule state
