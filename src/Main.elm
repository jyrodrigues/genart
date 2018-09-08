module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events exposing (onKeyPress, onKeyUp)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
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


type alias Transformation =
    State


type alias Model =
    { state : State
    , recording : State
    , recOn : Bool
    , history : List State
    , isShowingNextIteration : Bool
    , dir : String

    -- , dir : Direction
    }


initialState : State
initialState =
    [ D, L, D, L, D, L, D ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initialState [] True [] True ""
      --Other
    , Cmd.none
    )



-- UPDATE


type Msg
    = Add Step
    | Backspace
    | ClearStep
    | ClearSvg
    | Iterate
    | Deiterate
    | ToggleShowNextIteration
    | KeyPress String



-- | KeyPress Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add step ->
            ( { model | recording = model.recording ++ [ step ] }, Cmd.none )

        Backspace ->
            ( { model | recording = dropLast model.recording }, Cmd.none )

        ClearStep ->
            ( { model | recording = [] }, Cmd.none )

        ClearSvg ->
            ( { model | state = initialState }, Cmd.none )

        Iterate ->
            ( { model
                | state =
                    apply model.recording model.state
                , history =
                    model.history ++ [ model.recording ]
              }
            , Cmd.none
            )

        Deiterate ->
            let
                newHistory =
                    dropLast model.history
            in
            ( { model
                | state = rebuildState initialState newHistory
                , history = newHistory
              }
            , Cmd.none
            )

        ToggleShowNextIteration ->
            ( { model | isShowingNextIteration = not model.isShowingNextIteration }, Cmd.none )

        KeyPress dir ->
            let
                _ =
                    Debug.log "direction" dir
            in
            ( processKey model dir, Cmd.none )


dropLast : List a -> List a
dropLast list =
    list
        |> List.reverse
        |> List.drop 1
        |> List.reverse


rebuildState : State -> List Transformation -> State
rebuildState baseState history =
    List.foldl apply baseState history


apply : Transformation -> State -> State
apply transformation baseState =
    let
        rule =
            makeRule transformation
    in
    List.concatMap rule baseState


processKey model dir =
    case dir of
        "r" ->
            { model | recOn = not model.recOn, dir = dir }
        "ArrowLeft" ->
            { model | recording = model.recording ++ [ L ], dir = dir }
        "ArrowRight" ->
            { model | recording = model.recording ++ [ R ], dir = dir }
        "ArrowUp" ->
            { model | recording = model.recording ++ [ D ], dir = dir }
        " " ->
            { model | recording = model.recording ++ [ S ], dir = dir }
        "Backspace" ->
            { model | recording = dropLast model.recording, dir = dir }
        "i" ->
            { model | state = apply model.recording model.state, dir = dir }

        _ ->
            { model | dir = dir }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyUp (Decode.map KeyPress keyDecoder) ]



-- type Direction
--     = Left
--     | Right
--     | Up
--     | Skip
--     | Backspace1
--     | Other
-- -- keyDecoder : Decoder Direction


keyDecoder : Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- -- Decode.map toDirection (Decode.field "key" Decode.string)
-- toDirection : String -> Direction
-- toDirection string =
--     case string of
--         "ArrowLeft" ->
--             Left
--         "ArrowRight" ->
--             Right
--         "ArrowUp" ->
--             Up
--         "Space" ->
--             Skip
--         "Backspace" ->
--             Backspace1
--         _ ->
--             Other
-- VIEW


mystyle : List (Html.Attribute msg)
mystyle =
    [ Html.Attributes.style "width" "100%"
    , Html.Attributes.style "height" "130px"
    , Html.Attributes.style "border" "1px solid black"
    ]


view : Model -> Html Msg
view model =
    div []
        [ div mystyle
            [ button [ onClick <| Add D ] [ text "Add D" ]
            , button [ onClick <| Add L ] [ text "Add L" ]
            , button [ onClick <| Add R ] [ text "Add R" ]
            , button [ onClick Backspace ] [ text "Backspace" ]
            , button [ onClick ClearStep ] [ text "Clear Step" ]
            , button [ onClick ClearSvg ] [ text "Clear Svg" ]
            , button [ onClick Iterate ] [ text "Iterate" ]
            , button [ onClick Deiterate ] [ text "Deiterate" ]
            , button [ onClick ToggleShowNextIteration ] [ text "Toggle" ]
            , text <|
                "Status: "
                    ++ (if model.isShowingNextIteration then
                            "On"

                        else
                            "Off"
                       )
            , text <|
                "  Rec: "
                    ++ (if model.recOn then
                            "On"

                        else
                            "Off"
                       )
            , div []
                [ text (String.fromInt <| List.length model.state)
                , text <| "->" ++ model.dir ++ "<-"
                , text (String.fromInt <| (*) (List.length model.recording) <| List.length model.state)
                ]
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
            [ drawSvg
                (if model.isShowingNextIteration then
                    apply model.recording model.state

                 else
                    model.state
                )
                size
                size
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


size : Float
size =
    1200.0


initialPosition : Float -> Float -> Position
initialPosition w h =
    Position (w / 2) (h / 2)


posToStr : Position -> String
posToStr pos =
    String.fromFloat pos.x ++ " " ++ String.fromFloat pos.y


stateToSvgPath : State -> Float -> Float -> Drawing
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


stateToString : State -> String
stateToString state =
    state
        |> List.map stepToString
        |> String.join " "


stepToString : Step -> String
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


makeRule : Transformation -> Step -> State
makeRule state step =
    case step of
        D ->
            state

        _ ->
            [ step ]

-- [D L D D L D D D D L D D L D D D D D D R D L D L D D D D D D D L]