module Draw exposing (drawSvg)

import LSystem exposing (State, Step(..))
import Msgs exposing (Msg)
import Svg exposing (Svg, polyline, svg)
import Svg.Attributes exposing (fill, height, stroke, style, viewBox, width)


type alias Position =
    { x : Float
    , y : Float
    }


type alias Drawing =
    { path : String
    , pos : Position
    , deg : Float
    }


drawSvg : State -> Float -> Float -> Svg Msg
drawSvg state w h =
    svg
        [ width "1000"
        , height "800"
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


initialPosition : Float -> Float -> Position
initialPosition w h =
    Position (w / 2) (h / 2)


positionToString : Position -> String
positionToString pos =
    String.fromFloat pos.x ++ " " ++ String.fromFloat pos.y


stateToSvgPath : State -> Float -> Float -> Drawing
stateToSvgPath state w h =
    List.foldl stepToPath (Drawing (positionToString <| initialPosition w h) (initialPosition w h) 0) state


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
