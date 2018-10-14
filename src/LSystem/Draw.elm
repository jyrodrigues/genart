module LSystem.Draw exposing (drawSvg, drawSvgFixed)

import LSystem.Core exposing (State, Step(..), countSize)
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


drawSvg : State -> Float -> Float -> Float -> Float -> Svg Msg
drawSvg state w h wDelta hDelta =
    svg
        [ width "1000"
        , height "700"
        , viewBox <| toSpacedString wDelta hDelta (w + wDelta) (h + hDelta)
        , style "border: 1px dashed black; display: block"
        ]
        [ polyline
            [ Svg.Attributes.points <| .path <| stateToSvgPath state (w / 2) (h / 2)
            , stroke "rgba(0,180,110,0.7)"
            , fill "none"
            ]
            []
        ]


drawSvgFixed : State -> Svg Msg
drawSvgFixed state =
    let
        maxes =
            countSize state

        x0 =
            toFloat maxes.minX

        x1 =
            toFloat maxes.maxX

        y0 =
            toFloat maxes.minY

        y1 =
            toFloat maxes.maxY

        w =
            x1 - x0

        h =
            y1 - y0

        margin =
            0.5

        xBegin =
            (*) scale <| -x0 + (margin / 2 * w)

        yBegin =
            (*) scale <| -y0 + (margin / 2 * h)

        scale =
            10

        fw =
            (1 + margin) * scale * w

        fh =
            (1 + margin) * scale * h
    in
    svg
        [ width "690"
        , height "690"
        , viewBox <| toSpacedString 0 0 fw fh
        , style "border: 1px dashed black; display: block"
        ]
        [ polyline
            [ Svg.Attributes.points <| .path <| stateToSvgPath state xBegin yBegin
            , stroke "rgba(0,180,110,0.7)"
            , fill "none"
            ]
            []
        ]


toSpacedString : Float -> Float -> Float -> Float -> String
toSpacedString a b c d =
    String.join " " <| List.map String.fromFloat [ a, b, c, d ]


positionToString : Position -> String
positionToString pos =
    String.fromFloat pos.x ++ " " ++ String.fromFloat pos.y


stateToSvgPath : State -> Float -> Float -> Drawing
stateToSvgPath state x0 y0 =
    List.foldl stepToPath (Drawing (String.fromFloat x0 ++ " " ++ String.fromFloat y0) (Position x0 y0) 0) state


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
