module LSystem.Draw exposing (drawSvg, drawSvgFixed)

-- Todo: remove Msgs from here. Msgs should live on Update and LSystem.Draw should have its own msgs

import Auxiliary exposing (floatsToSpacedString)
import LSystem.Core exposing (State, Step(..), Transformation, buildState, countSize)
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


drawSvg : State -> Float -> Float -> Float -> Float -> Svg msg
drawSvg state w h wDelta hDelta =
    svg
        [ width "1000"
        , height "700"
        , viewBox <| floatsToSpacedString [ wDelta, hDelta, w + wDelta, h + hDelta ]
        , style "border: 1px dashed black; display: block"
        ]
        [ polyline
            [ Svg.Attributes.points <| .path <| stateToSvgPath state (w / 2) (h / 2)
            , stroke "rgba(0,180,110,0.7)"
            , fill "none"
            ]
            []
        ]


drawSvgFixed : Transformation -> Svg msg
drawSvgFixed transform =
    let
        { minX, maxX, minY, maxY } =
            countSize transform

        w =
            maxX - minX

        h =
            maxY - minY

        margin =
            0.5

        xBegin =
            (*) scale <| -minX + (margin / 2 * w)

        yBegin =
            (*) scale <| -minY + (margin / 2 * h)

        scale =
            10

        fw =
            (1 + margin) * scale * w

        fh =
            (1 + margin) * scale * h
    in
    svg
        [ viewBox <| floatsToSpacedString [ 0, 0, fw, fh ]
        , style "border: 1px dashed black; display: block"
        ]
        [ polyline
            [ Svg.Attributes.points <| .path <| transformToSvgPath transform xBegin yBegin
            , stroke "rgba(0,180,110,0.7)"
            , fill "none"
            ]
            []
        ]


positionToString : Position -> String
positionToString pos =
    String.fromFloat pos.x ++ " " ++ String.fromFloat pos.y


transformToSvgPath : Transformation -> Float -> Float -> Drawing
transformToSvgPath transform x0 y0 =
    List.foldl stepToPath (Drawing (String.fromFloat x0 ++ " " ++ String.fromFloat y0) (Position x0 y0) 0) transform



-- todo: remove state to svg path


stateToSvgPath : State -> Float -> Float -> Drawing
stateToSvgPath state x0 y0 =
    transformToSvgPath (buildState state) x0 y0


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
