module LSystem.Draw exposing (drawSvg, drawSvgFixed, drawSvgFixedWithColor)

-- Todo: remove Msgs from here. Msgs should live on Update and LSystem.Draw should have its own msgs

import Auxiliary exposing (floatsToSpacedString)
import Colors exposing (..)
import LSystem.Core exposing (State, Step(..), Transformation, buildState, getSvgBorders)
import Svg.Styled exposing (Svg, circle, line, polyline, svg)
import Svg.Styled.Attributes
    exposing
        ( cx
        , cy
        , fill
        , height
        , points
        , r
        , stroke
        , strokeDasharray
        , style
        , viewBox
        , width
        , x1
        , x2
        , y1
        , y2
        )


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
            [ points <| .path <| transformToSvgPath (buildState state) (w / 2) (h / 2)
            , stroke "rgba(0,180,110,0.7)"
            , fill "none"
            ]
            []
        ]


drawSvgFixed : Transformation -> Svg msg
drawSvgFixed transform =
    drawSvgFixedWithColor defaultGreen transform


drawSvgFixedWithColor : Color -> Transformation -> Svg msg
drawSvgFixedWithColor color transform =
    let
        { minX, maxX, minY, maxY } =
            getSvgBorders transform

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

        fw =
            (1 + margin) * scale * w

        fh =
            (1 + margin) * scale * h

        drawing =
            transformToSvgPath transform xBegin yBegin

        --        _ =
        --            Debug.log "minX, maxX, minY, maxY" [ minX, maxX, minY, maxY ]
    in
    svg
        [ viewBox <| floatsToSpacedString [ 0, 0, fw, fh ]
        , style "border: 1px dashed black; display: block; height: 100%; width: 100%"
        ]
        [ originPoint xBegin yBegin
        , nextLine drawing
        , polyline
            [ points <| .path <| drawing
            , stroke (toString color)
            , fill "none"
            ]
            []
        ]


originPoint : Float -> Float -> Svg msg
originPoint x y =
    circle
        [ cx <| String.fromFloat x
        , cy <| String.fromFloat y
        , r "1"
        , fill "white"
        ]
        []


nextLine : Drawing -> Svg msg
nextLine drawing =
    let
        { pos, deg } =
            drawing

        newPos =
            movePoint pos (degrees deg)
    in
    line
        [ x1 <| String.fromFloat pos.x
        , y1 <| String.fromFloat pos.y
        , x2 <| String.fromFloat newPos.x
        , y2 <| String.fromFloat newPos.y
        , stroke "rgba(255, 0, 0, 0.5)"
        , strokeDasharray "1"
        ]
        []


positionToString : Position -> String
positionToString pos =
    String.fromFloat pos.x ++ " " ++ String.fromFloat pos.y


transformToSvgPath : Transformation -> Float -> Float -> Drawing
transformToSvgPath transform x0 y0 =
    let
        initialPos =
            Position x0 y0
    in
    List.foldl
        foldStepToDrawing
        (Drawing (positionToString initialPos) initialPos 0)
        transform


scale =
    10


turn =
    90


movePoint pos rad =
    Position (pos.x + cos rad * scale) (pos.y + sin rad * scale)


foldStepToDrawing : Step -> Drawing -> Drawing
foldStepToDrawing step drawing =
    let
        newPos =
            movePoint drawing.pos (degrees drawing.deg)

        newPath =
            drawing.path ++ ", " ++ positionToString newPos
    in
    case step of
        L ->
            { drawing | deg = drawing.deg - turn }

        R ->
            { drawing | deg = drawing.deg + turn }

        D ->
            Drawing newPath newPos drawing.deg

        _ ->
            drawing
