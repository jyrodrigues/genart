module LSystem.Draw exposing
    ( drawImage
    , drawSvg
    , drawSvgFixed
    , drawSvgFixedWithColor
    , image
    , withScale
    , withStrokeColor
    , withTranslation
    , withTurnAngle
    )

import Colors exposing (..)
import LSystem.Core exposing (Block, Composition, Step(..), digestComposition, imageBoundaries)
import ListExtra exposing (floatsToSpacedString, pairExec, pairMap)
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


type alias Angle =
    Float


type alias Scale =
    Float


type Translation
    = Translation Float Float


type Image
    = Image Composition Angle Color Scale Translation


image : Composition -> Image
image composition =
    Image composition 90 offWhite 1 (Translation 0 0)


withTurnAngle : Angle -> Image -> Image
withTurnAngle angle (Image t _ c s xy) =
    Image t angle c s xy


withStrokeColor : Color -> Image -> Image
withStrokeColor color (Image t a _ s xy) =
    Image t a color s xy


withScale : Scale -> Image -> Image
withScale scale (Image t a c _ xy) =
    Image t a c scale xy


withTranslation : ( Float, Float ) -> Image -> Image
withTranslation ( x, y ) (Image t a c s _) =
    Image t a c s (Translation x y)


{-| About vecTranslateToImgCenter:

The drawing's math coordinate system is UPxRIGHT while
SVG viewbox coordinate system is DOWNxRIGHT.

So the vector to translate viewboxe's (0,0) into image's
center for x-axis is the same as the middle point of the image
but is inverted for y-axis.

-}
drawImage : Image -> Svg msg
drawImage (Image composition angle color scale (Translation x y)) =
    let
        _ =
            Debug.log "\n\n\ndrawImage" (Image composition angle color scale (Translation x y))

        { topRight, bottomLeft } =
            imageBoundaries angle composition

        ( right, top ) =
            Debug.log "(r,t)" topRight

        ( left, bottom ) =
            Debug.log "(l,b)" bottomLeft

        vecTranslateOriginToDrawingCenter =
            ( (right + left) / 2 * 10
            , -(top + bottom) / 2 * 10
            )

        ( width, height ) =
            Debug.log "(w,h)" (topRight |> pairExec (-) bottomLeft)

        margin =
            0.5

        scaledWidth =
            (1 + margin) * 10 * max 2 width

        scaledHeight =
            (1 + margin) * 10 * max 2 height

        vecTranslateOriginToViewportCenter =
            ( -scaledWidth / 2, -scaledHeight / 2 )

        vecTranslate =
            vecTranslateOriginToDrawingCenter |> pairExec (+) vecTranslateOriginToViewportCenter

        drawing =
            transformToSvgPath (digestComposition composition) 0 0 angle
    in
    svg
        [ viewBox <|
            Debug.log "viewport" <|
                floatsToSpacedString
                    [ Tuple.first vecTranslate
                    , Tuple.second vecTranslate
                    , scaledWidth
                    , scaledHeight
                    ]
        , style <|
            "display: block; "
                ++ "height: 100%; "
                ++ "width: 100%; "
                ++ "transform: "
                -- As stated here (https://stackoverflow.com/a/10765771),
                -- multiple transform functions are applied from right
                -- to left. So `scale` should come last (to be applied first)
                -- to prevent scaled translation (moving 2px with mouse and
                -- seeing a 200px move in the image).
                ++ ("translate("
                        ++ String.fromFloat x
                        ++ "px, "
                        ++ String.fromFloat y
                        ++ "px)"
                   )
                ++ ("scale(" ++ String.fromFloat scale ++ ")")
        ]
        [ originPoint 0 0
        , nextLine drawing
        , polyline
            [ points <| .path <| drawing
            , stroke (toString color)
            , fill "none"
            ]
            []
        ]


drawSvg : Composition -> Float -> Float -> Float -> Float -> Svg msg
drawSvg state w h wDelta hDelta =
    svg
        [ width "1000"
        , height "700"
        , viewBox <| floatsToSpacedString [ wDelta, hDelta, w + wDelta, h + hDelta ]
        , style "border: 1px dashed black; display: block"
        ]
        [ polyline
            [ points <|
                .path <|
                    transformToSvgPath (digestComposition state)
                        (w / 2)
                        (h / 2)
                        90
            , stroke "rgba(0,180,110,0.7)"
            , fill "none"
            ]
            []
        ]


drawSvgFixed : Composition -> Svg msg
drawSvgFixed composition =
    drawSvgFixedWithColor 90 defaultGreen composition


drawSvgFixedWithColor : Float -> Color -> Composition -> Svg msg
drawSvgFixedWithColor angle color composition =
    svg [] []



{--
        w =
            maxX - minX

        h =
            maxY - minY

        margin =
            0.5

        xBegin =
            (*) 10 <| -minX + (margin / 2 * w)

        yBegin =
            (*) 10 <| -minY + (margin / 2 * h)

        fw =
            (1 + margin) * 10 * w

        fh =
            (1 + margin) * 10 * h

        drawing =
            transformToSvgPath transform xBegin yBegin 90

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
--}


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


transformToSvgPath : Block -> Float -> Float -> Float -> Drawing
transformToSvgPath transform x0 y0 turn =
    let
        initialPos =
            Position x0 y0
    in
    List.foldl
        (addStepToDrawing turn)
        (Drawing (positionToString initialPos) initialPos 0)
        transform


movePoint pos rad =
    Position (pos.x + cos rad * 10) (pos.y + sin rad * 10)


addStepToDrawing : Float -> Step -> Drawing -> Drawing
addStepToDrawing turn step drawing =
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
