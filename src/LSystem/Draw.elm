module LSystem.Draw exposing
    ( drawImage
    , image
    , withScale
    , withStrokeColor
    , withTranslation
    , withTurnAngle
    )

import Colors exposing (Color)
import LSystem.Core exposing (Block, Composition, Step(..), digestComposition, imageBoundaries)
import ListExtra exposing (floatsToSpacedString, pairExec, pairMap)
import Svg.Styled exposing (Svg, circle, line, polyline, svg)
import Svg.Styled.Attributes
    exposing
        ( cx
        , cy
        , fill
        , points
        , r
        , stroke
        , strokeDasharray
        , style
        , viewBox
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
    Image composition 90 Colors.offWhite 1 (Translation 0 0)


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
        { topRight, bottomLeft } =
            imageBoundaries angle composition

        vecTranslateOriginToDrawingCenter =
            topRight
                -- Sum boundaries and get the mean for both axis
                |> pairExec (+) bottomLeft
                |> pairMap (\value -> value / 2)
                -- Scale both by a factor of 10 (this value is arbitrary, probably should live in a variable)
                |> pairMap ((*) 10)
                -- Check the comment above to understand why we invert only the y-axis
                |> pairExec (*) ( 1, -1 )

        scaledSize =
            topRight
                -- Get width and height
                |> pairExec (-) bottomLeft
                -- Make sure both are at least `2`
                |> pairMap (max 2)
                -- Scale both by a factor of 10 (this value is arbitrary, probably should live in a variable)
                |> pairMap ((*) 10)
                -- Scale with 50% of margin (somehow this is a magic number, I think we should be able to change this
                -- value without moving the image from the center
                |> pairMap ((*) 1.6)

        vecTranslateOriginToViewportCenter =
            -- Move origin by half the viewport size in the oposite direction, centralizing the drawing.
            scaledSize |> pairMap ((*) (-1 / 2))

        vecTranslate =
            vecTranslateOriginToDrawingCenter |> pairExec (+) vecTranslateOriginToViewportCenter

        drawing =
            transformToSvgPath (digestComposition composition) 0 0 angle
    in
    svg
        [ viewBox <|
            floatsToSpacedString
                [ Tuple.first vecTranslate
                , Tuple.second vecTranslate
                , Tuple.first scaledSize
                , Tuple.second scaledSize
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
            , stroke (Colors.toString color)
            , fill "none"
            ]
            []
        ]



-- TODO Refactor this tail of functions


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


movePoint : Position -> Float -> Position
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
