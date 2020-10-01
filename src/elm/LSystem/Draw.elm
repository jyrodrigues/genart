module LSystem.Draw exposing (drawBlock, drawFixedImage, drawImage)

import Colors exposing (Color)
import LSystem.Core as Core exposing (Block)
import LSystem.Image as Image exposing (Boundaries, Image)
import Svg.Styled exposing (Svg, circle, defs, path, radialGradient, stop, svg)
import Svg.Styled.Attributes
    exposing
        ( cx
        , cy
        , d
        , fill
        , id
        , offset
        , r
        , stopColor
        , stroke
        , strokeDasharray
        , strokeLinecap
        , strokeWidth
        , style
        , viewBox
        )
import Svg.Styled.Lazy as Lazy
import Utils exposing (Position, floatsToSpacedString, pairExec, pairMap)


type alias Id =
    String



-- DRAW


drawBlocks : Image -> List (Svg msg)
drawBlocks image =
    Image.blocksToImages image
        |> List.map (drawImage Nothing True)


drawBlock : Color -> Color -> Float -> Image.PathCurve -> Block -> Svg msg
drawBlock backgroundColor strokeColor turnAngle curve block =
    drawImage Nothing True <|
        { composition = Core.fromList [ block ]
        , backgroundColor = backgroundColor
        , turnAngle = turnAngle
        , strokeColor = strokeColor
        , curve = curve
        , scale = 1
        , translate = ( 0, 0 )
        , strokeWidth = 1
        , svgPathAndBoundaries = Nothing
        }


drawFixedImage : Image -> Svg msg
drawFixedImage image =
    image
        |> Image.withTranslate ( 0, 0 )
        |> Image.withScale 1
        |> drawImage Nothing False


drawImage : Maybe Id -> Bool -> Image -> Svg msg
drawImage =
    Lazy.lazy3 drawImageEager


{-| TODO remove this last Bool (at least use a custom type).
-}
drawImageEager : Maybe Id -> Bool -> Image -> Svg msg
drawImageEager maybeId drawOriginAndNextStep image =
    let
        { backgroundColor, strokeColor, translate, scale } =
            image

        ( x, y ) =
            translate

        -- MAIN COMPUTATION: process image and create SVG path.
        ( mainPathString, nextStepPathString, boundaries ) =
            case image.svgPathAndBoundaries of
                Just svgPathAndBoundaries ->
                    svgPathAndBoundaries

                Nothing ->
                    Image.imageToSvgPathString image

        ( ( viewBoxMinX, viewBoxMinY ), ( viewBoxWidth, viewBoxHeight ) ) =
            calculateViewBox boundaries

        optionalAttrs =
            List.filterMap identity
                [ Maybe.map id maybeId
                ]

        {--Used to draw a circle in the `centerOfMass`--
        centerOfMass =
            Utils.pairMap (\v -> v / boundaries.counter) boundaries.centerOfMass
        --}
        originAndNextStep =
            if drawOriginAndNextStep then
                [ originPoint 0 0
                , path [ d nextStepPathString, stroke (Colors.toString Colors.red_), strokeDasharray "1" ] []
                ]

            else
                []

        {--Center of mass--
                [ circle
                    [ cx (Tuple.first centerOfMass |> String.fromFloat)
                    , cy (Tuple.second centerOfMass |> String.fromFloat)
                    , r "0.1"
                    , fill (Colors.toString Colors.offWhite)
                    ]
                    []
                ]

        --}
    in
    svg
        ([ viewBox <|
            floatsToSpacedString
                -- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/viewBox
                [ viewBoxMinX
                , viewBoxMinY
                , viewBoxWidth
                , viewBoxHeight
                ]
         , style <|
            "display: block; "
                ++ "height: 100%; "
                ++ "width: 100%; "
                ++ ("background-color: " ++ Colors.toString backgroundColor ++ "; ")
                ++ "transform: "
                -- As stated here (https://stackoverflow.com/a/10765771),
                -- multiple transform functions are applied from right
                -- to left. So `scale` should come last (to be applied first)
                -- to prevent scaled translation (moving 2px with mouse and
                -- seeing a 200px move in the image).
                ++ ("translate(" ++ String.fromFloat x ++ "px, " ++ String.fromFloat y ++ "px)")
                ++ ("scale(" ++ String.fromFloat scale ++ ")")
         ]
            ++ optionalAttrs
        )
        (path
            [ d mainPathString
            , stroke (Colors.toString strokeColor)
            , strokeWidth (String.fromFloat image.strokeWidth ++ "px")
            , strokeLinecap "square"
            , fill "none"

            --, stroke "url(#RadialGradient1)"
            --, fill "url(#RadialGradient2)"
            ]
            []
            :: originAndNextStep
        )



-- ORIGIN POINT SVG


originPoint : Float -> Float -> Svg msg
originPoint x y =
    circle
        [ cx <| String.fromFloat x
        , cy <| String.fromFloat y
        , r "1"
        , fill (Colors.toString Colors.offWhite)
        ]
        []



-- VIEWBOX


calculateViewBox : Boundaries -> ( Position, Position )
calculateViewBox { leftTop, rightBottom, centerOfMass, counter } =
    let
        vecTranslateOriginToDrawingCenter =
            pairMap (\v -> v / counter) centerOfMass

        {--TODO: This function could recieve an option to choose between center of mass or boundaries to position--
            rightBottom
                -- Sum boundaries and get the mean for both axis.
                -- Essentially this compensates positive/negative values
                -- since leftTop <= 0 and rightBottom >= 0, and thus
                -- it gives us the vector from the origin to the drawing center.
                |> pairExec (+) leftTop
                |> pairMap (\value -> value / 2)

        --}
        -- This will be the viewport size (its width and height)
        scaledSize =
            rightBottom
                -- Get drawing width and height
                |> pairExec (-) leftTop
                -- Make sure both are at least `20` (this factor comes from `nextPositionDelta` (2 * 10))
                |> pairMap (max 20)
                -- Scale with 50% of margin (somehow this is a magic number, I think we should be able to change this
                -- value without moving the image from the center
                |> pairMap ((*) 1.6)

        vecTranslateOriginToViewportCenter =
            -- Move origin by half the viewport size in the oposite direction, centralizing the drawing.
            scaledSize |> pairMap ((*) (-1 / 2))

        vecTranslate =
            vecTranslateOriginToDrawingCenter |> pairExec (+) vecTranslateOriginToViewportCenter
    in
    ( vecTranslate, scaledSize )



-- STYLE
-- EFFETCS
{--
    Example with offset center:

      <radialGradient id="RadialGradient2" cx="0.25" cy="0.25" r="0.25">
        <stop offset="0%" stop-color="red"/>
        <stop offset="100%" stop-color="blue"/>
      </radialGradient>
--}


gradients : Color -> Color -> Svg msg
gradients strokeColor backgroundColor =
    defs []
        [ radialGradient [ id "RadialGradient1" ]
            [ stop [ offset "30%", stopColor (Colors.toString backgroundColor) ] []
            , stop [ offset "100%", stopColor (Colors.toString strokeColor) ] []
            ]
        , radialGradient [ id "RadialGradient2" ]
            [ stop [ offset "0%", stopColor (Colors.toString strokeColor) ] []
            , stop [ offset "100%", stopColor (Colors.toString backgroundColor) ] []
            ]
        ]
