module LSystem.Draw exposing (drawBlock, drawFixedImage, drawImage)

import Colors exposing (Color)
import LSystem.Core as Core exposing (Block)
import LSystem.Image as Image exposing (Boundaries, Image)
import Svg.Styled exposing (Svg, animate, circle, defs, path, radialGradient, stop, svg)
import Svg.Styled.Attributes
    exposing
        ( attributeName
        , cx
        , cy
        , d
        , dur
        , fill
        , id
        , offset
        , r
        , repeatCount
        , stopColor
        , stroke
        , strokeDasharray
        , strokeLinecap
        , strokeWidth
        , style
        , values
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

        { pathString, nextStepPathString, boundaries, strokeWidth } =
            case image.svgPathAndBoundaries of
                Just svgPathAndBoundaries ->
                    svgPathAndBoundaries

                Nothing ->
                    -- MAIN COMPUTATION: process image and create SVG path.
                    Image.imageToSvgPathString image

        strokeWidthClamped =
            max strokeWidth image.strokeWidth

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
        (gradients strokeColor backgroundColor
            :: path
                [ d pathString

                --, stroke (Colors.toString strokeColor)
                --, strokeWidth (String.fromFloat image.strokeWidth ++ "px")
                , Svg.Styled.Attributes.strokeWidth (String.fromFloat strokeWidthClamped ++ "px")
                , strokeLinecap "square"
                , fill "none"
                , stroke "url(#AnimatedRadialGradient3)"

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
        , radialGradient [ id "AnimatedRadialGradient1" ]
            [ stop [ offset "0%", stopColor (Colors.toString strokeColor) ] []
            , stop [ offset "50%", stopColor (Colors.toString backgroundColor) ] []
            , stop [ offset "100%", stopColor (Colors.toString strokeColor) ] []
            ]
        , radialGradient [ id "AnimatedRadialGradient2" ]
            (stop [ offset "0%", stopColor (Colors.toString backgroundColor) ] []
                :: stop [ stopColor (Colors.toString strokeColor) ]
                    [ animate [ attributeName "offset", values "0%;0%;10%", dur "5s", repeatCount "indefinite" ] [] ]
                --:: animatedStops [ backgroundColor, strokeColor, backgroundColor ]
                :: []
            )
        , radialGradient [ id "AnimatedRadialGradient3" ] (hypnosis strokeColor)
        ]


hypnosis : Color -> List (Svg msg)
hypnosis color =
    let
        colorA =
            Colors.updateValue (.v (Colors.toHsva color) + 0.7) color

        colorB =
            Colors.updateValue (.v (Colors.toHsva color) - 0.5) color

        density =
            7

        duration =
            dur "4s"

        range =
            100 / toFloat (2 * density)

        toString pct =
            String.fromInt (round pct) ++ "%"

        colors =
            List.concat (List.repeat density [ colorA, colorB ])
    in
    stop [ offset "0%", stopColor (Colors.toString colorA) ] []
        :: stop [ stopColor (Colors.toString colorB) ]
            [ animate
                [ attributeName "offset"
                , values ("0%;0%;" ++ toString range)
                , duration
                , repeatCount "indefinite"
                ]
                []
            ]
        :: List.concat (List.indexedMap (colorToStop range duration) colors)
        ++ [ stop [ stopColor (Colors.toString colorA) ]
                [ animate
                    [ attributeName "offset"
                    , values ("100%;" ++ toString (100 + range) ++ ";" ++ toString (100 + range))
                    , duration
                    , repeatCount "indefinite"
                    ]
                    []
                ]
           ]



{--
animatedStops : List Color -> List (Svg msg)
animatedStops colors =
    let
        range =
            100 / toFloat (List.length colors)
    in
    List.indexedMap (colorToStop range) colors
--}


colorToStop : Float -> Svg.Styled.Attribute msg -> Int -> Color -> List (Svg msg)
colorToStop range duration index color =
    let
        start =
            round <| range * toFloat index

        --+ range
        mid =
            start + round range

        end =
            mid + round range

        asString =
            String.fromInt start ++ "%;" ++ String.fromInt mid ++ "%;" ++ String.fromInt end ++ "%"

        laterAsString =
            String.fromInt start ++ "%;" ++ String.fromInt mid ++ "%;" ++ String.fromInt end ++ "%"
    in
    [ stop [ stopColor (Colors.toString color) ]
        [ animate [ attributeName "offset", values asString, duration, repeatCount "indefinite" ] [] ]
    , stop [ stopColor (Colors.toString color) ]
        [ animate [ attributeName "offset", values laterAsString, duration, repeatCount "indefinite" ] [] ]
    ]



{--
    <radialGradient id="myGradient3">
      <stop offset="0%" stop-color="#141">
        <animate attributeName="offset" values="0%;17%" dur="5s" repeatCount="indefinite" />
      </stop>
      <stop offset="0%" stop-color="#9f9">
        <animate attributeName="offset" values="17%;34%" dur="5s" repeatCount="indefinite" />
      </stop>
      <stop offset="0%" stop-color="#141">
        <animate attributeName="offset" values="34%;51%" dur="5s" repeatCount="indefinite" />
      </stop>
      <stop offset="0%" stop-color="#9f9">
        <animate attributeName="offset" values="51%;67%" dur="5s" repeatCount="indefinite" />
      </stop>
      <stop offset="0%" stop-color="#141">
        <animate attributeName="offset" values="67%;84%" dur="5s" repeatCount="indefinite" />
      </stop>
      <stop offset="0%" stop-color="#9f9">
        <animate attributeName="offset" values="84%;100%" dur="5s" repeatCount="indefinite" />
      </stop>
    </radialGradient>
--}
