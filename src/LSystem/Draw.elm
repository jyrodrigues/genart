module LSystem.Draw exposing
    ( drawImage
    , drawImageEssentials
    , image
    , withBackgroundColor
    , withId
    , withOnClick
    , withScale
    , withStrokeColor
    , withTranslation
    , withTurnAngle
    )

import Colors exposing (Color)
import ImageEssentials exposing (ImageEssentials)
import LSystem.Core as Core exposing (Composition, Step(..), digestComposition, imageBoundaries)
import ListExtra exposing (floatsToSpacedString, pairExec, pairMap)
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
import Svg.Styled.Events exposing (onClick)


type alias Angle =
    Float


type alias Scale =
    Float


type alias Id =
    String


type Translation
    = Translation Float Float



--                      StrokeColor BackgroundColor
--                              |     |


type Image msg
    = Image Composition Angle Color Color Scale Translation (Maybe Id) (Maybe msg)


image : Composition -> Image msg
image composition =
    Image composition 90 Colors.offWhite Colors.gray 1 (Translation 0 0) Nothing Nothing


withTurnAngle : Angle -> Image msg -> Image msg
withTurnAngle angle (Image t _ sc bc s xy id mm) =
    Image t angle sc bc s xy id mm


withStrokeColor : Color -> Image msg -> Image msg
withStrokeColor color (Image t a _ bc s xy id mm) =
    Image t a color bc s xy id mm


withBackgroundColor : Color -> Image msg -> Image msg
withBackgroundColor color (Image t a sc _ s xy id mm) =
    Image t a sc color s xy id mm


withScale : Scale -> Image msg -> Image msg
withScale scale (Image t a sc bc _ xy id mm) =
    Image t a sc bc scale xy id mm


withTranslation : ( Float, Float ) -> Image msg -> Image msg
withTranslation ( x, y ) (Image t a sc bc s _ id mm) =
    Image t a sc bc s (Translation x y) id mm


withId : Id -> Image msg -> Image msg
withId id (Image t a sc bc s xy _ mm) =
    Image t a sc bc s xy (Just id) mm


withOnClick : msg -> Image msg -> Image msg
withOnClick msg (Image t a sc bc s xy id _) =
    Image t a sc bc s xy id (Just msg)


{-| TODO remove this function after refactor this module to be based on ImageEssentials
there will be only `drawImage`
-}
drawImage : Image msg -> Svg msg
drawImage (Image composition turnAngle strokeColor backgroundColor scale (Translation x y) maybeId maybeMsg) =
    let
        imageEssentials =
            { composition = composition
            , turnAngle = turnAngle
            , backgroundColor = backgroundColor
            , strokeColor = strokeColor
            , strokeWidth = 1
            , translate = ( x, y )
            , scale = scale
            }
    in
    drawImageEssentials imageEssentials maybeId maybeMsg True


{-| About vecTranslateOriginToDrawingCenter:

The drawing's math coordinate system is UPxRIGHT while
SVG viewbox coordinate system is DOWNxRIGHT.

So the vector to translate viewboxe's (0,0) into image's
center for x-axis is the same as the middle point of the image
but is inverted for y-axis.

-}
drawImageEssentials : ImageEssentials -> Maybe Id -> Maybe msg -> Bool -> Svg msg
drawImageEssentials essentials maybeId maybeMsg drawOriginAndNextStep =
    -- TODO remove this last Bool (at least use a custom type).
    let
        { composition, turnAngle, backgroundColor, strokeColor, translate, scale } =
            essentials

        ( x, y ) =
            translate

        { topRight, bottomLeft } =
            {--This function call takes a lot of time/resources/cpu and it's one of the main
                reasons for frame drops (low FPS) when composition is too large.

                What does it means to be too large?

                TODO Memoize this function
            --}
            imageBoundaries turnAngle composition

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

        maybeIdAttr =
            case maybeId of
                Just id_ ->
                    [ id id_ ]

                Nothing ->
                    []

        maybeOnClick =
            case maybeMsg of
                Just msg ->
                    [ onClick msg ]

                Nothing ->
                    []

        ( mainPathString, nextStepPathString ) =
            imageToSvgPathString essentials

        originAndNextStep =
            if drawOriginAndNextStep then
                [ originPoint 0 0
                , path [ d nextStepPathString, stroke (Colors.toString Colors.red_), strokeDasharray "1" ] []
                ]

            else
                []
    in
    svg
        ([ viewBox <|
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
                ++ "background-color: "
                ++ Colors.toString backgroundColor
                ++ "; "
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
            ++ maybeIdAttr
            ++ maybeOnClick
        )
        (path
            [ d mainPathString
            , stroke (Colors.toString strokeColor)
            , strokeWidth (String.fromFloat essentials.strokeWidth ++ "px")
            , strokeLinecap "square"
            , fill "none"

            --, stroke "url(#RadialGradient1)"
            --, fill "url(#RadialGradient2)"
            ]
            []
            :: originAndNextStep
        )



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



-- SVG
-- PATH
{--This function call takes a lot of time/resources/cpu and it's one of the main
    reasons for frame drops (low FPS) when composition is too large.

    What does it means to be too large?

    TODO Memoize this function
--}


imageToSvgPathString : ImageEssentials -> ( String, String )
imageToSvgPathString { composition, turnAngle } =
    let
        ( finalAngle, pathString, ( lastX, lastY ) ) =
            digestComposition composition
                |> List.foldl (accumulateStepsIntoSegments turnAngle) ( 0, "", ( 0, 0 ) )
    in
    -- Main path
    ( "M 0 0 " ++ pathString
      -- Next step path
    , "M "
        ++ String.fromFloat lastX
        ++ " "
        ++ String.fromFloat lastY
        ++ " "
        ++ segmentToString (L (nextPositionDelta finalAngle))
    )


{-|

    For performance reasons we have to iterate over the digested composition
    as few times as possible, ideally just one. That's why we aren't breaking
    up this function (as it once was: one step to get a list of PathSegments
    and another to create a string from those)

-}
accumulateStepsIntoSegments : Float -> Step -> ( Float, String, Position ) -> ( Float, String, Position )
accumulateStepsIntoSegments turnAngleSize step ( angle, pathString, position ) =
    let
        nextPosition =
            nextPositionDelta angle
    in
    case step of
        Core.D ->
            ( angle
            , pathString ++ " " ++ segmentToString (L nextPosition)
            , pairExec (+) nextPosition position
            )

        Core.L ->
            ( angle - turnAngleSize
            , pathString
            , position
            )

        Core.R ->
            ( angle + turnAngleSize
            , pathString
            , position
            )

        Core.S ->
            ( angle
            , pathString ++ " " ++ segmentToString (M nextPosition)
            , pairExec (+) nextPosition position
            )


{-| TODO This `10` value here is scaling the drawing. It's probably related to the viewbox size. Extract it.
-}
nextPositionDelta : Float -> Position
nextPositionDelta angle =
    ( cos (degrees angle) * 10, sin (degrees angle) * 10 )


{-| <https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths>

    Important:

    Using uppercase letters only because Elm forces us when creating types,
    *BUT* every case (but `M`) represents lowercase counterparts from the
    SVG specification!


    Also check:
    - v1: https://www.w3.org/TR/SVG11/paths.html#PathElement
    - v2: https://svgwg.org/svg2-draft/paths.html#PathElement
    - Working draft: https://svgwg.org/specs/paths/#PathElement

-}
type
    PathSegment
    -- Move to
    -- m dx dy
    = M Position
      -- Line to
      -- l dx dy
    | L Position
      -- Horizontal line to
      -- h dx
    | H Float
      -- Vertical line to
      -- v dy
    | V Float
      -- Close path ("from A to Z")
      -- z
    | Z
      -- Cubic bezier curve: `C controlPoint1 controlPoint2 destination`
      -- c dx1 dy1, dx2 dy2, dx dy
    | C Position Position Position
      -- Cubic bezier curve reflecting last curve's last control point
      -- s dx2 dy2, dx dy
    | S Position Position
      -- Quadratic bezier curve: `Q controlPoint destination`
      -- q dx1 dy1, dx dy
    | Q Position Position
      -- Quadratic bezier curve reflecting last curve's last control point
      -- t dx dy
    | T Position
      -- Arc curve
      -- a rx ry x-axis-rotation large-arc-flag sweep-flag dx dy
    | A Position Float Bool Bool Position


type alias PathSegmentString =
    String


type alias Position =
    ( Float, Float )


segmentToString : PathSegment -> PathSegmentString
segmentToString segment =
    case segment of
        M ( dx, dy ) ->
            "m " ++ String.fromFloat dx ++ " " ++ String.fromFloat dy

        L ( dx, dy ) ->
            "l " ++ String.fromFloat dx ++ " " ++ String.fromFloat dy

        -- TODO \/
        H dx ->
            ""

        V dy ->
            ""

        Z ->
            ""

        C ( dx1, dy1 ) ( dx2, dy2 ) ( dx, dy ) ->
            ""

        S ( dx2, dy2 ) ( dx, dy ) ->
            ""

        Q ( dx1, dy1 ) ( dx, dy ) ->
            ""

        T ( dx, dy ) ->
            ""

        A ( rx, ry ) xAxisRotation largeArcFlag sweepFlag ( x, y ) ->
            ""


originPoint : Float -> Float -> Svg msg
originPoint x y =
    circle
        [ cx <| String.fromFloat x
        , cy <| String.fromFloat y
        , r "1"
        , fill (Colors.toString Colors.offWhite)
        ]
        []
