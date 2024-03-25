module ABALA.Core exposing
    ( PathSegment(..)
    , PathSegmentString
    , rotateSegmentTo
    , segmentToString
    , toAbsoluteValue
    , toRelativeValue
    )

import Utils exposing (Position)


{-| <https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths>

    Important:

    Using uppercase letters only because Elm forces us when creating types,
    *BUT* every case represents lowercase counterparts from the
    SVG specification, which represent relative movements!


    Also check:
    - v1: https://www.w3.org/TR/SVG11/paths.html#PathElement
    - v2: https://svgwg.org/svg2-draft/paths.html#PathElement
    - Working draft: https://svgwg.org/specs/paths/#PathElement

-}
type alias PathSegmentString =
    String


type
    PathSegment
    -- Move to delta
    -- m dx dy
    = M Position
      -- Line to delta
      -- l dx dy
    | L Position
      -- Horizontal line to delta
      -- h dx
    | H Float
      -- Vertical line to delta
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


rotateSegmentTo : Position -> PathSegment -> PathSegment
rotateSegmentTo direction segment =
    let
        ( _, tetha ) =
            toPolar direction

        rotate position =
            let
                ( size, alpha ) =
                    toPolar position
            in
            fromPolar ( size, alpha + tetha )
    in
    case segment of
        M position ->
            M (rotate position)

        L position ->
            L (rotate position)

        C position0 position1 position2 ->
            C (rotate position0) (rotate position1) (rotate position2)

        S position0 position1 ->
            S (rotate position0) (rotate position1)

        Q position0 position1 ->
            Q (rotate position0) (rotate position1)

        T position ->
            T (rotate position)

        A position0 float bool0 bool1 position1 ->
            A (rotate position0) float bool0 bool1 (rotate position1)

        _ ->
            segment


{-|

    Note about `"m" ++ String.fromFloat dx ...`:
    There could be a space in between but we should follow W3C SVG Recommendation:

       "Superfluous white space and separators such as commas can be eliminated
        (e.g., "M 100 100 L 200 200" contains unnecessary spaces and could be
        expressed more compactly as "M100 100L200 200")."

    From: https://web.archive.org/web/20200331042341/www.w3.org/TR/SVG/paths.html
    (yes, a link from archive.org)

-}
segmentToString : PathSegment -> PathSegmentString
segmentToString segment =
    case segment of
        M ( dx, dy ) ->
            "m" ++ String.fromFloat dx ++ " " ++ String.fromFloat dy

        L ( dx, dy ) ->
            "l" ++ String.fromFloat dx ++ " " ++ String.fromFloat dy

        H dx ->
            "h" ++ String.fromFloat dx

        V dy ->
            "v" ++ String.fromFloat dy

        Z ->
            "z"

        C ( dx1, dy1 ) ( dx2, dy2 ) ( dx, dy ) ->
            "c"
                ++ (String.fromFloat dx1 ++ " " ++ String.fromFloat dy1)
                ++ (" " ++ String.fromFloat dx2 ++ " " ++ String.fromFloat dy2)
                ++ (" " ++ String.fromFloat dx ++ " " ++ String.fromFloat dy)

        S ( dx2, dy2 ) ( dx, dy ) ->
            "s"
                ++ (String.fromFloat dx2 ++ " " ++ String.fromFloat dy2)
                ++ (" " ++ String.fromFloat dx ++ " " ++ String.fromFloat dy)

        Q ( dx1, dy1 ) ( dx, dy ) ->
            "q"
                ++ (String.fromFloat dx1 ++ " " ++ String.fromFloat dy1)
                ++ (" " ++ String.fromFloat dx ++ " " ++ String.fromFloat dy)

        T ( dx, dy ) ->
            "t" ++ String.fromFloat dx ++ " " ++ String.fromFloat dy

        A ( rx, ry ) xAxisRotation largeArcFlag sweepFlag ( x, y ) ->
            "a"
                ++ (String.fromFloat rx ++ " " ++ String.fromFloat ry)
                ++ (" " ++ String.fromFloat (degrees xAxisRotation))
                ++ (" "
                        ++ (if largeArcFlag then
                                "1"

                            else
                                "0"
                           )
                   )
                ++ (" "
                        ++ (if sweepFlag then
                                "1"

                            else
                                "0"
                           )
                   )
                ++ (" " ++ String.fromFloat x ++ " " ++ String.fromFloat y)


toAbsoluteValue : PathSegmentString -> PathSegmentString
toAbsoluteValue =
    String.toUpper


toRelativeValue : PathSegmentString -> PathSegmentString
toRelativeValue =
    String.toLower
