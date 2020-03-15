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
import LSystem.Core exposing (Block, Composition, Step(..), digestComposition, imageBoundaries)
import ListExtra exposing (floatsToSpacedString, pairExec, pairMap)
import Svg.Styled exposing (Svg, circle, defs, line, polyline, radialGradient, stop, svg)
import Svg.Styled.Attributes
    exposing
        ( cx
        , cy
        , fill
        , id
        , offset
        , points
        , r
        , stopColor
        , stroke
        , strokeDasharray
        , strokeWidth
        , style
        , viewBox
        , x1
        , x2
        , y1
        , y2
        )
import Svg.Styled.Events exposing (onClick)


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

        drawing =
            {--This function call takes a lot of time/resources/cpu and it's one of the main
                reasons for frame drops (low FPS) when composition is too large.

                What does it means to be too large?

                TODO Memoize this function
            --}
            transformToSvgPath (digestComposition composition) 0 0 turnAngle

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

        originAndNextStep =
            if drawOriginAndNextStep then
                [ originPoint 0 0
                , nextLine drawing
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
        (polyline
            [ points <| .path <| drawing
            , stroke (Colors.toString strokeColor)
            , strokeWidth (String.fromFloat essentials.strokeWidth ++ "px")
            , fill "none"

            --, stroke "url(#RadialGradient1)"
            --, fill "url(#RadialGradient2)"
            ]
            []
            --, gradients color backgroundColor
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



{--This `10` value here is scaling the drawing. It's probable related to the viewbox size.
    TODO Should extract it.
--}


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
