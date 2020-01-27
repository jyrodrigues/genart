module Icons exposing
    ( eye
    , pen
    , toSvg
    , trash
    , withColor
    , withConditionalColor
    , withOnClick
    , withSize
    )

import Colors exposing (Color, toString)
import Css exposing (bottom, display, inlineBlock, position, px, relative)
import Html.Styled exposing (div)
import Html.Styled.Attributes exposing (css)
import Svg.Styled exposing (Svg, path, svg)
import Svg.Styled.Attributes exposing (d, fill, height, viewBox, width)
import Svg.Styled.Events exposing (onClick)


type alias Drawing =
    String


type alias Size =
    Float


type Icon msg
    = Icon Drawing Size Color (Maybe msg)


toSvg : Icon msg -> Svg msg
toSvg (Icon drawing size color maybeMsg) =
    let
        maybeOnClick =
            case maybeMsg of
                Just msg ->
                    [ onClick msg ]

                Nothing ->
                    []
    in
    -- TODO remove this. It's a hack because I'm tired. Could even remove the
    -- `div`, used because `bottom: 30px` doesn't exist on Svg.Styled.Attributes
    div [ css [ position relative, bottom (px 30), display inlineBlock ] ]
        [ svg
            ([ viewBox "0 0 100 100"
             , width (String.fromFloat size)
             , height (String.fromFloat size)
             ]
                ++ maybeOnClick
            )
            [ path
                [ d drawing
                , fill (toString color)
                ]
                []
            ]
        ]


standard : String -> Icon msg
standard drawing =
    Icon drawing 24 Colors.offWhite Nothing



--withAttrs : List Attribute -> Icon msg -> Icon msg
--withAttrs attrs (Icon drawing size color maybeMsg attrs) =


withSize : Float -> Icon msg -> Icon msg
withSize newSize (Icon drawing _ color maybeMsg) =
    Icon drawing newSize color maybeMsg


withColor : Color -> Icon msg -> Icon msg
withColor newColor (Icon drawing size _ maybeMsg) =
    Icon drawing size newColor maybeMsg


withConditionalColor : Bool -> Color -> Icon msg -> Icon msg
withConditionalColor shouldApply newColor (Icon drawing size oldColor maybeMsg) =
    if shouldApply then
        Icon drawing size newColor maybeMsg

    else
        Icon drawing size oldColor maybeMsg


withOnClick : msg -> Icon msg -> Icon msg
withOnClick msg (Icon drawing size color _) =
    Icon drawing size color (Just msg)


pen : Icon msg
pen =
    standard "M29,64.23l13.16,10-13.88,10a3.59,3.59,0,0,1-5-5Zm3.35-4.78L45.48,69l22-30.62-13.16-10Q43.32,43.89,32.32,59.45Zm25.2-34.73L70.2,34.77l7.18-8.85c.05-.36.68-5.3-3.11-8.61A9.65,9.65,0,0,0,64,15.87Z"


trash : Icon msg
trash =
    standard "M25,29.26V24.07H37.5c-.52-3.1.34-6.13,2.27-7.77A5.54,5.54,0,0,1,43.18,15H58a5.84,5.84,0,0,1,2.28,1.3c2.8,2.6,2.33,7.3,2.27,7.77H75v5.19H70.45v48a9.17,9.17,0,0,1-1.13,3.89A7.94,7.94,0,0,1,63.64,85H37.5a8.19,8.19,0,0,1-6.82-3.89,9,9,0,0,1-1.13-3.89v-48Zm17-5.19H58a4.88,4.88,0,0,0,0-3.88,3.42,3.42,0,0,0-1.13-1.3H43.18a3.42,3.42,0,0,0-1.13,1.3A4.88,4.88,0,0,0,42.05,24.07ZM39.77,35.74V72h3.41V35.74Zm9.09,0V72h3.41V35.74Zm9.09,0V72h3.41V35.74Z"


eye : Icon msg
eye =
    standard "M 50 18.75 C 29.167969 18.75 11.375 31.707031 4.167969 50 C 11.375 68.292969 29.167969 81.25 50 81.25 C 70.832031 81.25 88.625 68.292969 95.832031 50 C 88.625 31.707031 70.855469 18.75 50 18.75 Z M 50 70.832031 C 38.5 70.832031 29.167969 61.5 29.167969 50 C 29.167969 38.5 38.5 29.167969 50 29.167969 C 61.5 29.167969 70.832031 38.5 70.832031 50 C 70.832031 61.5 61.5 70.832031 50 70.832031 Z M 50 37.5 C 43.105469 37.5 37.5 43.105469 37.5 50 C 37.5 56.894531 43.105469 62.5 50 62.5 C 56.894531 62.5 62.5 56.894531 62.5 50 C 62.5 43.105469 56.894531 37.5 50 37.5 Z M 50 37.5"



-- To add an Icon, resize any svg to 100x100px on https://www.iloveimg.com/resize-image
-- , undo = "M15,65.67V34.33L27.54,46.87c1.31-1.17,14-12.09,31.34-8.36C77.8,42.58,84.59,60.37,85,61.49l-8.36,3.14A27.27,27.27,0,0,0,55.75,46.87c-12-2-20.69,5.2-21.94,6.26L46.34,65.67Z"
-- , redo = "M53.66,65.67,66.19,53.13c-1.25-1.06-10-8.27-21.94-6.26A27.27,27.27,0,0,0,23.36,64.63L15,61.49c.41-1.12,7.2-18.91,26.12-23,17.3-3.73,30,7.19,31.34,8.36L85,34.33V65.67Z"
