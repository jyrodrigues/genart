module Svg.Icon exposing
    ( Icon
    , colorIf
    , eye
    , iconDelete
    , iconDuplicate
    , iconTrash
    , pen
    , toSvg
    )

import Colors exposing (Color, toString)
import Css exposing (bottom, display, inlineBlock, position, px, relative)
import Html.Styled exposing (div)
import Html.Styled.Attributes as HtmlAttributes exposing (css)
import Html.Styled.Events exposing (stopPropagationOn)
import Json.Decode as Decode
import Svg.Styled as Svg exposing (Svg, line, path, svg)
import Svg.Styled.Attributes as Attrs
    exposing
        ( d
        , fill
        , height
        , stroke
        , strokeLinecap
        , strokeLinejoin
        , strokeWidth
        , viewBox
        , width
        , x1
        , x2
        , y1
        , y2
        )


type alias Icon msg =
    { path : String
    , size : Float
    , fillColor : Color
    , onClick : Maybe msg
    , css : List Css.Style
    }


toSvg : Icon msg -> Svg msg
toSvg { path, size, fillColor, onClick, css } =
    let
        maybeOnClick =
            case onClick of
                Just msg ->
                    [ stopPropagationOn "click" (Decode.map alwaysStopPropagation (Decode.succeed msg)) ]

                Nothing ->
                    []
    in
    svg
        ([ viewBox "0 0 100 100"
         , width (String.fromFloat size)
         , height (String.fromFloat size)
         , fill "none"
         ]
            ++ maybeOnClick
        )
        [ Svg.path
            [ d path
            , fill (toString fillColor)

            -- , stroke (toString strokeColor)
            , stroke (toString fillColor)
            , strokeWidth "10"
            , strokeLinecap "round"
            , strokeLinejoin "round"
            ]
            []
        ]



-- ]


alwaysStopPropagation : msg -> ( msg, Bool )
alwaysStopPropagation msg =
    ( msg, True )


standardOldAPI : String -> Icon msg
standardOldAPI path =
    { path = path
    , size = 24
    , fillColor = Colors.offWhite
    , onClick = Nothing
    , css = []
    }


colorIf : Bool -> Color -> Icon msg -> Icon msg
colorIf shouldApply newColor { path, size, fillColor, onClick, css } =
    if shouldApply then
        Icon path size newColor onClick css

    else
        Icon path size fillColor onClick css


pen : Icon msg
pen =
    standardOldAPI "M29,64.23l13.16,10-13.88,10a3.59,3.59,0,0,1-5-5Zm3.35-4.78L45.48,69l22-30.62-13.16-10Q43.32,43.89,32.32,59.45Zm25.2-34.73L70.2,34.77l7.18-8.85c.05-.36.68-5.3-3.11-8.61A9.65,9.65,0,0,0,64,15.87Z"


iconTrash : Icon msg
iconTrash =
    standardOldAPI "M25,29.26V24.07H37.5c-.52-3.1.34-6.13,2.27-7.77A5.54,5.54,0,0,1,43.18,15H58a5.84,5.84,0,0,1,2.28,1.3c2.8,2.6,2.33,7.3,2.27,7.77H75v5.19H70.45v48a9.17,9.17,0,0,1-1.13,3.89A7.94,7.94,0,0,1,63.64,85H37.5a8.19,8.19,0,0,1-6.82-3.89,9,9,0,0,1-1.13-3.89v-48Zm17-5.19H58a4.88,4.88,0,0,0,0-3.88,3.42,3.42,0,0,0-1.13-1.3H43.18a3.42,3.42,0,0,0-1.13,1.3A4.88,4.88,0,0,0,42.05,24.07ZM39.77,35.74V72h3.41V35.74Zm9.09,0V72h3.41V35.74Zm9.09,0V72h3.41V35.74Z"


eye : Icon msg
eye =
    standardOldAPI "M 50 18.75 C 29.167969 18.75 11.375 31.707031 4.167969 50 C 11.375 68.292969 29.167969 81.25 50 81.25 C 70.832031 81.25 88.625 68.292969 95.832031 50 C 88.625 31.707031 70.855469 18.75 50 18.75 Z M 50 70.832031 C 38.5 70.832031 29.167969 61.5 29.167969 50 C 29.167969 38.5 38.5 29.167969 50 29.167969 C 61.5 29.167969 70.832031 38.5 70.832031 50 C 70.832031 61.5 61.5 70.832031 50 70.832031 Z M 50 37.5 C 43.105469 37.5 37.5 43.105469 37.5 50 C 37.5 56.894531 43.105469 62.5 50 62.5 C 56.894531 62.5 62.5 56.894531 62.5 50 C 62.5 43.105469 56.894531 37.5 50 37.5 Z M 50 37.5"


iconDuplicate : Icon msg
iconDuplicate =
    standardOldAPI "M 88.277344 21.988281 L 67.179688 0.890625 C 66.601562 0.3125 65.820312 0 65.027344 0 L 34.492188 0 C 32.8125 0 31.449219 1.363281 31.449219 3.042969 L 31.449219 20.511719 L 13.980469 20.511719 C 12.300781 20.511719 10.9375 21.875 10.9375 23.554688 L 10.9375 96.957031 C 10.9375 98.636719 12.300781 100 13.980469 100 L 65.613281 100 C 67.292969 100 68.65625 98.636719 68.65625 96.957031 L 68.65625 79.488281 L 86.125 79.488281 C 87.804688 79.488281 89.167969 78.125 89.167969 76.445312 L 89.167969 24.140625 C 89.167969 23.367188 88.871094 22.582031 88.277344 21.988281 Z M 83.082031 73.402344 L 68.65625 73.402344 L 68.65625 44.652344 C 68.65625 43.863281 68.347656 43.085938 67.765625 42.5 L 46.667969 21.402344 C 46.09375 20.828125 45.316406 20.511719 44.515625 20.511719 L 37.535156 20.511719 L 37.535156 6.085938 L 61.984375 6.085938 L 61.984375 24.140625 C 61.984375 25.820312 63.347656 27.183594 65.027344 27.183594 L 83.082031 27.183594 Z M 62.570312 93.914062 L 17.023438 93.914062 L 17.023438 26.597656 L 41.472656 26.597656 L 41.472656 44.652344 C 41.472656 46.332031 42.835938 47.695312 44.515625 47.695312 L 62.570312 47.695312 Z M 47.558594 30.898438 L 58.269531 41.609375 L 47.558594 41.609375 Z M 78.78125 21.097656 L 68.070312 21.097656 L 68.070312 10.386719 C 69.410156 11.726562 77.710938 20.03125 78.78125 21.097656 Z M 78.78125 21.097656"



-- To add an Icon, resize any svg to 100x100px on https://www.iloveimg.com/resize-image
-- , undo = "M15,65.67V34.33L27.54,46.87c1.31-1.17,14-12.09,31.34-8.36C77.8,42.58,84.59,60.37,85,61.49l-8.36,3.14A27.27,27.27,0,0,0,55.75,46.87c-12-2-20.69,5.2-21.94,6.26L46.34,65.67Z"
-- , redo = "M53.66,65.67,66.19,53.13c-1.25-1.06-10-8.27-21.94-6.26A27.27,27.27,0,0,0,23.36,64.63L15,61.49c.41-1.12,7.2-18.91,26.12-23,17.3-3.73,30,7.19,31.34,8.36L85,34.33V65.67Z"


iconDelete : Icon msg
iconDelete =
    standardOldAPI "M 75 25 L 25 75 M 25 25 L 75 75"
