module Components exposing
    ( Elem
    , box
    , button
    , colorButton
    , colorsRow
    , column
    , icon
    , layout
    , row
    , styledBox
    , styledButton
    , styledColumn
    , styledRow
    , toElement
    , withAttributes
    , withBgColor
    , withBorder
    , withHeight
    , withHeightAsLength
    , withHeightFill
    , withOnClick
    , withPadding
    , withScrollbars
    , withSpacing
    , withWidth
    , withWidthAndHeight
    , withWidthAndHeightFill
    , withWidthAsLength
    , withWidthFill
    )

import Colors
import Element as El exposing (Attribute, Color, Element, Length)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input exposing (button)
import Html
import Html.Attributes
import Html.Events exposing (preventDefaultOn)
import Icons exposing (icons)



-- Layout --


layout : Color -> Element msg -> Html.Html msg
layout backgroundColor body =
    El.layout
        [ El.width El.fill
        , El.height El.fill
        , Background.color backgroundColor
        , El.padding 15
        ]
        body



-- TYPES


type Elem msg
    = Column (Options msg) (List (Element msg))
    | Row (Options msg) (List (Element msg))
    | Box (Options msg) (Element msg)
    | Button (Options msg) msg (Element msg)


type alias Options msg =
    { width : Length
    , height : Length
    , backgroundColor : Color
    , border : Maybe BorderOptions
    , spacing : Int
    , padding : Int
    , attributes : List (Attribute msg)
    , onClick : Maybe msg
    , scrollbars : Bool
    }


type alias BorderOptions =
    { color : Color
    , width : Int
    }



-- CONSTRUCTORS


defaultOptions : Options msg
defaultOptions =
    { width = El.fillPortion 1
    , height = El.fillPortion 1
    , backgroundColor = Colors.darkGray
    , border = Nothing
    , spacing = 0
    , padding = 0
    , attributes = []
    , onClick = Nothing
    , scrollbars = False
    }


column : List (Element msg) -> Elem msg
column children =
    Column defaultOptions children


row : List (Element msg) -> Elem msg
row children =
    Row defaultOptions children


box : Element msg -> Elem msg
box child =
    Box defaultOptions child


button : msg -> Element msg -> Elem msg
button msg child =
    Button defaultOptions msg child


icon : msg -> Bool -> Color -> String -> Element msg
icon msg isSet isSetColor iconSvgString =
    let
        drawnIcon =
            if isSet then
                Icons.drawWithColor 24 isSetColor iconSvgString

            else
                Icons.draw24px iconSvgString
    in
    box (El.html drawnIcon)
        |> withOnClick msg
        |> toElement



-- VIEW


toElement : Elem msg -> Element msg
toElement elem =
    case elem of
        Row options children ->
            El.row
                (optionsToAttrs options)
                children

        Column options children ->
            El.column
                (optionsToAttrs options)
                children

        Box options child ->
            El.el
                (optionsToAttrs options)
                child

        Button options msg child ->
            El.el
                (optionsToAttrs options)
                child


optionsToAttrs : Options msg -> List (Attribute msg)
optionsToAttrs options =
    [ El.width options.width
    , El.height options.height
    , Background.color options.backgroundColor
    ]
        ++ borderToAttrs options.border


borderToAttrs : Maybe BorderOptions -> List (Attribute msg)
borderToAttrs maybeBorder =
    case maybeBorder of
        Just border ->
            [ Border.color border.color
            , Border.width border.width
            , Border.solid
            ]

        Nothing ->
            []



-- WITH *


withAttributes : List (Attribute msg) -> Elem msg -> Elem msg
withAttributes attributes elem =
    changeOptions elem
        (\options ->
            { options
                | attributes =
                    options.attributes
                        ++ attributes
            }
        )


withWidthAsLength : Length -> Elem msg -> Elem msg
withWidthAsLength width elem =
    changeOptions elem (\options -> { options | width = width })


withHeightAsLength : Length -> Elem msg -> Elem msg
withHeightAsLength height elem =
    changeOptions elem (\options -> { options | height = height })


withWidth : Int -> Elem msg -> Elem msg
withWidth portion elem =
    elem
        |> withWidthAsLength (El.fillPortion portion)


withHeight : Int -> Elem msg -> Elem msg
withHeight portion elem =
    elem
        |> withHeightAsLength (El.fillPortion portion)


withWidthAndHeight : Int -> Int -> Elem msg -> Elem msg
withWidthAndHeight widthPortion heightPortion elem =
    elem
        |> withWidth widthPortion
        |> withHeight heightPortion


withWidthFill : Elem msg -> Elem msg
withWidthFill elem =
    elem
        |> withWidthAsLength El.fill


withHeightFill : Elem msg -> Elem msg
withHeightFill elem =
    elem
        |> withHeightAsLength El.fill


withWidthAndHeightFill : Elem msg -> Elem msg
withWidthAndHeightFill elem =
    elem
        |> withWidthFill
        |> withHeightFill


withScrollbars : Elem msg -> Elem msg
withScrollbars elem =
    changeOptions elem (\options -> { options | scrollbars = True })


withBorder : Elem msg -> Elem msg
withBorder elem =
    changeOptions elem
        (\options ->
            { options
                | border =
                    Just
                        { color = Colors.gray
                        , width = 1
                        }
            }
        )


withSpacing : Int -> Elem msg -> Elem msg
withSpacing spacing elem =
    changeOptions elem (\options -> { options | spacing = spacing })


withPadding : Int -> Elem msg -> Elem msg
withPadding padding elem =
    changeOptions elem (\options -> { options | padding = padding })


withBgColor : Color -> Elem msg -> Elem msg
withBgColor color elem =
    changeOptions elem (\options -> { options | backgroundColor = color })


withOnClick : msg -> Elem msg -> Elem msg
withOnClick msg elem =
    changeOptions elem (\options -> { options | onClick = Just msg })



-- AUX TO WITH *


changeOptions : Elem msg -> (Options msg -> Options msg) -> Elem msg
changeOptions elem change =
    case elem of
        Row options children ->
            Row (change options) children

        Column options children ->
            Column (change options) children

        Box options child ->
            Box (change options) child

        Button options msg child ->
            Button (change options) msg child



-- STYLED COMPONENTS


colorsRow : (Color -> msg) -> Element msg
colorsRow msg =
    row
        (List.map (\color -> colorButton color msg) Colors.allColors)
        |> withBorder
        |> withWidthAndHeight 1 1
        |> withSpacing 5
        |> toElement


colorButton : Color -> (Color -> msg) -> Element msg
colorButton color msg =
    button
        (msg color)
        (El.text "")
        |> withOnClick (msg color)
        |> withWidthAndHeight 1 1
        |> withBgColor color
        |> toElement


styledRow : List (Element msg) -> Elem msg
styledRow children =
    row children
        |> withBorder
        |> withWidthAndHeight 1 1
        |> withSpacing 5
        |> withScrollbars


styledColumn : List (Element msg) -> Elem msg
styledColumn children =
    column children
        |> withBorder
        |> withWidthAndHeight 1 1
        |> withSpacing 5
        |> withScrollbars


styledButton : msg -> String -> Elem msg
styledButton msg label =
    button msg (El.text label)
        |> withWidthAndHeight 1 1
        |> withBorder
        |> withBgColor Colors.offWhite
        |> withOnClick msg


styledBox : String -> Elem msg
styledBox label =
    box (El.text label)
        |> withWidthAndHeight 1 1
        |> withBorder
        |> withBgColor Colors.offWhite
