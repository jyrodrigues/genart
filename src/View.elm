module View exposing (view)

import Colors
import Element as El exposing (Attribute, Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input exposing (button)
import Html
import Html.Attributes
import Html.Events exposing (preventDefaultOn)
import Icons exposing (icons)
import Json.Decode as Decoder exposing (Decoder, bool, field, float)
import LSystem.Core as LCore
    exposing
        ( State
        , Step(..)
        , Transformation
        , applyRule
        , buildState
        , stateLength
        )
import LSystem.Draw exposing (drawSvg, drawSvgFixed, drawSvgFixedWithColor)
import LSystem.String
import Models exposing (Model)
import Update exposing (Msg(..))


columnStyle =
    columnStyleWithColor Colors.darkBlue


columnStyleWithColor color =
    [ El.width El.fill
    , El.height El.fill
    , Background.color color
    , El.padding 20
    , El.scrollbars
    ]


baseColumnWithColor color =
    El.column (columnStyleWithColor color)


baseColumn =
    El.column columnStyle


baseLayout backgroundColor body =
    El.layout
        [ El.width El.fill
        , El.height El.fill
        , Background.color backgroundColor
        , El.padding 20
        ]
        body


withBorder el style children =
    el
        (style
            ++ [ Border.color Colors.green_
               , Border.solid
               , Border.width 1
               ]
        )
        children


colorBox color msg =
    El.el
        [ Background.color color
        , El.width <| El.fillPortion 1
        , El.height <| El.fillPortion 1
        , Events.onClick (msg color)
        ]
        (El.text "")


view : Model -> Html.Html Msg
view model =
    baseLayout model.backgroundColor <|
        baseColumnWithColor model.backgroundColor <|
            [ topRow model
            , withBorder El.row
                (filling 1 5 ++ [ El.spacing 5 ])
                [ stateCompositionView model.state model.editingIndex
                , mainSvgView model
                ]
            , withBorder El.row
                (filling 1 1 ++ [ El.spacing 5 ])
                (List.map (\color -> colorBox color SetBackgroundColor) Colors.allColors)
            , withBorder El.row
                (filling 1 1 ++ [ El.spacing 5 ])
                (List.map (\color -> colorBox color SetDrawColor) Colors.allColors)
            ]


size : Float
size =
    1500.0


addBorder =
    [ Border.color Colors.darkGray
    , Border.solid
    , Border.width 1
    ]


filling w h =
    [ El.width <| El.fillPortion w
    , El.height <| El.fillPortion h
    ]


bf11 =
    addBorder ++ filling 1 1


styledButton :
    { onPress : Maybe Msg
    , label : Element Msg
    }
    -> Element Msg
styledButton =
    button <| bf11 ++ [ Background.color Colors.offWhite ]


styledEl : List (Attribute Msg) -> Element Msg -> Element Msg
styledEl attr =
    El.el <| attr ++ [ Background.color Colors.offWhite ]


topRow : Model -> Element Msg
topRow model =
    let
        editingTransform =
            LCore.getTransformAt model.editingIndex model.state

        recState =
            { base = [ D ]
            , transforms = [ editingTransform ]
            }
    in
    El.column (bf11 ++ [ El.scrollbars, El.spacing 5 ])
        [ El.row (bf11 ++ [ El.scrollbars, El.spacing 5 ])
            -- todo: cleanup here
            -- [ styledButton { onPress = Just SaveState, label = El.text "Save State" }
            [ styledButton { onPress = Just ClearSvg, label = El.text "ClearSvg" }
            , styledButton { onPress = Just (Iterate editingTransform), label = El.text "Iterate" }
            , styledButton { onPress = Just Deiterate, label = El.text "Deiterate" }

            -- , styledButton { onPress = Just ToggleShowNextIteration, label = El.text "ToggleShowNextIteration" }
            -- , styledEl bf11 (El.text <| "Status: " ++ onOff model.isShowingNextIteration)
            , styledEl bf11 (El.text <| " Fixed: " ++ onOff model.fixed)
            ]
        , El.row (filling 1 4 ++ [ El.scrollbars, El.spacing 5 ])
            [ El.column (bf11 ++ [ El.scrollbars ])
                [ styledEl (filling 1 1)
                    (El.text <|
                        (String.fromInt <| Tuple.first <| stateLength model.state)
                            ++ ", "
                            ++ (String.fromInt <| Tuple.second <| stateLength model.state)
                    )
                , styledEl (filling 1 1) (El.text <| "{" ++ model.dir ++ "}")
                , styledEl (filling 1 1) (El.text <| LSystem.String.fromTransform editingTransform)
                ]
            , styledEl (addBorder ++ filling 1 1 ++ [ El.scrollbars ]) (El.html <| drawSvg recState 60 60 0 0)
            ]
        ]


onOff : Bool -> String
onOff bool =
    if bool then
        "On"

    else
        "Off"


stateCompositionView : State -> Int -> Element Msg
stateCompositionView state editingIndex =
    let
        transforms =
            LCore.toList state
    in
    El.column
        ([ El.width (El.minimum 200 El.fill)
         , El.height El.fill
         , El.scrollbars
         ]
            ++ addBorder
        )
        (List.reverse <| List.indexedMap elFromTransform <| List.map (\t -> ( t, editingIndex )) transforms)


elFromTransform : Int -> ( Transformation, Int ) -> Element Msg
elFromTransform index ( transform, editingIndex ) =
    El.row
        -- todo: change (El.fill |> El.minimum 80 |> El.maximum 80) to (El.px 80) and make it work
        ([ El.height (El.fill |> El.minimum 80 |> El.maximum 80)
         , El.width El.fill
         , Background.color Colors.gray
         ]
            ++ addBorder
        )
        [ El.el (filling 1 1) <| El.html <| drawSvgFixed transform
        , El.column []
            -- [ El.html <| Icons.draw24px icons.eye
            [ penIcon index editingIndex
            , trashIcon index
            ]
        ]


penIcon index editingIndex =
    El.el
        [ Events.onClick (SetEditingIndex index) ]
        (El.html <|
            if index /= editingIndex then
                Icons.draw24px icons.pen

            else
                Icons.drawWithColor 24 Colors.green_ icons.pen
        )


trashIcon index =
    El.el
        [ Events.onClick (DropFromState index) ]
        (El.html <| Icons.drawWithColor 24 Colors.red_ icons.trash)


mainSvgView : Model -> Element Msg
mainSvgView model =
    El.el ([ El.scrollbars, modifyWheelEvent ] ++ addBorder ++ filling 7 1)
        (El.html <|
            if model.fixed then
                svgDivFixed model

            else
                svgDiv model
        )



-- Todo: refactor modifyWheelEvent/alwaysPreventDefault/wheelDecoder out of here


modifyWheelEvent =
    El.htmlAttribute <| preventDefaultOn "wheel" (Decoder.map alwaysPreventDefault wheelDecoder)


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


wheelDecoder : Decoder Msg
wheelDecoder =
    Decoder.map3 Zoom
        (field "deltaX" float)
        (field "deltaY" float)
        (field "shiftKey" bool)


svgDiv : Model -> Html.Html Msg
svgDiv model =
    Html.div []
        [ drawSvg
            -- (if model.isShowingNextIteration then
            --     applyRule model.recording model.state
            --  else
            model.state
            -- )
            (mapZoomLevelToSize model.zoomLevel)
            (mapZoomLevelToSize model.zoomLevel)
            model.wDelta
            model.hDelta
        ]


mapZoomLevelToSize zl =
    max 10.0 (zl * 4.0)


svgDivFixed : Model -> Html.Html Msg
svgDivFixed model =
    drawSvgFixedWithColor model.drawColor <| buildState model.state
