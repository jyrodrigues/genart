module View exposing (view)

import Colors exposing (..)
import Element as El exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input exposing (button)
import Html
import Html.Attributes
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decoder exposing (Decoder, bool, field, float)
import LSystem.Core
    exposing
        ( State
        , Step(..)
        , Transformation
        , applyRule
        , buildState
        , getLastTransform
        , stateLength
        , stateToString
        , transformToString
        )
import LSystem.Draw exposing (drawSvg, drawSvgFixed)
import Models exposing (Model)
import Update exposing (Msg(..))


view : Model -> Html.Html Msg
view model =
    El.layout
        [ El.width El.fill
        , El.height El.fill
        , Background.color Colors.lightBlue
        , El.padding 20
        ]
    <|
        El.column
            [ El.width El.fill
            , El.height El.fill
            , Background.color Colors.darkBlue
            , El.padding 20
            , El.scrollbars
            ]
            [ topRow model
            , El.row (addBorder ++ filling 1 5 ++ [ El.spacing 5 ])
                [ stateCompositionView model.state
                , El.el (addBorder ++ filling 7 1 ++ [ El.scrollbars, modifyWheelEvent ])
                    (El.html <|
                        if model.fixed then
                            svgDivFixed model

                        else
                            svgDiv model
                    )
                ]
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
        recState =
            { base = [ D ]
            , transforms = [ getLastTransform model.state ]
            }
    in
    El.column (bf11 ++ [ El.scrollbars, El.spacing 5 ])
        [ El.row (bf11 ++ [ El.scrollbars, El.spacing 5 ])
            [ styledButton { onPress = Just SaveState, label = El.text "Save State" }
            , styledButton { onPress = Just ResetStep, label = El.text "ResetStep" }
            , styledButton { onPress = Just ClearSvg, label = El.text "ClearSvg" }
            , styledButton { onPress = Just (Iterate <| getLastTransform model.state), label = El.text "Iterate" }
            , styledButton { onPress = Just Deiterate, label = El.text "Deiterate" }
            , styledButton { onPress = Just ToggleShowNextIteration, label = El.text "ToggleShowNextIteration" }
            , styledEl bf11 (El.text <| "Status: " ++ onOff model.isShowingNextIteration)
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
                , styledEl (filling 1 1) (El.text <| "->" ++ model.dir ++ "<-")
                , styledEl (filling 1 1) (El.text <| transformToString <| getLastTransform model.state)
                ]
            , styledEl (addBorder ++ filling 1 1 ++ [ El.scrollbars ]) (El.html <| drawSvg recState 60 60 0 0)
            ]
        ]


onOff bool =
    if bool then
        "On"

    else
        "Off"


elFromState : Int -> State -> Element Msg
elFromState index state =
    let
        stateComposition =
            buildState state
    in
    El.row
        (addBorder
            ++ [ El.height (El.fill |> El.minimum 100)
               , El.width El.fill
               , Background.color Colors.gray
               ]
        )
        [ El.column []
            [ styledButton { onPress = Just (Exclude index), label = El.text "Exclude" }
            , styledButton { onPress = Just (SetAsBase state), label = El.text "Use this svg" }
            , styledButton { onPress = Just (Iterate stateComposition), label = El.text "Iterate" }
            , styledEl (filling 1 1)
                (El.text
                    ((++) "Size: " <|
                        String.fromInt <|
                            let
                                ( ds, os ) =
                                    stateLength state
                            in
                            ds + os
                    )
                )
            ]
        , El.el (filling 1 1) <| El.html <| drawSvgFixed stateComposition
        ]


elFromTransform : Int -> Transformation -> Element Msg
elFromTransform index transform =
    El.row
        (addBorder
            ++ [ El.height (El.fill |> El.minimum 100)
               , El.width El.fill
               , Background.color Colors.gray
               ]
        )
        [ El.column []
            [ styledButton { onPress = Just (Exclude index), label = El.text "Exclude" }

            -- , styledButton { onPress = Just (SetAsBase state), label = El.text "Use this svg" }
            , styledButton { onPress = Just (Iterate transform), label = El.text "Iterate" }

            -- , styledEl (filling 1 1)
            --     (El.text
            --         ((++) "Size: " <|
            --             String.fromInt <|
            --                 let
            --                     ( ds, os ) =
            --                         stateLength state
            --                 in
            --                 ds + os
            --         )
            --     )
            ]
        , El.el (filling 1 1) <| El.html <| drawSvgFixed transform
        ]


stateCompositionView : State -> Element Msg
stateCompositionView state =
    let
        x =
            Debug.log state

        transforms =
            List.reverse <| state.base :: state.transforms
    in
    El.column (addBorder ++ filling 1 1 ++ [ El.scrollbars ])
        (List.indexedMap elFromTransform transforms)



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
    drawSvgFixed <| buildState model.state
