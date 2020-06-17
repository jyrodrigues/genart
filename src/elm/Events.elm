module Events exposing
    ( MidiEvent
    , ShiftKey
    , alwaysPreventDefault
    , keyPressDecoder
    , midiEventDecoder
    , mousePositionDecoder
    , onKeyDown
    , onWheel
    , wheelDecoder
    )

import Html.Styled
import Html.Styled.Events exposing (on, preventDefaultOn)
import Json.Decode as Decode exposing (Decoder)
import LSystem.Image exposing (Position)


type alias ShiftKey =
    Bool


keyPressDecoder : (String -> msg) -> Decoder msg
keyPressDecoder msg =
    Decode.map msg
        (Decode.field "key" Decode.string)


onKeyDown : (String -> msg) -> Html.Styled.Attribute msg
onKeyDown msg =
    on "keydown" (keyPressDecoder msg)


mousePositionDecoder : (Position -> msg) -> Decoder msg
mousePositionDecoder msg =
    Decode.map msg <|
        Decode.map2 Tuple.pair
            (Decode.field "clientX" Decode.float)
            (Decode.field "clientY" Decode.float)


onWheel : (Float -> Float -> ShiftKey -> Position -> msg) -> Html.Styled.Attribute msg
onWheel msg =
    preventDefaultOn "wheel" (Decode.map alwaysPreventDefault (wheelDecoder msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


wheelDecoder : (Float -> Float -> ShiftKey -> Position -> msg) -> Decoder msg
wheelDecoder msg =
    Decode.map4 msg
        (Decode.field "deltaX" Decode.float)
        (Decode.field "deltaY" Decode.float)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.map2 Tuple.pair
            (Decode.field "clientX" Decode.float)
            (Decode.field "clientY" Decode.float)
        )


type alias MidiEvent =
    { command : Float
    , noteMap : Float
    , velocityPosition : Float
    }


{--}
midiEventDecoder : Decoder MidiEvent
midiEventDecoder =
    Decode.map3 MidiEvent
        (Decode.at [ "data", "0" ] Decode.float)
        (Decode.at [ "data", "1" ] Decode.float)
        (Decode.at [ "data", "2" ] Decode.float)
--}
