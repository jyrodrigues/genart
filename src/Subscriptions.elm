module Subscriptions exposing (keyDecoder, subscriptions)

import Browser.Events exposing (onKeyPress, onKeyUp)
import Json.Decode as Decode exposing (Decoder)
import Models exposing (Model)
import Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyUp (Decode.map KeyPress keyDecoder) ]



-- type Direction
--     = Left
--     | Right
--     | Up
--     | Skip
--     | Backspace1
--     | Other
-- -- keyDecoder : Decoder Direction


keyDecoder : Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- -- Decode.map toDirection (Decode.field "key" Decode.string)
-- toDirection : String -> Direction
-- toDirection string =
--     case string of
--         "ArrowLeft" ->
--             Left
--         "ArrowRight" ->
--             Right
--         "ArrowUp" ->
--             Up
--         "Space" ->
--             Skip
--         "Backspace" ->
--             Backspace1
--         _ ->
--             Other
-- [D L D D L D D D D L D D L D D D D D D R D L D L D D D D D D D L]
