module Components.TopBar exposing (Config, Element(..), Msg, State, View, init, update, view)

import Components as C
import Components.Dropdown as Dropdown
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import List.Extra


type alias State =
    Maybe Int


init : State
init =
    Nothing


type alias Config msg =
    { elements : List (Element msg)
    , toMsg : Msg -> msg
    }


type Element msg
    = Dropdown { title : String, elements : List (Html msg) }
    | Any (Html msg)


type Msg
    = DropdownMsg Int Dropdown.Msg


type alias View msg =
    Config msg -> State -> Html msg


view : View msg
view { elements, toMsg } state =
    let
        wrapper =
            div
                [ css
                    [ displayFlex
                    , flexDirection row
                    ]
                ]

        indexOpen =
            -- Indexes start at 0 so `-1000` is the same as none (not pretty I know).
            Maybe.withDefault -1000 state
    in
    elements
        |> List.indexedMap (elementToHtml toMsg indexOpen)
        |> wrapper


elementToHtml : (Msg -> msg) -> Int -> Int -> Element msg -> Html msg
elementToHtml toMsg indexOpen index element =
    case element of
        Any html ->
            html

        Dropdown { title, elements } ->
            Dropdown.view
                { toMsg = DropdownMsg index >> toMsg
                , title = title
                , elements = elements
                }
                (indexOpen == index)


update : Msg -> State -> State
update msg state =
    case msg of
        DropdownMsg index dropdownMsg ->
            -- N.B. We could have multiple dropdowns opened at the same time, in that case we would pass the
            -- `dropdownMsg` to one of them, but then we'd have to store each state inside our own.
            -- Right now we only want ONE open at a time, so we force the state and ignore the msg.
            if state == Just index then
                -- Assuming *ONLY* ToggleDropdown messages (not close on blur, can be a bug. TODO)
                Nothing

            else
                Just index



{--
type InternalElement msg
    = InternalDropdown (Dropdown.Config msg) Dropdown.State
    | InternalAny (Html msg)
--}
{--
init : State msg
init =
    State
        (List.map
            (\element ->
                case element of
                    Any html ->
                        InternalAny html

                    Dropdown config ->
                        InternalDropdown config (Dropdown.init False)
            )
            elements
        )


update : Msg -> State msg -> State msg
update msg (State items) =
    case msg of
        DropdownMsg dropdownMsg msgIndex ->
            State (List.Extra.updateAt msgIndex (updateDropdown dropdownMsg) items)


updateDropdown : Dropdown.Msg -> InternalElement msg -> InternalElement msg
updateDropdown msg element =
    case element of
        InternalAny _ ->
            element

        InternalDropdown config state ->
            InternalDropdown config (Dropdown.update msg state)
--}
