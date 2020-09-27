module Components.TopBar exposing (Element(..), Msg, State, init, update, view)

import Colors
import Components as C
import Components.Dropdown as Dropdown
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import List.Extra
import Pages exposing (Page(..))


type State msg
    = State
        { dropdownStates : List Dropdown.State
        , toMsg : Msg -> msg
        }


init : (Msg -> msg) -> Int -> State msg
init toMsg numberOfElements =
    -- This can possibly create `Dropdown.State`s for non-Dropdown elements. But it's fine for now.
    -- `numberOfElements + 1` to count for `pagesDropdown`.
    State
        { dropdownStates = List.repeat (numberOfElements + 1) (Dropdown.init False)
        , toMsg = toMsg
        }


type Element msg
    = Dropdown { title : String, elements : List (Html msg) }
      -- TODO Add this option to use with `pagesDropdown`, or something like this
      --| DropdownCloseOnClick { title : String, elements : List (Html msg) }
    | Any (Html msg)


type Msg
    = DropdownMsg Int Dropdown.Msg


view : Page -> List (Element msg) -> State msg -> Html msg
view page pageElements (State { dropdownStates, toMsg }) =
    let
        wrapper =
            div
                [ css
                    [ displayFlex
                    , flexDirection row
                    ]
                ]

        elements =
            pagesDropdown page :: pageElements

        length =
            Debug.log "elements length" <| List.length (List.Extra.zip elements dropdownStates)
    in
    List.Extra.zip elements dropdownStates
        |> List.indexedMap (elementToHtml toMsg)
        |> wrapper


elementToHtml : (Msg -> msg) -> Int -> ( Element msg, Dropdown.State ) -> Html msg
elementToHtml toMsg index ( element, state ) =
    case element of
        Any html ->
            html

        Dropdown { title, elements } ->
            Dropdown.view
                { toMsg = DropdownMsg index >> toMsg
                , title = title
                , elements = elements
                }
                state


update : Msg -> State msg -> ( State msg, Cmd msg )
update msg (State data) =
    let
        { dropdownStates, toMsg } =
            data
    in
    case msg of
        DropdownMsg index dropdownMsg ->
            case List.Extra.getAt index dropdownStates of
                Just dropdownState ->
                    let
                        ( updatedDropdown, cmd ) =
                            Dropdown.update (DropdownMsg index >> toMsg) dropdownMsg dropdownState
                    in
                    ( State { data | dropdownStates = List.Extra.updateAt index (always updatedDropdown) dropdownStates }, cmd )

                Nothing ->
                    ( State data, Cmd.none )



-- HELPERS


pagesDropdown : Page -> Element msg
pagesDropdown currentPage =
    Dropdown
        { title = Pages.toString currentPage
        , elements =
            List.map (pageToAnchor currentPage)
                [ EditorPage
                , GalleryPage
                , WritingPage
                , WelcomePage
                ]
        }


pageToAnchor : Page -> Page -> Html msg
pageToAnchor activePage page =
    let
        activeAttrs =
            if page == activePage then
                [ backgroundColor (Colors.toCssColor Colors.black) ]

            else
                []
    in
    a
        [ css
            ([ color (Colors.toCssColor Colors.offWhite)
             , display block
             , textDecoration none
             , Css.padding2 (px 10) (px 20)
             , cursor pointer
             , textAlign center
             , hover
                [ Css.textDecorationLine Css.underline
                ]
             , active
                [ backgroundColor (Colors.toCssColor Colors.black) ]
             , fontFamily sansSerif
             ]
                ++ activeAttrs
            )
        , href ("/" ++ Pages.routeFor page)
        ]
        [ text (Pages.toString page) ]
