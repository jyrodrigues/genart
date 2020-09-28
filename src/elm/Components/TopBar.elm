module Components.TopBar exposing (Element(..), Msg, State, init, update, view)

import Colors
import Components as C
import Components.Dropdown as Dropdown
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import List.Extra
import Pages exposing (Page(..))



-- MODEL
-- STATE


type State msg
    = State
        { dropdownStates : List Dropdown.State
        , toMsg : Msg -> msg
        , openDropdownIndex : Maybe Int
        }


init : (Msg -> msg) -> State msg
init toMsg =
    let
        numberOfElements =
            -- max number of elements this top bar can handle
            12
    in
    -- This can possibly create `Dropdown.State`s for non-Dropdown elements. But it's fine for now.
    State
        { dropdownStates = List.repeat numberOfElements (Dropdown.init False)
        , toMsg = toMsg
        , openDropdownIndex = Nothing
        }


type Element msg
    = Dropdown { title : String, body : Html msg }
      -- TODO Add this option to use with `pagesDropdown`, or something like this
      --| DropdownCloseOnClick { title : String, elements : List (Html msg) }
    | Any (Html msg)



-- MSG


type Msg
    = DropdownMsg Int Dropdown.Msg



-- VIEW


view : Page -> List (Element msg) -> State msg -> Html msg
view page elements (State { dropdownStates, toMsg }) =
    let
        indexedElements =
            (pagesDropdown page :: elements)
                -- (state, elem)
                |> List.Extra.zip dropdownStates
                -- (index, (state, elem))
                |> List.indexedMap Tuple.pair

        pagesToHtml =
            elementToHtml toMsg (Dropdown.customView [ position absolute, left (px 10) ])

        elementsToHtml =
            List.map (elementToHtml toMsg Dropdown.view) >> List.intersperse C.separator
    in
    case indexedElements of
        pagesDropdownTuple :: elementsTuples ->
            div wrapperAttrs (pagesToHtml pagesDropdownTuple :: elementsToHtml elementsTuples)

        [] ->
            div wrapperAttrs []


elementToHtml : (Msg -> msg) -> Dropdown.View msg -> ( Int, ( Dropdown.State, Element msg ) ) -> Html msg
elementToHtml toMsg view_ ( index, ( state, element ) ) =
    case element of
        Any html ->
            html

        Dropdown { title, body } ->
            view_
                { toMsg = DropdownMsg index >> toMsg
                , title = title
                , body = body
                }
                state



-- ATTRIBUTES


wrapperAttrs : List (Html.Styled.Attribute msg)
wrapperAttrs =
    [ css
        [ displayFlex
        , flexDirection row
        , justifyContent center
        , backgroundColor (Colors.toCssColor Colors.theme.backgroundColor)
        , height (px 40)
        , position relative
        , borderBottom3 (px 1) solid (Colors.toCssColor Colors.black)
        , boxShadow5 zero (px -3) (px 10) (px 1) (Colors.toCssColor Colors.black)
        , zIndex (int 1)
        ]
    ]



-- UPDATE


update : Msg -> State msg -> ( State msg, Cmd msg )
update (DropdownMsg index dropdownMsg) (State data) =
    let
        { dropdownStates, toMsg, openDropdownIndex } =
            data
    in
    case List.Extra.getAt index dropdownStates of
        Just dropdownState ->
            let
                ( updatedDropdown, cmd ) =
                    Dropdown.update (DropdownMsg index >> toMsg) dropdownMsg dropdownState

                updatedDropdownStates =
                    dropdownStates
                        |> List.Extra.setAt index updatedDropdown

                -- Close last open dropdown if it exists, is different from current updated and current one became open.
                closeLastOpenIfNeeded =
                    openDropdownIndex
                        |> Maybe.map
                            (\lastOpenIndex ->
                                if lastOpenIndex /= index && updatedDropdown.isOpen then
                                    List.Extra.updateAt lastOpenIndex Dropdown.close

                                else
                                    identity
                            )
                        |> Maybe.withDefault identity
            in
            ( State
                { data
                    | dropdownStates = closeLastOpenIfNeeded updatedDropdownStates
                    , openDropdownIndex =
                        if updatedDropdown.isOpen then
                            Just index

                        else
                            Nothing
                }
            , cmd
            )

        Nothing ->
            ( State data, Cmd.none )



-- HELPERS


pagesDropdown : Page -> Element msg
pagesDropdown currentPage =
    Dropdown
        { title = "Genart " ++ Pages.toString currentPage ++ " ▾"
        , body =
            div [ css [ width (px 100) ] ] <|
                List.map (pageToAnchor currentPage)
                    [ EditorPage
                    , GalleryPage
                    , WritingPage
                    , WelcomePage
                    ]
        }


pageToAnchor : Page -> Page -> Html msg
pageToAnchor activePage page =
    a
        [ css (Dropdown.listItemStyle (activePage == page))
        , href ("/" ++ Pages.routeFor page)
        ]
        [ text (Pages.toString page) ]
