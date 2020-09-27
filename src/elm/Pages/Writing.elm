module Pages.Writing exposing (ExternalMsg(..), Model, Msg, initialCmd, initialModel, subscriptions, update, view)

import Browser
import Browser.Dom
import Colors exposing (Color)
import Components as C
import Components.TopBar as TopBar
import Css
    exposing
        ( auto
        , backgroundColor
        , borderColor
        , bottom
        , calc
        , color
        , display
        , fixed
        , height
        , hidden
        , hover
        , inlineFlex
        , margin2
        , minus
        , overflow
        , padding
        , pct
        , position
        , px
        , width
        , zero
        )
import Html.Styled exposing (Html, button, div, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick, onInput)
import LSystem.Core as LCore exposing (Block, Composition, Step(..))
import LSystem.Draw exposing (drawImage)
import LSystem.Image as Image exposing (Image)
import Pages exposing (Page(..))
import Task
import Time



-- MODEL


type alias Model =
    { image : Image
    , isPlaying : Bool
    , videoAngleChangeRate : Float
    , writing : String
    , copies : Int
    , topBar : TopBar.State Msg
    }


initialWriting : String
initialWriting =
    "ART  "


initialVideoAngleChangeRate : Float
initialVideoAngleChangeRate =
    0.001


initialNumberOfCopies : Int
initialNumberOfCopies =
    20


initialImage : Image
initialImage =
    Image.welcomeImage
        |> Image.withComposition (stringToComposition initialWriting initialNumberOfCopies)
        |> Image.withStrokeWidth 0.01


initialModel : Model
initialModel =
    { image = initialImage
    , isPlaying = True
    , videoAngleChangeRate = initialVideoAngleChangeRate
    , writing = initialWriting
    , copies = initialNumberOfCopies
    , topBar = TopBar.init TopBarMsg (List.length topBarElements)
    }



-- MSG


type Msg
    = VideoTick
    | InputWriting String
    | GoToEditor
    | SumCopies Int
    | AdjustWidth Float
    | ToggleVideo
    | ResetAngle
    | NoOp
    | TopBarMsg TopBar.Msg


type ExternalMsg
    = UpdateWriting
    | OpenedEditor Image
    | NothingToUpdate



-- VIEW


topBarElements : List (TopBar.Element Msg)
topBarElements =
    -- TODO Change all below
    [ TopBar.Dropdown
        { title = "Title 2"
        , elements =
            [ div []
                [ div [] [ text "item" ]
                , div [ onClick ResetAngle ] [ text "ResetAngle" ]
                , div [] [ text "item" ]
                ]
            ]
        }
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Writing to Genart"
    , body =
        [ TopBar.view WritingPage topBarElements model.topBar |> toUnstyled
        , div
            [ css
                [ width (pct 100)
                , height (pct 100)
                , overflow hidden
                ]
            ]
            [ drawImage (Just "WritingVideo") Nothing False model.image
            , controls model
            ]
            |> toUnstyled
        ]
    }


controls : Model -> Html Msg
controls model =
    div
        [ css
            [ position fixed
            , bottom (px 40)
            , margin2 zero (calc (pct 50) minus (px 200))
            , width (px 400)
            , height (px 100)
            , padding (px 10)
            , color (Colors.toCssColor Colors.offWhite)
            , Css.displayFlex
            , Css.flexDirection Css.row
            ]
        ]
        [ inputWriting model
        , div [ css [] ]
            [ controlButton ToggleVideo "Play/Pause"
            , controlButton ResetAngle "Reset Video"
            , controlButton (SumCopies 10) "More Copies"
            , controlButton (SumCopies -10) "Less Copies"
            , controlButton (AdjustWidth 1) "Thicker"
            , controlButton (AdjustWidth -1) "Thinner"
            , controlButton GoToEditor "Open Editor"
            ]
        ]


inputWriting : Model -> Html Msg
inputWriting model =
    textarea
        [ id "WritingInput"
        , css
            [ width (px 200)
            , height (px 100)
            , backgroundColor (Colors.toCssColor model.image.backgroundColor)
            , color (Colors.toCssColor Colors.offWhite)
            , Css.fontSize (px 20)
            , Css.flexShrink zero
            , Css.fontFamily Css.monospace
            ]
        , onInput InputWriting
        ]
        [ text model.writing ]


controlButton : Msg -> String -> Html Msg
controlButton msg btnText =
    button
        [ onClick msg
        , css
            [ display inlineFlex ]
        ]
        [ text btnText ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, ExternalMsg )
update msg model =
    case msg of
        InputWriting string ->
            let
                image =
                    Image.withComposition (stringToComposition string model.copies) model.image
            in
            ( { model
                | writing = string
                , image = image
              }
            , Cmd.none
            , UpdateWriting
            )

        SumCopies num ->
            ( { model
                | copies = model.copies + num
                , image = Image.withComposition (stringToComposition model.writing (model.copies + num)) model.image
              }
            , Cmd.none
            , UpdateWriting
            )

        AdjustWidth direction ->
            ( { model
                | image = Image.withStrokeWidth (adjustWidth direction model.image.strokeWidth) model.image
              }
            , Cmd.none
            , UpdateWriting
            )

        ResetAngle ->
            ( { model
                | image = Image.withTurnAngle 90 model.image
                , videoAngleChangeRate = initialVideoAngleChangeRate
              }
            , Cmd.none
            , UpdateWriting
            )

        ToggleVideo ->
            ( { model | isPlaying = not model.isPlaying, videoAngleChangeRate = initialVideoAngleChangeRate }
            , Cmd.none
            , UpdateWriting
            )

        VideoTick ->
            let
                newAngle =
                    model.image.turnAngle + model.videoAngleChangeRate

                { h, s, v, a } =
                    Colors.toHsva model.image.strokeColor

                strokeColor =
                    Colors.hsva (h + 0.02) s v a
            in
            ( { model
                | image =
                    model.image
                        |> Image.withStrokeColor strokeColor
                        |> Image.withTurnAngle newAngle

                --|> Image.withStrokeWidth (min (model.image.strokeWidth * 1.01) 0.1)
                , videoAngleChangeRate = min (model.videoAngleChangeRate * 1.07) 0.03
              }
            , Cmd.none
            , UpdateWriting
            )

        GoToEditor ->
            ( model, Cmd.none, OpenedEditor model.image )

        NoOp ->
            ( model, Cmd.none, NothingToUpdate )

        TopBarMsg subMsg ->
            let
                ( updatedTopBar, cmd ) =
                    TopBar.update subMsg model.topBar
            in
            ( { model | topBar = updatedTopBar }, cmd, UpdateWriting )



-- MAKE COMPOSITION FROM INPUT


stringToComposition : String -> Int -> Composition
stringToComposition string numberOfCopies =
    let
        lineBlocks =
            string
                |> String.lines
                |> List.map lineToBlock

        goBackUp =
            List.repeat (List.length lineBlocks - 1) [ S, S ]
                |> List.concat

        goBackToStartingPoint =
            if List.length goBackUp > 0 then
                [ R, R, R ] ++ goBackUp ++ [ R ]

            else
                []

        writing =
            lineBlocks
                |> List.intersperse [ R, S, S, R, R, R ]
                |> List.concat
                -- This last D is to allow single letter art:
                -- with it we can have an intermediate block that is DRRSRR which will make a single letter rotate.
                |> (\l -> l ++ goBackToStartingPoint ++ [ S ])
    in
    LCore.appendBlock writing (baseWritingComposition numberOfCopies)


lineToBlock : String -> Block
lineToBlock string =
    let
        wordBlocks =
            string
                |> String.split " "
                |> List.map wordToBlock

        line =
            List.concat (List.intersperse [ S ] wordBlocks)

        goBack =
            [ R, R ] ++ List.repeat (List.length line) S ++ [ R, R ]
    in
    line ++ goBack


wordToBlock : String -> Block
wordToBlock =
    String.toList >> List.map Glyph


baseWritingComposition : Int -> Composition
baseWritingComposition numberOfCopies =
    LCore.fromList
        [ List.repeat numberOfCopies D
        , [ D, R, R, S, R, R ]
        ]



-- USER EDITS


adjustWidth : Float -> Float -> Float
adjustWidth direction width =
    (direction / 2 + 1) * width



-- SUBSCRIPTIONS


framesInterval : Float
framesInterval =
    100


subscriptions : Model -> Bool -> Sub Msg
subscriptions model isVisible =
    let
        videoSub =
            if model.isPlaying then
                Time.every framesInterval (always VideoTick)

            else
                Sub.none
    in
    if isVisible && model.isPlaying then
        videoSub

    else
        Sub.none


initialCmd : Cmd Msg
initialCmd =
    Task.attempt (always NoOp) (Browser.Dom.focus "WritingInput")
