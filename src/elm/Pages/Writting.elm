module Pages.Writting exposing (ExternalMsg(..), Model, Msg, initialModel, subscriptions, update, view)

import Browser
import Colors exposing (Color)
import Components as C
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
import Time



-- MODEL


type alias Model =
    { image : Image
    , isPlaying : Bool
    , videoAngleChangeRate : Float
    , writting : String
    , copies : Int
    }


initialWritting : String
initialWritting =
    "Welcome\n  to\nGenart"


initialVideoAngleChangeRate : Float
initialVideoAngleChangeRate =
    0.001


initialNumberOfCopies : Int
initialNumberOfCopies =
    20


initialImage : Image
initialImage =
    Image.welcomeImage
        |> Image.withComposition (stringToComposition initialWritting initialNumberOfCopies)
        |> Image.withStrokeWidth 0.01


initialModel : Model
initialModel =
    { image = initialImage
    , isPlaying = True
    , videoAngleChangeRate = initialVideoAngleChangeRate
    , writting = initialWritting
    , copies = initialNumberOfCopies
    }



-- MSG


type Msg
    = VideoTick
    | InputWritting String
    | GoToEditor
    | SumCopies Int
    | AdjustWidth Float
    | ToggleVideo


type ExternalMsg
    = UpdateWritting
    | OpenedEditor Image



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Writting to Genart"
    , body =
        [ div
            [ css
                [ width (pct 100)
                , height (pct 100)
                , overflow hidden
                ]
            ]
            [ drawImage (Just "WrittingVideo") Nothing False model.image
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
        [ inputWritting model
        , div [ css [] ]
            [ controlButton (SumCopies 1) "More Copies"
            , controlButton (SumCopies -1) "Less Copies"
            , controlButton GoToEditor "Open Editor"
            , controlButton ToggleVideo "Play/Pause"
            , controlButton (AdjustWidth 1) "Thicker"
            , controlButton (AdjustWidth -1) "Thinner"
            ]
        ]


inputWritting : Model -> Html Msg
inputWritting model =
    textarea
        [ id "WrittingInput"
        , css
            [ width (px 200)
            , height (px 100)
            , backgroundColor (Colors.toCssColor model.image.backgroundColor)
            , color (Colors.toCssColor Colors.offWhite)
            , Css.fontSize (px 20)
            ]
        , onInput InputWritting
        ]
        [ text model.writting ]


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
        InputWritting string ->
            let
                image =
                    Image.withComposition (stringToComposition string model.copies) model.image
                        |> Image.withTurnAngle 90
            in
            ( { model
                | writting = string
                , image = image
                , videoAngleChangeRate = initialVideoAngleChangeRate
              }
            , Cmd.none
            , UpdateWritting
            )

        SumCopies num ->
            ( { model
                | copies = model.copies + num
                , image = Image.withComposition (stringToComposition model.writting (model.copies + num)) model.image
              }
            , Cmd.none
            , UpdateWritting
            )

        AdjustWidth direction ->
            ( { model
                | image = Image.withStrokeWidth (adjustWidth direction model.image.strokeWidth) model.image
              }
            , Cmd.none
            , UpdateWritting
            )

        ToggleVideo ->
            ( { model | isPlaying = not model.isPlaying, videoAngleChangeRate = initialVideoAngleChangeRate }
            , Cmd.none
            , UpdateWritting
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
            , UpdateWritting
            )

        GoToEditor ->
            ( model, Cmd.none, OpenedEditor model.image )



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

        writting =
            lineBlocks
                |> List.intersperse [ R, S, S, R, R, R ]
                |> List.concat
                |> (\l -> l ++ goBackToStartingPoint)
    in
    LCore.appendBlock writting (baseWrittingComposition numberOfCopies)


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


baseWrittingComposition : Int -> Composition
baseWrittingComposition numberOfCopies =
    LCore.fromList [ List.repeat numberOfCopies D ]



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
