module Pages.DevPlayground exposing (ExternalMsg(..), Model, Msg, initialCmd, initialModel, subscriptions, update, view)

import Browser
import Colors exposing (Color)
import Components as C
import Css
    exposing
        ( backgroundColor
        , borderColor
        , bottom
        , calc
        , color
        , fixed
        , height
        , hidden
        , hover
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
import Html.Styled exposing (Html, div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import LSystem.Core as Core exposing (Step(..))
import LSystem.Draw exposing (drawImage)
import LSystem.Image as Image exposing (Image, withComposition)
import Time
import Utils exposing (delay)


type alias Model =
    { image : Image
    , isPlaying : Bool
    , videoAngleChangeRate : Float
    , showEnterButton : Bool
    }


initialImage : Image
initialImage =
    Image.welcomeImage
        |> withComposition
            (Core.fromList
                -- [ [ D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D ]
                [ [ D, D, D, D, D, D, D ]
                , [ D, Core.L, Core.L, Core.S, Core.L, Core.L, D, Core.L, Core.L, Core.S, Core.L, Core.L, D, Core.L, Core.L, Core.S, Core.L, Core.L ]
                , [ Glyph 'A'
                  , Glyph 'B'
                  , Glyph 'C'
                  , R
                  , R
                  , Core.S
                  , Core.S
                  , Core.S
                  , R
                  , R
                  , Core.S
                  ]
                ]
            )


initialModel : Model
initialModel =
    { image = initialImage
    , isPlaying = True
    , videoAngleChangeRate = 0.0001
    , showEnterButton = False
    }


type Msg
    = VideoTick
    | ShowEnterButton
    | ExitPage


type ExternalMsg
    = UpdateWelcome
    | GoToEditor Image


view : Model -> Browser.Document Msg
view model =
    let
        showEnterButton =
            if model.showEnterButton then
                [ enterButton model.image.strokeColor ]

            else
                []
    in
    { title = "Developer playground"
    , body =
        [ div
            [ css [ width (pct 100), height (pct 100), overflow hidden ]

            -- , onClick ShowEnterButton
            ]
            (drawImage (Just "WelcomeVideo") False model.image
                :: showEnterButton
            )
            |> toUnstyled
        ]
    }


enterButton : Color -> Html Msg
enterButton strokeColor =
    let
        buttonColor =
            strokeColor
                |> Colors.updateAlpha 0.5
                |> Colors.toCssColor
    in
    C.primaryButtonStyled
        [ position fixed
        , bottom (px 40)
        , margin2 zero (calc (pct 50) minus (px 100))
        , width (px 200)
        , height (px 50)
        , padding (px 10)
        , color buttonColor
        , borderColor buttonColor
        , hover
            [ backgroundColor buttonColor
            , color (Colors.toCssColor Colors.black)
            , borderColor (Colors.toCssColor Colors.black)
            ]
        ]
        ExitPage
        "Click here to begin"


update : Msg -> Model -> ( Model, Cmd Msg, ExternalMsg )
update msg model =
    case msg of
        VideoTick ->
            let
                newAngle =
                    model.image.turnAngle - model.videoAngleChangeRate

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
                        |> Image.withStrokeWidth (min (model.image.strokeWidth * 1.01) 0.1)
                , videoAngleChangeRate = min (model.videoAngleChangeRate * 1.17) 0.03
              }
            , Cmd.none
            , UpdateWelcome
            )

        ShowEnterButton ->
            ( { model | showEnterButton = True }, Cmd.none, UpdateWelcome )

        ExitPage ->
            ( model, Cmd.none, GoToEditor model.image )


framesInterval : Float
framesInterval =
    100


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        videoSub =
            if model.isPlaying then
                Time.every framesInterval (always VideoTick)

            else
                Sub.none
    in
    videoSub


initialCmd : Cmd Msg
initialCmd =
    delay 5000 ShowEnterButton
