module Pages.Video exposing (ExternalMsg(..), Model, Msg, subscriptions, update, view, welcomeModel)

import Browser
import Colors
import Css exposing (height, pct, width)
import Events exposing (onKeyDown)
import Html.Styled exposing (div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import LSystem.Draw exposing (drawImage)
import LSystem.Image as Image exposing (Image)
import Time


type alias Model =
    { image : Image
    , isPlaying : Bool
    , videoAngleChangeRate : Float
    }


welcomeModel : Model
welcomeModel =
    { image = Image.welcomeImage
    , isPlaying = True
    , videoAngleChangeRate = 0.0001
    }


type Msg
    = VideoTick
    | UserInteracted


type ExternalMsg
    = UpdateVideo
    | VideoEnded Image


view : Model -> Browser.Document Msg
view model =
    { title = "Welcome to Genart"
    , body =
        [ div
            [ css [ width (pct 100), height (pct 100) ]
            , onClick UserInteracted
            , onKeyDown (always UserInteracted)
            ]
            [ drawImage (Just "WelcomeVideo") Nothing False model.image ]
            |> toUnstyled
        ]
    }


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
                , videoAngleChangeRate = min (model.videoAngleChangeRate * 1.07) 0.03
              }
            , Cmd.none
            , UpdateVideo
            )

        UserInteracted ->
            ( model, Cmd.none, VideoEnded model.image )


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
    if isVisible then
        videoSub

    else
        Sub.none
