module Controller exposing (..)

import Html exposing (Html)
import Keyboard exposing (KeyCode)
import Collage exposing (..)
import Element exposing (toHtml)
import Tetromino exposing (Tetromino)


type alias Coordinates =
    ( Int, Int )


type Input
    = Rotate
    | Shift Coordinates


type Direction
    = Up
    | Down
    | Left
    | Right
    | None


type alias Model =
    { falling : Tetromino
    }


defaultModel : Model
defaultModel =
    { falling = Tetromino.z
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


type Msg
    = PressDown KeyCode


keyToDirection : Int -> Direction
keyToDirection key =
    case key of
        37 ->
            Left

        38 ->
            Up

        39 ->
            Right

        40 ->
            Down

        _ ->
            None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressDown k ->
            let
                direction =
                    keyToDirection k

                falling =
                    case direction of
                        Left ->
                            Tetromino.shift ( 0, -1 ) model.falling

                        Up ->
                            Tetromino.rotate model.falling

                        Right ->
                            Tetromino.shift ( 0, 1 ) model.falling

                        Down ->
                            Tetromino.shift ( -1, 0 ) model.falling

                        None ->
                            model.falling
            in
                ( { model | falling = falling }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs PressDown


view : Model -> Html Msg
view model =
    let
        screenWidth =
            800

        screenHeight =
            600

        fallingForm =
            Tetromino.toForm model.falling
    in
        toHtml (collage screenWidth screenHeight [ fallingForm ])


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
