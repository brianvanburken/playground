module Controller exposing (..)

import Html exposing (Html, text, div)
import Keyboard exposing (presses, KeyCode)
import Collage exposing (..)
import Element exposing (Element, toHtml)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressDown k ->
            let
                falling =
                    case k of
                        37 ->
                            -- Left
                            Tetromino.shift ( 0, -1 ) model.falling

                        38 ->
                            -- Up
                            Tetromino.rotate model.falling

                        39 ->
                            -- Right
                            Tetromino.shift ( 0, 1 ) model.falling

                        40 ->
                            -- Down
                            Tetromino.shift ( -1, 0 ) model.falling

                        _ ->
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
