module Controller exposing (..)

import Html exposing (Html, text, div)
import Keyboard exposing (presses, KeyCode)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "", Cmd.none )


type Msg
    = PressDown KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressDown k ->
            let
                code =
                    if (k == 37) then
                        "Left"
                    else if (k == 38) then
                        "Up"
                    else if (k == 39) then
                        "Right"
                    else if (k == 40) then
                        "Down"
                    else
                        "Not an arrow"
            in
                ( code, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs PressDown


view : Model -> Html Msg
view model =
    let
        str =
            if model == "" then
                "Press a key"
            else
                "You pressed: " ++ (toString model)
    in
        Html.text str
