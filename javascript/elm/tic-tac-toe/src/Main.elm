module Main exposing (..)

import Browser
import Html exposing (Html)

type Msg = NoOp

type alias Model = {}

init: () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)


view : Model -> Html Msg
view _ = Html.text "Test"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
