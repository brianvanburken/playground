module Greeting exposing (main)

import Browser
import Html exposing (Html, text)


type alias Flags =
    { name : String }


type alias Model =
    { name : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { name = flags.name }, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text ("Hello, " ++ model.name ++ "!")


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
