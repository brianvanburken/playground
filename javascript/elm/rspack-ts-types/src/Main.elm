port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


port saveCount : Int -> Cmd msg


type alias Flags =
    { count : Int }


type alias Model =
    { count : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { count = flags.count }, Cmd.none )


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Increment ->
                    { model | count = model.count + 1 }

                Decrement ->
                    { model | count = model.count - 1 }
    in
    ( newModel, saveCount newModel.count )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
