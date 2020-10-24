module Main exposing (main)

import Browser
import Html exposing (Html)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = always (Html.text "Hello world!")
        }


type Msg
    = NoOp
    | NoOp2


type alias Model =
    ()


init : () -> ( Model, Cmd Msg )
init _ =
    withoutCmd ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                |> withoutCmd

        NoOp2 ->
            model
                |> with Cmd.none
                |> add Cmd.none


withoutCmd : model -> ( model, Cmd msg )
withoutCmd model =
    ( model, Cmd.none )


{-| Add Cmd to model to create a pair.
-}
with : Cmd msg -> model -> ( model, Cmd msg )
with cmd model =
    ( model, cmd )


{-| Add new cmd to an existing pair.
-}
add : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
add newCmd ( model, prevCmd ) =
    ( model, Cmd.batch [ newCmd, prevCmd ] )
