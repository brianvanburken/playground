module Pages.About exposing (Model, Msg, init, subscriptions, title, update, view)

import Browser exposing (Document)
import Html exposing (Html, h1, text)


type alias Model =
    {}


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


title : String
title =
    "About"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> List (Html Msg)
view _ =
    [ h1 [] [ text "About" ] ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
