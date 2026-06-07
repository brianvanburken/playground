module Pages.NotFound exposing (Model, Msg, init, update, view, subscriptions)

import Browser exposing (Document)
import Html exposing (h1, text)


type alias Model =
    {}


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Document Msg
view _ =
    { title = "404 - Not Found"
    , body = [ h1 [] [ text "404 - Not Found" ] ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
