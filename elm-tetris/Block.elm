module Block exposing (..)

import Collage exposing (collage, square, solid, outlined, group, filled, Form)
import Element exposing (..)
import Color exposing (Color)
import Html exposing (Html)


type alias Block =
    { color : Color
    }


type alias Model =
    {}


type Msg
    = NoOp


size : Float
size =
    25


model : Model
model =
    {}


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


toForm : Block -> Form
toForm block =
    let
        shape =
            square size

        border =
            outlined (solid Color.black) shape
    in
        group [ filled block.color shape, border ]


view : Model -> Html Msg
view model =
    toHtml (collage 400 400 [ toForm (Block Color.blue) ])


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
