module Main exposing (..)

import Block exposing (Block)
import Collage exposing (collage, square, solid, outlined, group, filled, move, Form)
import Element exposing (..)
import Html exposing (..)
import Color exposing (Color)


type alias Location =
    ( Int, Int )


type alias Tetromino =
    { shape : List Location, block : Block }


toForm : Tetromino -> Form
toForm { shape, block } =
    let
        form =
            Block.toForm block

        translate ( row, col ) =
            move ( (toFloat col) * Block.size, (toFloat row) * Block.size ) form

        forms =
            List.map translate shape
    in
        group forms


i : Tetromino
i =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -2, 0 )
        ]
    , block = Block Color.lightBlue
    }


j : Tetromino
j =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, -1 )
        , ( -1, 0 )
        ]
    , block = Block Color.blue
    }


tetromino =
    j


type alias Model =
    {}


model : Model
model =
    {}


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    toHtml (collage 400 400 [ toForm tetromino ])


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
