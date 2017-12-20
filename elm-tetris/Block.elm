module Block exposing (..)

import Collage exposing (square, solid, outlined, group, filled, Form)
import Color exposing (Color)


type alias Block =
    Color


size : Float
size =
    25


toForm : Block -> Form
toForm block =
    let
        shape =
            square size

        border =
            outlined (solid Color.black) shape
    in
        group [ filled block shape, border ]
