module Main exposing (..)

import Block exposing (Block)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Color exposing (Color)


type alias Location =
    ( Int, Int )


type alias Pivot =
    { r : Float, c : Float }


type alias Tetromino =
    { shape : List Location
    , block : Block
    , pivot : Pivot
    , rows : Int
    , cols : Int
    }


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
    , pivot = { r = -0.5, c = 0.5 }
    , rows = 4
    , cols = 1
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
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 3
    , cols = 2
    }


l : Tetromino
l =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -1, 1 )
        ]
    , block = Block Color.orange
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 3
    , cols = 2
    }


z : Tetromino
z =
    { shape =
        [ ( 1, -1 )
        , ( 1, 0 )
        , ( 0, 0 )
        , ( 0, 1 )
        ]
    , block = Block Color.red
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 3
    , cols = 2
    }


s : Tetromino
s =
    { shape =
        [ ( 0, 0 )
        , ( 0, 1 )
        , ( -1, -1 )
        , ( -1, 0 )
        ]
    , block = Block Color.green
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 3
    , cols = 2
    }


t : Tetromino
t =
    { shape =
        [ ( 0, -1 )
        , ( 0, 0 )
        , ( 0, 1 )
        , ( -1, 0 )
        ]
    , block = Block Color.purple
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 2
    , cols = 3
    }


o : Tetromino
o =
    { shape =
        [ ( 0, 0 )
        , ( 0, 1 )
        , ( -1, 0 )
        , ( -1, 1 )
        ]
    , block = Block Color.yellow
    , pivot = { r = -0.5, c = 0.5 }
    , rows = 2
    , cols = 2
    }


drawPivot : Tetromino -> Form
drawPivot { pivot } =
    let
        dot =
            circle 5 |> filled Color.black

        translate =
            move ( pivot.c * Block.size, pivot.r * Block.size )
    in
        translate dot


rotateLocation : Pivot -> Float -> Location -> Location
rotateLocation { r, c } angle ( row, col ) =
    let
        rowOrigin =
            (toFloat row) - r

        colOrigin =
            (toFloat col) - c

        ( s, c ) =
            ( sin (angle), cos (angle) )

        rowRotated =
            rowOrigin * c - colOrigin * s

        colRotated =
            rowOrigin * s + colOrigin * c
    in
        ( round <| rowRotated + r, round <| colRotated + c )


rotate : Tetromino -> Tetromino
rotate tetromino =
    let
        rotateHelper =
            rotateLocation tetromino.pivot (degrees 90)

        newShape =
            List.map rotateHelper tetromino.shape
    in
        { tetromino
            | shape = newShape
            , rows = tetromino.cols
            , cols = tetromino.rows
        }


shift : ( Int, Int ) -> Tetromino -> Tetromino
shift ( rows, cols ) tetromino =
    let
        shiftHelper ( row, col ) =
            ( row + rows, col + cols )

        newShape =
            List.map shiftHelper tetromino.shape

        pivot =
            { r = tetromino.pivot.r + (toFloat rows)
            , c = tetromino.pivot.c + (toFloat cols)
            }
    in
        { tetromino
            | shape = newShape
            , pivot = pivot
        }


tetromino =
    shift ( 1, 5 ) o


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
    toHtml (collage 400 400 [ toForm tetromino, drawPivot tetromino ])


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
