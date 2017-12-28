module Controller exposing (..)

import Html exposing (Html)
import Keyboard exposing (KeyCode)
import Collage exposing (collage)
import Element exposing (toHtml)
import Tetromino exposing (Tetromino)
import Time exposing (Time, second)
import Board exposing (Board)
import Random exposing (Generator)


type alias Coordinates =
    ( Int, Int )


type Input
    = Rotate
    | Shift Coordinates


type Direction
    = Up
    | Down
    | Left
    | Right
    | None


type alias Model =
    { falling : Tetromino
    , bag : List Tetromino
    , board : Board
    }


defaultModel : Model
defaultModel =
    { falling = Tetromino.shift startingShift Tetromino.j
    , bag = []
    , board = Board.new []
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Random.generate ShuffleBag Tetromino.zeroToOne )


type Msg
    = PressDown KeyCode
    | Tick Time
    | ShuffleBag Float


keyToDirection : Int -> Direction
keyToDirection key =
    case key of
        37 ->
            Left

        38 ->
            Up

        39 ->
            Right

        40 ->
            Down

        _ ->
            None


startingShift : ( Int, Int )
startingShift =
    ( 20, 5 )


useIfValid : Model -> Model -> Model
useIfValid current new =
    if Board.isValid new.falling new.board then
        new
    else
        current


tryKicks : List ( Int, Int ) -> Model -> Model -> Model
tryKicks shifts currentModel nextModel =
    case shifts of
        [] ->
            currentModel

        head :: tail ->
            let
                shifted =
                    Tetromino.shift head nextModel.falling
            in
                if Board.isValid shifted nextModel.board then
                    { nextModel | falling = shifted }
                else
                    tryKicks tail currentModel nextModel


wallKick : Model -> Model -> Model
wallKick current next =
    let
        limit =
            next.falling.cols

        range =
            List.range 1 limit

        shifts =
            List.concatMap (\n -> [ ( 0, n ), ( 0, -n ) ]) range
    in
        tryKicks shifts current next


floorKick : Model -> Model -> Model
floorKick current next =
    let
        limit =
            next.falling.rows

        range =
            List.range 1 limit

        shifts =
            List.map (\n -> ( n, 0 )) range
    in
        tryKicks shifts current next


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        applyIfValid =
            useIfValid model
    in
        case msg of
            ShuffleBag n ->
                ( model, Cmd.none )

            Tick _ ->
                let
                    falling =
                        Tetromino.shift ( -1, 0 ) model.falling

                    new =
                        applyIfValid { model | falling = falling }
                in
                    ( new, Cmd.none )

            PressDown k ->
                let
                    direction =
                        keyToDirection k

                    falling =
                        case direction of
                            Left ->
                                Tetromino.shift ( 0, -1 ) model.falling

                            Up ->
                                Tetromino.rotate model.falling

                            Right ->
                                Tetromino.shift ( 0, 1 ) model.falling

                            Down ->
                                Tetromino.shift ( -1, 0 ) model.falling

                            None ->
                                model.falling

                    new =
                        applyIfValid { model | falling = falling }

                    next =
                        if (direction == Up && new == model) then
                            wallKick model new
                        else
                            new

                    next2 =
                        if (direction == Up && new == model) then
                            floorKick model next
                        else
                            next
                in
                    ( next2, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.downs PressDown
        , Time.every second Tick
        ]


view : Model -> Html Msg
view model =
    let
        screenWidth =
            800

        screenHeight =
            600

        boardForm =
            Board.addTetromino model.falling model.board |> Board.toForm
    in
        toHtml (collage screenWidth screenHeight [ boardForm ])


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
