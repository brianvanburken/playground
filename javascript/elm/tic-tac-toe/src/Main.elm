module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (placeholder)

type Msg = NoOp | Choose Player Int

type Player = PlayerX | PlayerO

type alias Board = List (Maybe Player)

type alias Model = { currentPlayer: Player, board: Board }

emptyBoard: Board
-- emptyBoard = [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,Nothing, Nothing, Nothing]
emptyBoard = [ Nothing, Just PlayerO, Nothing, Nothing, Just PlayerX, Nothing,Nothing, Nothing, Nothing]

init: () -> ( Model, Cmd Msg )
init _ =
    ( { currentPlayer = PlayerO, board = emptyBoard }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        _ -> (model, Cmd.none)


view : Model -> Html Msg
view model = 
    Html.div [] (List.map viewCell model.board)


viewCell : Maybe Player -> Html Msg
viewCell player =
    player
        |> Maybe.map viewPlayer
        |> Maybe.withDefault "-"
        |> Html.text

viewPlayer : Player -> String
viewPlayer player =
    case player of
        PlayerO -> "O"
        PlayerX -> "X"

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
