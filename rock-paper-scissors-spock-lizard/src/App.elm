module App exposing (..)

import Html exposing (Html, div, text, button, br)
import Html.Events exposing (onClick)


type alias Model =
    { playerMove : Maybe Move
    , opponentMove : Maybe Move
    }


init : ( Model, Cmd Msg )
init =
    ( { playerMove = Nothing, opponentMove = Nothing }, Cmd.none )


type Msg
    = ChooseMove Move


type Move
    = Rock
    | Paper
    | Scissors
    | Spock
    | Lizard


type Outcome
    = Win
    | Lose
    | Tie


result : Move -> Move -> Outcome
result move1 move2 =
    case ( move1, move2 ) of
        ( Paper, Rock ) ->
            Win

        ( Paper, Spock ) ->
            Win

        ( Paper, Lizard ) ->
            Lose

        ( Paper, Scissors ) ->
            Lose

        ( Rock, Scissors ) ->
            Win

        ( Rock, Lizard ) ->
            Win

        ( Rock, Paper ) ->
            Lose

        ( Rock, Spock ) ->
            Lose

        ( Scissors, Paper ) ->
            Win

        ( Scissors, Lizard ) ->
            Win

        ( Scissors, Rock ) ->
            Lose

        ( Scissors, Spock ) ->
            Lose

        ( Spock, Scissors ) ->
            Win

        ( Spock, Rock ) ->
            Win

        ( Spock, Paper ) ->
            Lose

        ( Spock, Lizard ) ->
            Lose

        ( Lizard, Spock ) ->
            Win

        ( Lizard, Paper ) ->
            Win

        ( Lizard, Rock ) ->
            Lose

        ( Lizard, Scissors ) ->
            Lose

        ( _, _ ) ->
            Tie


moveToString : Maybe Move -> String
moveToString move =
    case move of
        Just move ->
            toString move
                |> String.toLower

        Nothing ->
            "nothing yet"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseMove move ->
            ( { model | playerMove = Just move }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (ChooseMove Rock) ] [ text "Rock" ]
        , button [ onClick (ChooseMove Paper) ] [ text "Paper" ]
        , button [ onClick (ChooseMove Scissors) ] [ text "Scissors" ]
        , button [ onClick (ChooseMove Spock) ] [ text "Spock" ]
        , button [ onClick (ChooseMove Lizard) ] [ text "Lizard" ]
        , br [] []
        , text ("You've choosen " ++ (moveToString model.playerMove))
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
