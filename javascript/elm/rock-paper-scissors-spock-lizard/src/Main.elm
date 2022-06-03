module Main exposing (main)

import Browser
import Html
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)
import Random


type alias Model =
    { playerMove : Maybe Move
    , opponentMove : Maybe Move
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playerMove = Nothing, opponentMove = Nothing }, Cmd.none )


type Msg
    = ChooseMove Move
    | GenerateMove
    | NewMove Move


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


outcomeToString: Outcome -> String
outcomeToString outcome =
    case outcome of
        Win -> "Win"
        Lose -> "Lose"
        Tie -> "Tie"

indexToMove : Int -> Move
indexToMove idx =
    case idx of
        1 ->
            Rock

        2 ->
            Paper

        3 ->
            Scissors

        4 ->
            Spock

        5 ->
            Lizard

        _ ->
            Rock


explainMoves : Maybe Move -> Maybe Move -> String
explainMoves playerMove opponentMove =
    case ( playerMove, opponentMove ) of
        ( Just Scissors, Just Paper ) ->
            "scissors cuts paper"

        ( Just Paper, Just Rock ) ->
            "paper covers rock"

        ( Just Rock, Just Lizard ) ->
            "rock crushes lizard"

        ( Just Lizard, Just Spock ) ->
            "lizard poisons Spock"

        ( Just Spock, Just Scissors ) ->
            "Spock smashes scissors"

        ( Just Scissors, Just Lizard ) ->
            "scissors decapitates lizard"

        ( Just Lizard, Just Paper ) ->
            "lizard eats paper"

        ( Just Paper, Just Spock ) ->
            "paper disproves Spock"

        ( Just Spock, Just Rock ) ->
            "Spock vaporizes rock"

        ( Just Rock, Just Scissors ) ->
            "rock crushes scissors"

        ( _, _ ) ->
            ""


moveToString : Move -> String
moveToString move =
    case move of
        Rock -> "Rock"
        Paper -> "Paper"
        Scissors -> "Scissors"
        Spock -> "Spock"
        Lizard -> "Lizard"


resultToString : Maybe Move -> Maybe Move -> String
resultToString move1 move2 =
    case ( move1, move2 ) of
        ( Nothing, _ ) ->
            "-"

        ( _, Nothing ) ->
            "-"

        ( Just playerMove, Just opponentMove ) ->
            result playerMove opponentMove
                |> outcomeToString
                |> String.toLower


generateMove : Random.Generator Move
generateMove =
    Random.map indexToMove (Random.int 1 5)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseMove move ->
            { model | playerMove = Just move } |> update GenerateMove

        GenerateMove ->
            ( model, Random.generate NewMove generateMove )

        NewMove move ->
            ( { model | opponentMove = Just move }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ viewMoveChooser Rock
            , viewMoveChooser Paper
            , viewMoveChooser Scissors
            , viewMoveChooser Spock
            , viewMoveChooser Lizard
            ]
        , br [] []
        , text ("You choose: " ++ (Maybe.map moveToString model.playerMove |> Maybe.withDefault "-"))
        , br [] []
        , text ("Opponent chooses: " ++ (Maybe.map moveToString model.opponentMove |> Maybe.withDefault "-" ))
        , br [] []
        , text ("Outcome: " ++ resultToString model.playerMove model.opponentMove)
        , br [] []
        , text
            ("Explanation: "
                ++ explainMoves model.playerMove model.opponentMove
                ++ explainMoves model.opponentMove model.playerMove
            )
        ]


viewMoveChooser : Move -> Html Msg
viewMoveChooser move =
    button
        [ onClick (ChooseMove move) ]
        [ text (moveToString move) ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
