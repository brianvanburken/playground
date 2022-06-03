module Main exposing (main)

import Browser
import Html exposing (Html, em)
import Html.Attributes as Attr
import Html.Attributes.Extra as Attr
import Html.Events as Event
import List.Extra


type Msg
    = NoOp
    | Choose Int
    | Reset


type Player
    = X
    | O


type alias Cell =
    Maybe Player


type alias Board =
    List Cell


type GameState
    = InProgress
    | WonBy Player
    | Draw


type alias Model =
    { gameState : GameState, currentPlayer : Player, board : Board }


emptyBoard : Board
emptyBoard =
    List.repeat 9 Nothing


emptyGame : Model
emptyGame =
    { gameState = InProgress, currentPlayer = X, board = emptyBoard }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyGame, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Choose index ->
            let
                newBoard =
                    setChoiceBoard model.currentPlayer index model.board

                newState =
                    nextGameState newBoard
            in
            ( { model
                | currentPlayer = nextPlayer model.currentPlayer
                , gameState = newState
                , board = newBoard
              }
            , Cmd.none
            )

        Reset ->
            ( emptyGame, Cmd.none )


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        O ->
            X

        X ->
            O


setChoiceBoard : Player -> Int -> Board -> Board
setChoiceBoard player index =
    List.Extra.setAt index (Just player)


nextGameState : Board -> GameState
nextGameState board =
    case board of
        -- Top row
        (Just O) :: (Just O) :: (Just O) :: _ :: _ :: _ :: _ :: _ :: _ :: [] ->
            WonBy O

        (Just X) :: (Just X) :: (Just X) :: _ :: _ :: _ :: _ :: _ :: _ :: [] ->
            WonBy X

        -- Middle row
        _ :: _ :: _ :: (Just O) :: (Just O) :: (Just O) :: _ :: _ :: _ :: [] ->
            WonBy O

        _ :: _ :: _ :: (Just X) :: (Just X) :: (Just X) :: _ :: _ :: _ :: [] ->
            WonBy X

        -- Bottom row
        _ :: _ :: _ :: _ :: _ :: _ :: (Just O) :: (Just O) :: (Just O) :: [] ->
            WonBy O

        _ :: _ :: _ :: _ :: _ :: _ :: (Just X) :: (Just X) :: (Just X) :: [] ->
            WonBy X

        -- Left column
        (Just O) :: _ :: _ :: (Just O) :: _ :: _ :: (Just O) :: _ :: _ :: [] ->
            WonBy O

        (Just X) :: _ :: _ :: (Just X) :: _ :: _ :: (Just X) :: _ :: _ :: [] ->
            WonBy X

        -- Middle column
        _ :: (Just O) :: _ :: _ :: (Just O) :: _ :: _ :: (Just O) :: _ :: [] ->
            WonBy O

        _ :: (Just X) :: _ :: _ :: (Just X) :: _ :: _ :: (Just X) :: _ :: [] ->
            WonBy X

        -- Right column
        _ :: _ :: (Just O) :: _ :: _ :: (Just O) :: _ :: _ :: (Just O) :: [] ->
            WonBy O

        _ :: _ :: (Just X) :: _ :: _ :: (Just X) :: _ :: _ :: (Just X) :: [] ->
            WonBy X

        -- Diagonal \
        (Just O) :: _ :: _ :: _ :: (Just O) :: _ :: _ :: _ :: (Just O) :: [] ->
            WonBy O

        (Just X) :: _ :: _ :: _ :: (Just X) :: _ :: _ :: _ :: (Just X) :: [] ->
            WonBy X

        -- Diagonal /
        _ :: _ :: (Just O) :: _ :: (Just O) :: _ :: (Just O) :: _ :: _ :: [] ->
            WonBy O

        _ :: _ :: (Just X) :: _ :: (Just X) :: _ :: (Just X) :: _ :: _ :: [] ->
            WonBy X

        -- Draw
        (Just _) :: (Just _) :: (Just _) :: (Just _) :: (Just _) :: (Just _) :: (Just _) :: (Just _) :: (Just _) :: [] ->
            Draw

        _ ->
            InProgress


view : Model -> Html Msg
view model =
    case model.gameState of
        InProgress ->
            viewBoard model.board

        WonBy player ->
            Html.div []
                [ Html.text ("Game won by: " ++ viewPlayer player)
                , viewReset
                ]

        Draw ->
            Html.div []
                [ Html.text "Game ended in a draw"
                , viewReset
                ]


viewReset : Html Msg
viewReset =
    Html.button [ Event.onClick Reset ] [ Html.text "New game" ]


type alias IndexedCell =
    ( Int, Cell )


viewBoard : Board -> Html Msg
viewBoard =
    List.indexedMap (\i c -> ( i, c ))
        >> List.Extra.groupsOf 3
        >> List.map viewRow
        >> Html.table [ Attr.attribute "border" "1" ]


viewRow : List IndexedCell -> Html Msg
viewRow =
    List.map viewCell
        >> Html.tr []


viewCell : IndexedCell -> Html Msg
viewCell ( index, cell ) =
    cell
        |> Maybe.map viewPlayer
        >> Maybe.withDefault "-"
        >> Html.text
        >> List.singleton
        >> Html.button
            [ cell
                |> Maybe.map (\_ -> Attr.empty)
                |> Maybe.withDefault (Event.onClick (Choose index))
            ]
        >> List.singleton
        >> Html.td []


viewPlayer : Player -> String
viewPlayer player =
    case player of
        O ->
            "O"

        X ->
            "X"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
