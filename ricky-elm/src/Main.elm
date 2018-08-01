module Main exposing (main)

import Char
import Html exposing (Html)
import Http
import Parser exposing ((|.), (|=), Count(..), Parser, int, keep, oneOrMore, succeed, symbol)
import RemoteData exposing (RemoteData(..), WebData)


type alias Line =
    { hour : String, minute : String, seconds : String, text : String }


type alias Model =
    { lyrics : List Line
    , rawLyrics : WebData String
    }


fetchLyrics : Cmd Msg
fetchLyrics =
    Http.getString "/assets/lyrics.txt"
        |> RemoteData.sendRequest
        |> Cmd.map FetchLyrics


type Msg
    = FetchLyrics (WebData String)


parseLine : Parser Line
parseLine =
    succeed Line
        |. symbol "["
        |= keep oneOrMore Char.isDigit
        |. symbol ":"
        |= keep oneOrMore Char.isDigit
        |. symbol "."
        |= keep oneOrMore Char.isDigit
        |. symbol "]"
        |= keep oneOrMore (\_ -> True)


parseLyrics : String -> List Line
parseLyrics rawLyrics =
    let
        unpackResult : Result Parser.Error Line -> Line
        unpackResult res =
            case res of
                Ok line ->
                    line

                Err error ->
                    Line "0" "0" "0" (toString error)

        lines =
            rawLyrics
                |> String.split "\n"
                |> List.map (Parser.run parseLine)
                |> List.map unpackResult
    in
    lines


init : ( Model, Cmd Msg )
init =
    ( { lyrics = [], rawLyrics = NotAsked }, fetchLyrics )


view : Model -> Html Msg
view model =
    case model.rawLyrics of
        NotAsked ->
            Html.text "Initialising."

        Loading ->
            Html.text "Loading."

        Failure err ->
            Html.text ("Error: " ++ toString err)

        Success _ ->
            Html.div []
                (List.map viewLine model.lyrics)


viewLine : Line -> Html Msg
viewLine line =
    Html.text (line.hour ++ ":" ++ line.minute ++ ":" ++ line.seconds ++ " = " ++ line.text)
        |> List.singleton
        |> Html.p []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchLyrics response ->
            case response of
                Success rawLyrics ->
                    let
                        lyrics =
                            parseLyrics rawLyrics
                    in
                    ( { model | lyrics = lyrics, rawLyrics = response }, Cmd.none )

                _ ->
                    ( { model | rawLyrics = response }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
