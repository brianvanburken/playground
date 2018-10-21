port module Main exposing (main)

import Browser
import Cmd.Extra exposing (with)
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Http
import Parser exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import String


type alias Timestamp =
    { minutes : Int, seconds : Int, milliseconds : Int }


type alias Lyric =
    { startTime : Timestamp, text : String }


type alias Model =
    { lyrics : List Lyric
    , rawLyrics : WebData String
    , currentTime : Float
    }


port playFromTimestamp : Float -> Cmd msg


port currentTimestamp : (Float -> msg) -> Sub msg


port initSubscription : () -> Cmd msg


fetchLyrics : Cmd Msg
fetchLyrics =
    Http.getString "/assets/rick.txt"
        |> RemoteData.sendRequest
        |> Cmd.map FetchLyrics


type Msg
    = FetchLyrics (WebData String)
    | PlayFromTimestamp Float
    | CurrentTimestamp Float


lyricsParser : Parser (List Lyric)
lyricsParser =
    loop [] step


step : List Lyric -> Parser (Step (List Lyric) (List Lyric))
step entries =
    let
        finish entry_ next =
            next (entry_ :: entries)
    in
    succeed finish
        |= lyric
        |= oneOf
            [ succeed Loop
                |. symbol "\n"
            , succeed (Done << List.reverse)
            , problem "I was expecting a list with newlines but got nothing"
            ]


lyric : Parser Lyric
lyric =
    succeed Lyric
        |= timestamp
        |= text


timestamp : Parser Timestamp
timestamp =
    succeed Timestamp
        |. symbol "["
        |= digits
        |. symbol ":"
        |= digits
        |. symbol "."
        |= digits
        |. symbol "]"


text : Parser String
text =
    oneOrMore (not << isNewLine)


zeroOrMore : (Char -> Bool) -> Parser String
zeroOrMore isOk =
    succeed ()
        |. chompWhile isOk
        |> getChompedString


oneOrMore : (Char -> Bool) -> Parser String
oneOrMore isOk =
    succeed ()
        |. chompIf isOk
        |. chompWhile isOk
        |> getChompedString


isNewLine : Char -> Bool
isNewLine =
    (==) '\n'


digits : Parser Int
digits =
    let
        stringToInt =
            String.toInt
                >> Maybe.withDefault 0
                >> succeed
    in
    getChompedString (chompWhile Char.isDigit)
        |> andThen stringToInt


parseLyrics : String -> List Lyric
parseLyrics =
    Parser.run lyricsParser
        >> Result.withDefault []


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lyrics = [], rawLyrics = NotAsked, currentTime = 0.0 }, fetchLyrics )


view : Model -> Html Msg
view model =
    case model.rawLyrics of
        NotAsked ->
            Html.text "Initialising."

        Loading ->
            Html.text "Loading."

        Failure err ->
            Html.text "Error: could not download lyrics."

        Success _ ->
            Html.div []
                [ viewMp3
                , Html.div [] (List.map (viewLyric model.currentTime) model.lyrics)
                ]


viewMp3 : Html Msg
viewMp3 =
    Html.div []
        [ Html.audio [ Attribute.controls True, Attribute.autoplay False ]
            [ Html.source
                [ Attribute.src "/assets/rick.mp3"
                , Attribute.type_ "audio/mpeg"
                ]
                []
            ]
        ]


getTime : Timestamp -> Float
getTime { minutes, seconds } =
    toFloat ((minutes * 60) + seconds)


viewLyric : Float -> Lyric -> Html Msg
viewLyric currentTime line =
    let
        lineTime : Float
        lineTime =
            getTime line.startTime

        cssClass : String
        cssClass =
            if currentTime > lineTime then
                "pointer gray"

            else
                "pointer black"

        content : List (Html Msg)
        content =
            [ Html.text (viewTimestamp line.startTime)
            , Html.text " "
            , Html.text line.text
            ]
    in
    Html.p
        [ Attribute.class cssClass
        , Event.onClick (PlayFromTimestamp lineTime)
        ]
        content


viewTimestamp : Timestamp -> String
viewTimestamp { minutes, seconds, milliseconds } =
    let
        timeNotation : Int -> String
        timeNotation =
            String.fromInt
                >> String.padLeft 2 '0'
    in
    "["
        ++ timeNotation minutes
        ++ ":"
        ++ timeNotation seconds
        ++ "."
        ++ timeNotation milliseconds
        ++ "]"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchLyrics response ->
            case response of
                Success rawLyrics ->
                    let
                        lyrics =
                            rawLyrics
                                |> parseLyrics
                    in
                    model
                        |> updateLyrics lyrics
                        |> updateRawLyrics response
                        |> with (initSubscription ())

                _ ->
                    model
                        |> updateRawLyrics response
                        |> with Cmd.none

        CurrentTimestamp time ->
            model
                |> updateCurrentTime time
                |> with Cmd.none

        PlayFromTimestamp time ->
            model
                |> updateCurrentTime time
                |> with (playFromTimestamp time)


updateLyrics : List Lyric -> Model -> Model
updateLyrics lyrics model =
    { model | lyrics = lyrics }


updateRawLyrics : WebData String -> Model -> Model
updateRawLyrics data model =
    { model | rawLyrics = data }


updateCurrentTime : Float -> Model -> Model
updateCurrentTime time model =
    { model | currentTime = time }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ currentTimestamp CurrentTimestamp
        ]
