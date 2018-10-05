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


type alias Line =
    { minutes : Int, seconds : Int, milliseconds : Int, text : String }


type alias Model =
    { lyrics : List Line
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


parseLine : Parser Line
parseLine =
    succeed Line
        |. symbol "["
        |= digits
        |. symbol ":"
        |= digits
        |. symbol "."
        |= digits
        |. symbol "]"
        |= (getChompedString <| chompWhile anything)


anything : Char -> Bool
anything _ =
    True


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


parseLyrics : String -> List Line
parseLyrics rawLyrics =
    let
        unpackResult : Result (List DeadEnd) Line -> Line
        unpackResult res =
            case res of
                Ok line ->
                    line

                Err _ ->
                    Line 0 0 0 ""

        lines =
            rawLyrics
                |> String.split "\n"
                |> List.map (Parser.run parseLine)
                |> List.map unpackResult
    in
    lines


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
                , Html.div [] (List.map (viewLine model.currentTime) model.lyrics)
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


getTime : Line -> Float
getTime line =
    toFloat ((line.minutes * 60) + line.seconds)


viewLine : Float -> Line -> Html Msg
viewLine currentTime line =
    let
        lineTime =
            getTime line

        cssClass =
            if currentTime > lineTime then
                "pointer gray"

            else
                "pointer black"
    in
    viewTimestamp line
        ++ " "
        ++ line.text
        |> Html.text
        |> List.singleton
        |> Html.p [ Attribute.class cssClass, Event.onClick (PlayFromTimestamp lineTime) ]


viewTimestamp : Line -> String
viewTimestamp { minutes, seconds, milliseconds } =
    "["
        ++ String.fromInt minutes
        ++ ":"
        ++ String.fromInt seconds
        ++ "."
        ++ String.fromInt milliseconds
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


updateLyrics : List Line -> Model -> Model
updateLyrics lines model =
    { model | lyrics = lines }


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
