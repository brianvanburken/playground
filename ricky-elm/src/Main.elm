port module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Http
import Parser exposing ((|.), (|=), Count(..), Parser, ignore, int, keep, keyword, map, oneOf, oneOrMore, succeed, symbol, zeroOrMore)
import Regex exposing (HowMany(..))
import RemoteData exposing (RemoteData(..), WebData)
import String.Extra exposing (replace)
import Time exposing (Time)


type alias Line =
    { minutes : Int, seconds : Int, miliseconds : Int, text : String }


type alias Model =
    { lyrics : List Line
    , rawLyrics : WebData String
    , currentTime : Float
    }


port goToTime : Float -> Cmd msg


port setCurrentTime : (Float -> msg) -> Sub msg


port getCurrentTime : () -> Cmd msg


fetchLyrics : Cmd Msg
fetchLyrics =
    Http.getString "/assets/rick.txt"
        |> RemoteData.sendRequest
        |> Cmd.map FetchLyrics


type Msg
    = FetchLyrics (WebData String)
    | SetCurrentTime Float
    | GetCurrentTime Time
    | GoToTime Float


parseLine : Parser Line
parseLine =
    succeed Line
        |. symbol "["
        |= int
        |. symbol ":"
        |= int
        |. symbol ":"
        |= int
        |. symbol "]"
        |= keep oneOrMore anything


anything : Char -> Bool
anything _ =
    True


parseLyrics : String -> List Line
parseLyrics rawLyrics =
    let
        unpackResult : Result Parser.Error Line -> Line
        unpackResult res =
            case res of
                Ok line ->
                    line

                Err error ->
                    Line 0 0 0 (toString error)

        lines =
            rawLyrics
                |> String.split "\n"
                |> List.map fixString
                |> List.map (Parser.run parseLine)
                |> List.map unpackResult
    in
    lines



-- needed since parser doesn't accept double zero


fixString : String -> String
fixString string =
    string
        |> replace "00" "0"
        |> replace "01" "1"
        |> replace "02" "2"
        |> replace "03" "3"
        |> replace "04" "4"
        |> replace "05" "5"
        |> replace "06" "6"
        |> replace "07" "7"
        |> replace "08" "8"
        |> replace "09" "9"
        |> replace "." ":"


init : ( Model, Cmd Msg )
init =
    ( { lyrics = [], rawLyrics = NotAsked, currentTime = 0.0 }, fetchLyrics )


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
                "pointer light-gray"

            else
                "pointer black"
    in
    viewTimestamp line
        ++ " "
        ++ line.text
        |> Html.text
        |> List.singleton
        |> Html.p [ Attribute.class cssClass, Event.onClick (GoToTime lineTime) ]


viewTimestamp : Line -> String
viewTimestamp line =
    "["
        ++ toString line.minutes
        ++ ":"
        ++ toString line.seconds
        ++ "."
        ++ toString line.miliseconds
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
                        |> withoutCmd

                _ ->
                    model
                        |> updateRawLyrics response
                        |> withoutCmd

        SetCurrentTime time ->
            model
                |> updateCurrentTime time
                |> withoutCmd

        GetCurrentTime _ ->
            ( model, getCurrentTime () )

        GoToTime time ->
            ( model, goToTime time )


withoutCmd : Model -> ( Model, Cmd Msg )
withoutCmd model =
    ( model, Cmd.none )


updateLyrics : List Line -> Model -> Model
updateLyrics lines model =
    { model | lyrics = lines }


updateRawLyrics : WebData String -> Model -> Model
updateRawLyrics data model =
    { model | rawLyrics = data }


updateCurrentTime : Float -> Model -> Model
updateCurrentTime time model =
    { model | currentTime = time }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second GetCurrentTime
        , setCurrentTime SetCurrentTime
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
