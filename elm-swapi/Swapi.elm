module Main exposing (..)

import Html exposing (Html, text, div)
import Http exposing (Request)
import Json.Decode exposing (int, string, list, decodeString, Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Character =
    { name : String
    , birth_year : String
    , gender : String
    }


type alias Model =
    { results : List Character
    }


type Msg
    = LoadPeople (Result Http.Error Model)


peopleDecoder : Decoder Model
peopleDecoder =
    decode Model
        |> required "results" (list characterDecoder)


characterDecoder : Decoder Character
characterDecoder =
    decode Character
        |> required "name" string
        |> required "birth_year" string
        |> required "gender" string


initialCmd : Cmd Msg
initialCmd =
    Http.get "https://swapi.co/api/people" peopleDecoder
        |> Http.send LoadPeople


initialModel : Model
initialModel =
    { results = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadPeople (Ok new_model) ->
            ( new_model, Cmd.none )

        LoadPeople (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        (List.map viewCharacter model.results)


viewCharacter : Character -> Html Msg
viewCharacter character =
    div []
        [ text character.name
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
