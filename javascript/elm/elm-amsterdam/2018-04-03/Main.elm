module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attribute
import Json.Decode as JD exposing (Decoder)


type Field
    = TextInput String


type alias Model =
    List Field


type Msg
    = NoOp


data : String
data =
    """
    [ { "key": "name", "type": "String" } ]
    """


fieldDecoder : Decoder Field
fieldDecoder =
    JD.map TextInput (JD.field "key" JD.string)


dataDecoder : Decoder (List Field)
dataDecoder =
    JD.list fieldDecoder


init : ( Model, Cmd Msg )
init =
    let
        fields =
            data
                |> JD.decodeString dataDecoder
                |> Result.withDefault []
    in
    ( fields, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = always Sub.none
        , init = init
        }


view : Model -> Html Msg
view model =
    Html.div [] (List.map fieldToInput model)


fieldToInput : Field -> Html Msg
fieldToInput field =
    case field of
        TextInput name ->
            Html.label
                []
                [ Html.text name
                , Html.input [ Attribute.type_ "text" ] []
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
