module Main exposing (main)

import Html exposing (Html, div, img, input, text)
import Html.Attributes exposing (attribute, src, type_)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder)
import Time exposing (Time)


type alias File =
    { lastModified : Time
    , name : String
    , size : Int
    , mimeType : String
    , dataURL : String
    }


fileDecoder : Decoder File
fileDecoder =
    Json.Decode.map5 File
        (Json.Decode.at [ "detail", "lastModified" ] Json.Decode.float)
        (Json.Decode.at [ "detail", "name" ] Json.Decode.string)
        (Json.Decode.at [ "detail", "size" ] Json.Decode.int)
        (Json.Decode.at [ "detail", "mimeType" ] Json.Decode.string)
        (Json.Decode.at [ "detail", "dataURL" ] Json.Decode.string)


type alias Model =
    Maybe File


type Msg
    = ReadFile File


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( Nothing, Cmd.none )
        }



{-
   Creates a handler that parses the file input and converts into an JSON object.
   Then it dispatches a custom event where Elm is listening on.
-}


onChangeHandler : String
onChangeHandler =
    """
    const file = event.currentTarget.files[0];
    if (file) {
        const reader = new FileReader();
        reader.addEventListener("load", () => {
            const result =
                { lastModified: file.lastModified
                , name: file.name
                , size: file.size
                , mimeType: file.type
                , dataURL: reader.result
                };
            event.target.dispatchEvent(
                new CustomEvent("file", {detail: result} )
            );
        });
        reader.readAsDataURL(file);
    }
    """


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "file"
            , attribute "onchange" onChangeHandler
            , on "file" (Json.Decode.map ReadFile fileDecoder)
            ]
            []
        , case model of
            Nothing ->
                text ""

            Just file ->
                img [ src file.dataURL ] []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadFile file ->
            ( Just file, Cmd.none )
