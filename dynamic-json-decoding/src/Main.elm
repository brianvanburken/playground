module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Json.Decode as JD exposing (Value)
import Json.Decode.Pipeline as JDP
import List.Extra


type alias Metadata =
    List ( String, String )


type alias Model =
    Result JD.Error (List EnrichedInformationObject)


type EnrichedInformationObject
    = Video Data Metadata
    | Document Data Metadata


type alias Data =
    { id : String
    , fileName : String
    }


json : String
json =
    """
    [
    {
        "score": 127.37689,
        "result": {
            "extension": "webm",
            "thumbnail": "IMG_8894.png",
            "description": "Omschrijving van de situatie op de Communicatieweg in Assendelft",
            "duration": "02:59",
            "projectId": "PWN",
            "location": "Communicatieweg Assendelft",
            "storageLocation": "IMG_8894.webm",
            "type": "VIDEO",
            "id": "5e4fe556067cc0a2af16548a"
        }
    },
    {
        "score": 126.37689,
        "result": {
            "thumbnail": "IMG_8894.png",
            "numberOfPages": 20,
            "storageLocation": "IMG_8894.pdf",
            "type": "DOCUMENT",
            "id": "5e4fe556067cc0a2af16548a"
        }
    }
    ]
    """


decoder : JD.Decoder (List EnrichedInformationObject)
decoder =
    JD.list
        (JD.field "result" (JD.keyValuePairs valueToString)
            |> JD.andThen convertToEnrichedInformationObject
        )


valueToString : JD.Decoder String
valueToString =
    JD.oneOf
        [ JD.string
        , JD.float |> JD.andThen (String.fromFloat >> JD.succeed)
        , JD.int |> JD.andThen (String.fromInt >> JD.succeed)
        , JD.bool
            |> JD.andThen
                (\b ->
                    case b of
                        True ->
                            JD.succeed "true"

                        False ->
                            JD.succeed "false"
                )
        , JD.null ""
        ]


convertToEnrichedInformationObject : Metadata -> JD.Decoder EnrichedInformationObject
convertToEnrichedInformationObject keyValues =
    let
        findValueForKey key =
            List.Extra.find (Tuple.first >> (==) key) keyValues
                |> Maybe.map Tuple.second

        id =
            findValueForKey "id"

        fileName =
            findValueForKey "storageLocation"

        type_ =
            findValueForKey "type"

        data =
            Maybe.map2 Data id fileName
                |> Maybe.withDefault (Data "" "")

        isAllowedMetadata =
            \key ->
                case key of
                    "id" ->
                        False

                    "storageLocation" ->
                        False

                    "type" ->
                        False

                    _ ->
                        True

        metadata =
            List.filter (Tuple.first >> isAllowedMetadata) keyValues
    in
    case type_ of
        Just "VIDEO" ->
            JD.succeed (Video data metadata)

        Just "DOCUMENT" ->
            JD.succeed (Document data metadata)

        Just t ->
            JD.fail ("Unable to convert to EnrichedInformationObject from type " ++ t)

        Nothing ->
            JD.fail "Unable to type on result "


main : Program () Model Never
main =
    Browser.sandbox
        { init = JD.decodeString decoder json
        , view = \m -> text (Debug.toString m)
        , update = \_ m -> m
        }
