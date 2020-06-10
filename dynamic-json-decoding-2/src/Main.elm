module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (Html, text)
import Json.Decode as JD exposing (Value)
import Json.Decode.Pipeline as JDP
import Set


type alias Model =
    Result JD.Error (List EnrichedInformationObject)


type EnrichedInformationObject
    = EnrichedInformationObject Data Metadata


type EnrichedType
    = Video
    | Document


type alias Data =
    { id : String
    , filename : String
    , enrichedType : EnrichedType
    }


type alias Metadata =
    Dict String String


prefix : String
prefix =
    "result"


decoderList : JD.Decoder (List EnrichedInformationObject)
decoderList =
    JD.list decoder


decoder : JD.Decoder EnrichedInformationObject
decoder =
    JD.succeed EnrichedInformationObject
        |> JDP.custom dataDecoder
        |> JDP.custom metadataDecoder


typeDecoder : String -> JD.Decoder EnrichedType
typeDecoder type_ =
    case type_ of
        "VIDEO" ->
            JD.succeed Video

        "DOCUMENT" ->
            JD.succeed Document

        _ ->
            JD.fail ("Invalid EnrichedInformationObject type: " ++ type_)


dataDecoder : JD.Decoder Data
dataDecoder =
    JD.succeed Data
        |> JDP.requiredAt [ prefix, "id" ] JD.string
        |> JDP.requiredAt [ prefix, "storageLocation" ] JD.string
        |> JDP.requiredAt [ prefix, "type" ] (JD.string |> JD.andThen typeDecoder)


metadataDecoder : JD.Decoder Metadata
metadataDecoder =
    JD.field prefix (JD.keyValuePairs valueToString)
        |> JD.map cleanMetadata


valueToString : JD.Decoder String
valueToString =
    let
        asString =
            [ JD.string
            , JD.float |> JD.map String.fromFloat
            , JD.bool
                |> JD.map
                    (\b ->
                        if b then
                            "true"

                        else
                            "false"
                    )
            , JD.null ""
            ]
    in
    JD.oneOf
        ((JD.list (JD.oneOf asString) |> JD.map (String.join ", "))
            :: asString
        )


cleanMetadata : List ( String, String ) -> Metadata
cleanMetadata =
    Dict.fromList
        >> Dict.Extra.removeMany
            (Set.fromList
                [ "id"
                , "type"
                , "storageLocation"
                ]
            )


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


main : Program () Model Never
main =
    Browser.sandbox
        { init = JD.decodeString decoderList json
        , view = \m -> text (Debug.toString m)
        , update = \_ m -> m
        }
