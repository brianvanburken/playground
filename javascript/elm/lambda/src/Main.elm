module Main exposing (main)

import AWS.Core.Credentials as Credentials
import AWS.Core.Http as AWSHttp exposing (Method(..))
import AWS.Core.Service as Service
import Html exposing (Html)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Task


type alias Model =
    {}


type Msg
    = NoOp
    | GetResponse (Result Http.Error JD.Value)


type alias Function =
    { name : String
    , handler : String
    , runtime : String
    , version : String
    , role : String
    , environment : Environment
    , code : Code
    }


type alias Code =
    { blob : String
    }


type alias Environment =
    { variables : List ( String, String )
    }


type alias Role =
    { arn : String
    , name : String
    }


creds : Credentials.Credentials
creds =
    Credentials.fromAccessKeys
        "<access key here>"
        "<access secret here>"


service : Service.Service
service =
    Service.defineRegional
        "lambda"
        "2015-03-31"
        Service.restJson
        Service.signV4
        (Service.setJsonVersion "1.1")
        "eu-central-1"


lambdaDecoder : JD.Decoder JD.Value
lambdaDecoder =
    JD.value


environmentEncoder : Environment -> JD.Value
environmentEncoder env =
    let
        variables : List ( String, JE.Value )
        variables =
            List.map (Tuple.mapSecond JE.string) env.variables
    in
    JE.object
        [ ( "Variables", JE.object variables )
        ]


codeEncoder : Code -> JD.Value
codeEncoder code =
    JE.object
        [ ( "ZipFile", JE.string code.blob )
        ]


functionEncoder : Function -> JD.Value
functionEncoder function =
    JE.object
        [ ( "FunctionName", JE.string function.name )
        , ( "Handler", JE.string function.handler )
        , ( "Version", JE.string function.version )
        , ( "Runtime", JE.string function.runtime )
        , ( "Role", JE.string function.role )
        , ( "Environment", environmentEncoder function.environment )
        , ( "Code", codeEncoder function.code )
        ]


createFunctionRequest : Function -> AWSHttp.Request JD.Value
createFunctionRequest function =
    AWSHttp.request
        POST
        "/2015-03-31/functions"
        (AWSHttp.jsonBody (functionEncoder function))
        lambdaDecoder


greetingsFunctionBlob : Code
greetingsFunctionBlob =
    """
    <base64 of zip here>
    """
        |> String.trim
        |> Code


greetingsFunctionEnvironment : Environment
greetingsFunctionEnvironment =
    Environment [ ( "GREETING", "Hola" ) ]


init : ( Model, Cmd Msg )
init =
    let
        function =
            Function
                "greetingsOnDemand"
                "greetingsOnDemand.handler"
                "nodejs8.10"
                "0.0.1"
                "<role arn here>"
                greetingsFunctionEnvironment
                greetingsFunctionBlob

        request =
            createFunctionRequest function
                |> AWSHttp.send service creds
                |> Task.attempt GetResponse
    in
    ( {}, request )


view : Model -> Html Msg
view model =
    Html.text "Requesting.."


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetResponse result ->
            let
                _ =
                    Debug.log "result" result
            in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
