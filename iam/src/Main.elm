module Main exposing (listRolesRequest, main)

import AWS.Core.Credentials as Credentials
import AWS.Core.Http as AWSHttp exposing (Method(..))
import AWS.Core.Service as Service
import Html exposing (Html)
import Http
import Json.Decode as JD
import Task


type alias Model =
    { roles : List Role }


type Msg
    = NoOp
    | GetResponse (Result Http.Error (List Role))


type alias Role =
    { arn : String
    , name : String
    }


creds : Credentials.Credentials
creds =
    Credentials.fromAccessKeys
        "access key"
        "access secret"


service : Service.Service
service =
    Service.defineGlobal
        "iam"
        "2010-05-08"
        Service.query
        Service.signV4
        (Service.setXmlNamespace "https://iam.amazonaws.com/doc/2010-05-08/")


listRolesDecoder : JD.Decoder (List Role)
listRolesDecoder =
    JD.at [ "ListRolesResponse", "ListRolesResult", "Roles" ] (JD.list roleDecoder)


roleDecoder : JD.Decoder Role
roleDecoder =
    JD.map2 Role
        (JD.field "Arn" JD.string)
        (JD.field "RoleName" JD.string)


listRolesRequest : AWSHttp.Request (List Role)
listRolesRequest =
    AWSHttp.request GET "/" AWSHttp.emptyBody listRolesDecoder
        |> AWSHttp.addQuery
            [ ( "Action", "ListRoles" )
            , ( "MaxItems", "100" )
            , ( "PathPrefix", "/" )
            , ( "Version", "2010-05-08" )
            ]


init : ( Model, Cmd Msg )
init =
    let
        request =
            listRolesRequest
                |> AWSHttp.send service creds
                |> Task.attempt GetResponse
    in
    ( { roles = [] }, request )


view : Model -> Html Msg
view model =
    List.map viewRole model.roles
        |> Html.div []


viewRole : Role -> Html Msg
viewRole role =
    Html.p []
        [ Html.text role.name
        , Html.text "\n"
        , Html.text role.arn
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetResponse result ->
            case result of
                Ok roles ->
                    ( { model | roles = roles }, Cmd.none )

                Err _ ->
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
