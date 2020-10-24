module Main exposing (main)

import Base64
import Html exposing (Html)
import Html.Attributes as Attribute
import Http


type alias Model =
    { binary : String
    , base64 : String
    }


pdfUrl : String
pdfUrl =
    "/example.pdf"


type Msg
    = NoOp
    | OnResult (Result Http.Error String)


pdfDownloadRequest : Cmd Msg
pdfDownloadRequest =
    Http.getString pdfUrl
        |> Http.send OnResult


init : ( Model, Cmd Msg )
init =
    ( { binary = "", base64 = "" }
    , pdfDownloadRequest
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnResult (Ok data) ->
            let
                base64 =
                    Base64.encode data
            in
            ( { model | binary = data, base64 = base64 }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        url =
            "data:application/pdf;base64," ++ model.base64
    in
    Html.div
        []
        [ Html.a [ Attribute.href url ] [ Html.text "Download base64 pdf" ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
