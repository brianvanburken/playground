module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, li, p, span, text, ul)
import Html.Attributes exposing (attribute, checked, disabled, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Todo =
    { id : String
    , title : String
    , completed : Bool
    }


type Status
    = Loading
    | Ready
    | Failed


type alias Model =
    { todos : List Todo
    , draft : String
    , status : Status
    }


type Msg
    = GotTodos (Result Http.Error (List Todo))
    | DraftChanged String
    | Submitted
    | GotCreated (Result Http.Error Todo)
    | Toggled Todo
    | GotUpdated (Result Http.Error Todo)
    | Removed String
    | GotRemoved String (Result Http.Error ())


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todos = [], draft = "", status = Loading }, fetchTodos )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTodos (Ok todos) ->
            ( { model | todos = todos, status = Ready }, Cmd.none )

        GotTodos (Err _) ->
            ( { model | status = Failed }, Cmd.none )

        DraftChanged draft ->
            ( { model | draft = draft }, Cmd.none )

        Submitted ->
            if String.isEmpty (String.trim model.draft) then
                ( model, Cmd.none )

            else
                ( { model | draft = "" }, createTodo model.draft )

        GotCreated (Ok todo) ->
            ( { model | todos = model.todos ++ [ todo ] }, Cmd.none )

        GotCreated (Err _) ->
            ( { model | status = Failed }, Cmd.none )

        Toggled todo ->
            ( model, setCompleted todo (not todo.completed) )

        GotUpdated (Ok todo) ->
            ( { model | todos = List.map (replaceById todo) model.todos }, Cmd.none )

        GotUpdated (Err _) ->
            ( { model | status = Failed }, Cmd.none )

        Removed id ->
            ( model, deleteTodo id )

        GotRemoved id (Ok ()) ->
            ( { model | todos = List.filter (\todo -> todo.id /= id) model.todos }, Cmd.none )

        GotRemoved _ (Err _) ->
            ( { model | status = Failed }, Cmd.none )


replaceById : Todo -> Todo -> Todo
replaceById new current =
    if current.id == new.id then
        new

    else
        current



-- HTTP


todoDecoder : Decoder Todo
todoDecoder =
    Decode.map3 Todo
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "completed" Decode.bool)


fetchTodos : Cmd Msg
fetchTodos =
    Http.get
        { url = "/api/todos"
        , expect = Http.expectJson GotTodos (Decode.list todoDecoder)
        }


createTodo : String -> Cmd Msg
createTodo title =
    Http.post
        { url = "/api/todos"
        , body = Http.jsonBody (Encode.object [ ( "title", Encode.string (String.trim title) ) ])
        , expect = Http.expectJson GotCreated todoDecoder
        }


setCompleted : Todo -> Bool -> Cmd Msg
setCompleted todo completed =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "/api/todos/" ++ todo.id
        , body = Http.jsonBody (Encode.object [ ( "completed", Encode.bool completed ) ])
        , expect = Http.expectJson GotUpdated todoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteTodo : String -> Cmd Msg
deleteTodo id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/api/todos/" ++ id
        , body = Http.emptyBody
        , expect = Http.expectWhatever (GotRemoved id)
        , timeout = Nothing
        , tracker = Nothing
        }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Todos" ]
        , viewForm model
        , viewStatus model
        ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ onSubmit Submitted ]
        [ input
            [ type_ "text"
            , value model.draft
            , onInput DraftChanged
            , placeholder "What needs to be done?"
            , attribute "aria-label" "New todo"
            ]
            []
        , button [ type_ "submit", disabled (String.isEmpty (String.trim model.draft)) ]
            [ text "Add" ]
        ]


viewStatus : Model -> Html Msg
viewStatus model =
    case model.status of
        Loading ->
            p [] [ text "Loading..." ]

        Failed ->
            p [ attribute "role" "alert" ] [ text "Something went wrong." ]

        Ready ->
            ul [] (List.map viewTodo model.todos)


viewTodo : Todo -> Html Msg
viewTodo todo =
    li []
        [ input
            [ type_ "checkbox"
            , checked todo.completed
            , onCheck (\_ -> Toggled todo)
            , attribute "aria-label" ("Toggle " ++ todo.title)
            ]
            []
        , span [] [ text todo.title ]
        , button
            [ onClick (Removed todo.id)
            , attribute "aria-label" ("Delete " ++ todo.title)
            ]
            [ text "Delete" ]
        ]
