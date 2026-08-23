module Main exposing (main)

import Api.Mutation as Mutation
import Api.Object
import Api.Object.Todo as Todo
import Api.Query as Query
import Api.Scalar
import Browser
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, button, div, h1, input, li, p, span, text, ul)
import Html.Attributes exposing (attribute, checked, disabled, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)


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


type alias Response a =
    Result (Graphql.Http.Error a) a


type Msg
    = GotTodos (Response (List Todo))
    | DraftChanged String
    | Submitted
    | GotCreated (Response Todo)
    | Toggled Todo
    | GotUpdated (Response Todo)
    | Removed String
    | GotRemoved (Response String)


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

        GotRemoved (Ok id) ->
            ( { model | todos = List.filter (\todo -> todo.id /= id) model.todos }, Cmd.none )

        GotRemoved (Err _) ->
            ( { model | status = Failed }, Cmd.none )


replaceById : Todo -> Todo -> Todo
replaceById new current =
    if current.id == new.id then
        new

    else
        current



-- GRAPHQL


endpoint : String
endpoint =
    "/graphql"


rawId : Api.Scalar.Id -> String
rawId (Api.Scalar.Id value) =
    value


todoSelection : SelectionSet Todo Api.Object.Todo
todoSelection =
    SelectionSet.map3 Todo
        (SelectionSet.map rawId Todo.id)
        Todo.title
        Todo.completed


todosQuery : SelectionSet (List Todo) RootQuery
todosQuery =
    Query.todos todoSelection


fetchTodos : Cmd Msg
fetchTodos =
    todosQuery
        |> Graphql.Http.queryRequest endpoint
        |> Graphql.Http.withOperationName "Todos"
        |> Graphql.Http.send GotTodos


createTodo : String -> Cmd Msg
createTodo title =
    Mutation.addTodo { title = String.trim title } todoSelection
        |> Graphql.Http.mutationRequest endpoint
        |> Graphql.Http.withOperationName "AddTodo"
        |> Graphql.Http.send GotCreated


setCompleted : Todo -> Bool -> Cmd Msg
setCompleted todo completed =
    Mutation.setTodoCompleted
        { id = Api.Scalar.Id todo.id, completed = completed }
        todoSelection
        |> Graphql.Http.mutationRequest endpoint
        |> Graphql.Http.withOperationName "SetTodoCompleted"
        |> Graphql.Http.send GotUpdated


deleteTodo : String -> Cmd Msg
deleteTodo id =
    Mutation.deleteTodo { id = Api.Scalar.Id id }
        |> SelectionSet.map rawId
        |> Graphql.Http.mutationRequest endpoint
        |> Graphql.Http.withOperationName "DeleteTodo"
        |> Graphql.Http.send GotRemoved



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
