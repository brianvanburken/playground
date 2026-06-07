module Layouts.Base exposing (Config, Model, Msg(..), config, init, subscriptions, update, view)

import Html exposing (Html, a, aside, button, div, li, main_, nav, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)


type Config
    = Config ConfigData


type alias ConfigData =
    { title : String }


type Model
    = Model ModelData


type alias ModelData =
    { isOpen : Bool }


type Msg
    = Toggle


config : Config
config =
    Config { title = "" }


withTitle : String -> Config -> Config
withTitle t (Config c) =
    Config { c | title = t }


init : ( Model, Cmd Msg )
init =
    ( Model { isOpen = False }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Toggle ->
            ( Model { model | isOpen = not model.isOpen }, Cmd.none )


view : Config -> (Msg -> msg) -> Model -> List (Html msg) -> List (Html msg)
view (Config cnfg) toMsg (Model model) content =
    [ button [ onClick (toMsg Toggle) ]
        [ text
            (if model.isOpen then
                "Close sidebar"

             else
                "Open sidebar"
            )
        ]
    , if model.isOpen then
        aside []
            [ text cnfg.title
            , nav []
                [ ul []
                    [ li [] [ a [ href "/" ] [ text "Home" ] ]
                    , li [] [ a [ href "/about" ] [ text "About" ] ]
                    ]
                ]
            ]

      else
        text ""
    , main_ [] content
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
