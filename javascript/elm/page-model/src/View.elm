module View exposing (view)

import Browser exposing (Document)
import Html exposing (Html, a, li, text, ul)
import Html.Attributes exposing (href)
import Pages.About as About
import Pages.Home as Home
import Pages.NotFound as NotFound
import Model exposing (Model, Msg(..), PageModel(..))


view : Model -> Document Msg
view model =
    let
        doc =
            viewPage model.page
    in
    { doc | body = nav :: doc.body }


nav : Html Msg
nav =
    ul []
        [ li [] [ a [ href "/" ] [ text "Home" ] ]
        , li [] [ a [ href "/about" ] [ text "About" ] ]
        ]


viewPage : PageModel -> Document Msg
viewPage page =
    case page of
        HomePage subModel ->
            let
                doc =
                    Home.view subModel
            in
            { title = doc.title, body = List.map (Html.map HomeMsg) doc.body }

        AboutPage subModel ->
            let
                doc =
                    About.view subModel
            in
            { title = doc.title, body = List.map (Html.map AboutMsg) doc.body }

        NotFoundPage subModel ->
            let
                doc =
                    NotFound.view subModel
            in
            { title = doc.title, body = List.map (Html.map NotFoundMsg) doc.body }
