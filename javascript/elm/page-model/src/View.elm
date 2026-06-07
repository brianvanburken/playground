module View exposing (view)

import Browser exposing (Document)
import Html exposing (Html, a, li, text, ul)
import Html.Attributes exposing (href)
import Layouts.Base as Base
import Layouts.Container as Container
import Model exposing (LayoutModel(..), LayoutMsg(..), Model, Msg(..), PageModel(..), PageMsg(..))
import Pages.About as About
import Pages.Home as Home
import Pages.NotFound as NotFound


view : Model -> Document Msg
view model =
    viewPage model.page
        |> applyLayout model.layout


viewPage : PageModel -> Document Msg
viewPage page =
    case page of
        HomePage subModel ->
            { title = Home.title
            , body =
                Home.view subModel
                    |> List.map (Html.map (PageMsg << HomeMsg))
            }

        AboutPage subModel ->
            { title = About.title
            , body =
                About.view subModel
                    |> List.map (Html.map (PageMsg << AboutMsg))
            }

        NotFoundPage ->
            { title = NotFound.title
            , body = NotFound.view
            }

        Loading ->
            { title = "Loading..."
            , body = [ text "Loading..." ]
            }


applyLayout : LayoutModel -> Document Msg -> Document Msg
applyLayout layout content =
    { content | body = viewLayout layout content.body }


viewLayout : LayoutModel -> List (Html Msg) -> List (Html Msg)
viewLayout layout content =
    case layout of
        ContainerLayout config ->
            Container.view config content

        BaseLayout config subModel ->
            Base.view config (LayoutMsg << BaseMsg) subModel content
