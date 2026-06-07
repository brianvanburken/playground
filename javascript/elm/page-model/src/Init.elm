module Init exposing (init, initPage)

import Browser.Navigation as Nav
import Pages.About as About
import Pages.Home as Home
import Pages.NotFound as NotFound
import Route exposing (Route(..))
import Model exposing (Model, Msg(..), PageModel(..))
import Url exposing (Url)


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( page, cmd ) =
            initPage (Route.fromUrl url)
    in
    ( { key = key, page = page }, cmd )


initPage : Route -> ( PageModel, Cmd Msg )
initPage route =
    case route of
        Home ->
            let
                ( m, cmd ) =
                    Home.init
            in
            ( HomePage m, Cmd.map HomeMsg cmd )

        About ->
            let
                ( m, cmd ) =
                    About.init
            in
            ( AboutPage m, Cmd.map AboutMsg cmd )

        NotFound ->
            let
                ( m, cmd ) =
                    NotFound.init
            in
            ( NotFoundPage m, Cmd.map NotFoundMsg cmd )
