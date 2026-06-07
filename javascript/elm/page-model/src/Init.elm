module Init exposing (init, initPage)

import Browser.Navigation as Nav
import Layouts.Base as BaseLayout
import Layouts.Container as ContainerLayout
import Model exposing (LayoutModel(..), LayoutMsg(..), Model, Msg(..), PageModel(..), PageMsg(..))
import Pages.About as About
import Pages.Home as Home
import Pages.NotFound as NotFound
import Route exposing (Route(..))
import Url exposing (Url)


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        initialModel : Model
        initialModel =
            { key = key
            , page = Loading
            , layout = ContainerLayout ContainerLayout.config
            }

        route : Route
        route =
            Route.fromUrl url
    in
    initPage route initialModel


initPage : Route -> Model -> ( Model, Cmd Msg )
initPage route model =
    case route of
        Home ->
            let
                ( m, cmd ) =
                    Home.init
            in
            ( model
                |> setPage (HomePage m)
                |> withBaseLayout
            , Cmd.map (PageMsg << HomeMsg) cmd
            )

        About ->
            let
                ( m, cmd ) =
                    About.init
            in
            ( model
                |> setPage (AboutPage m)
                |> withBaseLayout
            , Cmd.map (PageMsg << AboutMsg) cmd
            )

        NotFound ->
            ( model
                |> setPage NotFoundPage
                |> withContainerLayout
            , Cmd.none
            )


withContainerLayout : Model -> Model
withContainerLayout =
    setLayout (ContainerLayout ContainerLayout.config)


withBaseLayout : Model -> Model
withBaseLayout =
    let
        ( m, cmd ) =
            BaseLayout.init
    in
    setLayout (BaseLayout BaseLayout.config m)


setPage : PageModel -> Model -> Model
setPage page model =
    { model | page = page }


setLayout : LayoutModel -> Model -> Model
setLayout layout model =
    { model | layout = layout }
