module Update exposing (update)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Init exposing (initPage)
import Layouts.Base as Base
import Model exposing (LayoutModel(..), LayoutMsg(..), Model, Msg(..), PageModel(..), PageMsg(..))
import Pages.About as About
import Pages.Home as Home
import Route
import Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested (Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        UrlRequested (External url) ->
            ( model, Nav.load url )

        UrlChanged url ->
            initPage (Route.fromUrl url) model

        PageMsg pageMsg ->
            handlePage pageMsg model

        LayoutMsg layoutMsg ->
            updateLayout layoutMsg model


handlePage : PageMsg -> Model -> ( Model, Cmd Msg )
handlePage msg model =
    case msg of
        HomeMsg subMsg ->
            case model.page of
                HomePage subModel ->
                    let
                        ( m, cmd ) =
                            Home.update subMsg subModel
                    in
                    ( { model | page = HomePage m }, Cmd.map (PageMsg << HomeMsg) cmd )

                _ ->
                    ( model, Cmd.none )

        AboutMsg subMsg ->
            case model.page of
                AboutPage subModel ->
                    let
                        ( m, cmd ) =
                            About.update subMsg subModel
                    in
                    ( { model | page = AboutPage m }, Cmd.map (PageMsg << AboutMsg) cmd )

                _ ->
                    ( model, Cmd.none )


updateLayout : LayoutMsg -> Model -> ( Model, Cmd Msg )
updateLayout msg model =
    case msg of
        BaseMsg subMsg ->
            case model.layout of
                BaseLayout config subModel ->
                    let
                        ( m, cmd ) =
                            Base.update subMsg subModel
                    in
                    ( { model | layout = BaseLayout config m }, Cmd.map (LayoutMsg << BaseMsg) cmd )

                _ ->
                    ( model, Cmd.none )
