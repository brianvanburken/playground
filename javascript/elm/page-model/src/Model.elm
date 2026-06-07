module Model exposing (LayoutModel(..), Model, Msg(..), PageModel(..))

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Layouts.Base as Base
import Layouts.Container as Container
import Pages.About as About
import Pages.Home as Home
import Pages.NotFound as NotFound
import Url exposing (Url)


type PageModel
    = HomePage Home.Model
    | AboutPage About.Model
    | NotFoundPage
    | Loading


type LayoutModel
    = ContainerLayout Container.Config
    | BaseLayout Base.Config Base.Model


type alias Model =
    { key : Nav.Key
    , page : PageModel
    , layout : LayoutModel
    }


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | HomeMsg Home.Msg
    | AboutMsg About.Msg
    | BaseMsg Base.Msg
