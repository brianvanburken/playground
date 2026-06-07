module Model exposing (Model, Msg(..), PageModel(..))

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Pages.About as About
import Pages.Home as Home
import Pages.NotFound as NotFound
import Url exposing (Url)


type PageModel
    = HomePage Home.Model
    | AboutPage About.Model
    | NotFoundPage NotFound.Model


type alias Model =
    { key : Nav.Key
    , page : PageModel
    }


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | HomeMsg Home.Msg
    | AboutMsg About.Msg
    | NotFoundMsg NotFound.Msg
