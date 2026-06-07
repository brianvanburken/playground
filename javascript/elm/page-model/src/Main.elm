module Main exposing (main)

import Browser
import Init
import Subscriptions
import Model exposing (Model, Msg(..))
import Update
import View


main : Program () Model Msg
main =
    Browser.application
        { init = Init.init
        , update = Update.update
        , view = View.view
        , subscriptions = Subscriptions.subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
