module Subscriptions exposing (subscriptions)

import Pages.About as About
import Pages.Home as Home
import Pages.NotFound as NotFound
import Model exposing (Model, Msg(..), PageModel(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        HomePage subModel ->
            Sub.map HomeMsg (Home.subscriptions subModel)

        AboutPage subModel ->
            Sub.map AboutMsg (About.subscriptions subModel)

        NotFoundPage subModel ->
            Sub.map NotFoundMsg (NotFound.subscriptions subModel)
