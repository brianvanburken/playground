module Subscriptions exposing (subscriptions)

import Layouts.Base as Base
import Model exposing (LayoutModel(..), LayoutMsg(..), Model, Msg(..), PageModel(..), PageMsg(..))
import Pages.About as About
import Pages.Home as Home
import Pages.NotFound as NotFound


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ pageSubscriptions model, layoutSubscriptions model ]


pageSubscriptions : Model -> Sub Msg
pageSubscriptions model =
    case model.page of
        HomePage subModel ->
            Sub.map (PageMsg << HomeMsg) (Home.subscriptions subModel)

        AboutPage subModel ->
            Sub.map (PageMsg << AboutMsg) (About.subscriptions subModel)

        NotFoundPage ->
            Sub.none

        Loading ->
            Sub.none


layoutSubscriptions : Model -> Sub Msg
layoutSubscriptions model =
    case model.layout of
        ContainerLayout _ ->
            Sub.none

        BaseLayout _ subModel ->
            Sub.map (LayoutMsg << BaseMsg) (Base.subscriptions subModel)
