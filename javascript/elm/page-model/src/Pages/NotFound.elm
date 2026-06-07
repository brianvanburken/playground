module Pages.NotFound exposing (title, view)

import Html exposing (Html, a, h1, text)
import Html.Attributes exposing (href)


title : String
title =
    "404 - Not Found"


view : List (Html msg)
view =
    [ h1 [] [ text "404 - Not Found" ]
    , a [ href "/" ] [ text "Back to home" ]
    ]
