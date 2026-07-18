module Main exposing (main)

import Browser
import Counter


main : Program Counter.Flags Counter.Model Counter.Msg
main =
    Browser.element
        { init = Counter.init
        , update = Counter.update
        , view = Counter.view
        , subscriptions = \_ -> Sub.none
        }
