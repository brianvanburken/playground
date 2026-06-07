module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = Home
    | About
    | NotFound


fromUrl : Url -> Route
fromUrl =
    parse parser
        >> Maybe.withDefault NotFound


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map About (s "about")
        ]
