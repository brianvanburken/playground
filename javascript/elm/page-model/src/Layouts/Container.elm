module Layouts.Container exposing (Config, config, view)

import Html exposing (Html, div)


type Config
    = Config ConfigData


type alias ConfigData =
    {}


config : Config
config =
    Config {}


view : Config -> List (Html msg) -> List (Html msg)
view (Config cnfg) content =
    [ div [] content ]
