module Main exposing (Flags, Model, Msg(..), Navigation, Page(..), bodyView, fromUrl, init, initialModel, main, parser, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Navigation
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, width)
import Url
import Url.Parser as Parser exposing ((</>), Parser, int, s, string)


type alias Model =
    { navigation : Navigation }


type alias Navigation =
    { key : Navigation.Key
    , page : Page
    }


initialModel : Url.Url -> Navigation.Key -> Model
initialModel url key =
    { navigation =
        { key = key
        , page =
            case fromUrl url of
                Nothing ->
                    NotFound

                Just a ->
                    a
        }
    }


parser : Parser (Page -> b) b
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Radar (s "radar" </> string </> int </> string)
        ]


fromUrl : Url.Url -> Maybe Page
fromUrl url =
    -- Treat fragment as path for convenience
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


type Page
    = Home
    | Radar String Int String
    | NotFound


type alias Flags =
    ()


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( initialModel url key, Cmd.none )



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChange Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "" msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navigation.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChange url ->
            let
                navigation =
                    model.navigation
            in
            ( { model
                | navigation =
                    { navigation
                        | page =
                            case fromUrl url of
                                Nothing ->
                                    NotFound

                                Just a ->
                                    a
                    }
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Document Msg
view model =
    let
        title =
            case model.navigation.page of
                NotFound ->
                    "Not Found"

                Home ->
                    "Homepage"

                Radar team year quarter ->
                    "Radar > "
                        ++ team
                        ++ " > "
                        ++ String.fromInt year
                        ++ " > "
                        ++ quarter
    in
    { title = title
    , body = [ bodyView model ]
    }


bodyView : Model -> Html Msg
bodyView model =
    div []
        [ ul []
            [ li [] [ a [ href "#/" ] [ text "Home" ] ]
            , li [] [ a [ href "#/radar" ] [ text "Radar" ] ]
            ]
        , div []
            [ case model.navigation.page of
                NotFound ->
                    text "Page not Found"

                Home ->
                    text "Home page"

                Radar team year quarter ->
                    viewRadar team year quarter model
            ]
        ]


viewRadar : String -> Int -> String -> Model -> Html Msg
viewRadar team year quarter model =
    Html.div []
        [ quadrant "#29abe2" NorthWest
        , quadrant "#20ba96" NorthEast
        , quadrant "#f38a3e" SouthWest
        , quadrant "#764594" SouthEast
        ]


type Position
    = NorthWest
    | NorthEast
    | SouthWest
    | SouthEast


quadrant : String -> Position -> Html msg
quadrant hexColor position =
    let
        size =
            573

        ( x, y ) =
            case position of
                NorthWest ->
                    ( size, size )

                NorthEast ->
                    ( 0, size )

                SouthWest ->
                    ( size, 0 )

                SouthEast ->
                    ( 0, 0 )

        cxString =
            String.fromInt >> cx

        cyString =
            String.fromInt >> cy
    in
    Svg.svg
        [ fill hexColor
        , height (String.fromInt size)
        , width (String.fromInt size)
        , stroke hexColor
        ]
        [ Svg.circle [ cxString x, cyString y, r "572", fill hexColor ] []
        , Svg.circle [ cxString x, cyString y, r "565", fill "#EEEEEE" ] []
        , Svg.circle [ cxString x, cyString y, r "452", fill "#DADADA" ] []
        , Svg.circle [ cxString x, cyString y, r "305", fill "#CACACA" ] []
        , Svg.circle [ cxString x, cyString y, r "195", fill "#BABABA" ] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChange
        }
