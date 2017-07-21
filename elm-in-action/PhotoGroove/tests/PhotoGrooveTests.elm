module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import PhotoGroove exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import Fuzz exposing (Fuzzer, list, int, string)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, attribute)
import Html.Attributes exposing (src)


stateTransitions : Test
stateTransitions =
    describe "state transitions"
        [ fuzz2 string int "title defaults to (untitled)" <|
            \url size ->
                [ ( "url", Encode.string url )
                , ( "size", Encode.int size )
                ]
                    |> Encode.object
                    |> decodeValue photoDecoder
                    |> Result.map .title
                    |> Expect.equal (Ok "(untitled)")
        , fuzz string " SelectByUrl selects the given photo by URL " <|
            \url ->
                PhotoGroove.initialModel
                    |> PhotoGroove.update (SelectByUrl url)
                    |> Tuple.first
                    |> .selectedUrl
                    |> Expect.equal (Just url)
        , fuzz (list string) "LoadPhotos slects the first photo" <|
            \urls ->
                let
                    photos =
                        List.map photoFromUrl urls
                in
                    PhotoGroove.initialModel
                        |> PhotoGroove.update (LoadPhotos (Ok photos))
                        |> Tuple.first
                        |> .selectedUrl
                        |> Expect.equal (List.head urls)
        ]


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            PhotoGroove.initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


thumbnailsWork : Test
thumbnailsWork =
    fuzz (Fuzz.intRange 1 5) "URLs render as thumbnails" <|
        \urlCount ->
            let
                urls : List String
                urls =
                    List.range 1 urlCount
                        |> List.map (\num -> toString num ++ ".png")

                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
                { initialModel | photos = List.map photoFromUrl urls }
                    |> PhotoGroove.view
                    |> Query.fromHtml
                    |> Expect.all thumbnailChecks


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute <| src (urlPrefix ++ url) ]
        |> Query.count (Expect.atLeast 1)


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }
