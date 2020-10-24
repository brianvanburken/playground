module Koans15AboutRegexes exposing (testSuite)

import Expect
import Regex
import String
import Utils.Test exposing (describe, test)
import Utils.Blank exposing (..)


testSuite =
    describe "About Regex"
        [ test "contains tests for a regex match" <|
            \() ->
                True
                    |> Expect.equal (Regex.contains (Regex.regex "[abc]+") "abcdefg")
        , test "escape will escape all special characters" <|
            \() ->
                "\\[abc\\]\\+"
                    |> Expect.equal (Regex.escape "[abc]+")
        , test "useful for matching strange strings" <|
            \() ->
                True
                    |> Expect.equal (Regex.contains (Regex.regex (Regex.escape "[eir]+")) "w[eir]+d")
        , test "it's easy to make your regex case insensitive" <|
            \() ->
                True
                    |> Expect.equal (Regex.contains (Regex.caseInsensitive (Regex.regex "abc")) "ABC")
        , test "find returns a list of all matches" <|
            \() ->
                3
                    |> Expect.equal (Regex.find Regex.All (Regex.regex "abc") "abcabcabc" |> List.length)
        , test "matches contain the match" <|
            \() ->
                [ "a", "b", "c" ]
                    |> Expect.equal (Regex.find Regex.All (Regex.regex "[abc]") "axbxc" |> List.map (\match -> match.match))
        , test "matches contain the index" <|
            \() ->
                [ 0, 2, 4 ]
                    |> Expect.equal (Regex.find Regex.All (Regex.regex "[abc]") "axbxc" |> List.map (\match -> match.index))
        , test "matches may contain submatches" <|
            \() ->
                [ [ Just "a", Nothing ], [ Nothing, Just "b" ] ]
                    |> Expect.equal (Regex.find Regex.All (Regex.regex "(a)|(b)") "axbxc" |> List.map (\match -> match.submatches))
        , test "you may limit the number of matches" <|
            \() ->
                [ "a", "b" ]
                    |> Expect.equal (Regex.find (Regex.AtMost 2) (Regex.regex "[abc]") "axbxc" |> List.map (\match -> match.match))
        , test "replace can modify based on the match" <|
            \() ->
                "AxBxC"
                    |> Expect.equal (Regex.replace Regex.All (Regex.regex "[abc]") (\match -> String.toUpper match.match) "axbxc")
        ]
