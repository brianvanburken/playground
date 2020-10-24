module Koans11AboutTuples exposing (testSuite)

import Expect
import Utils.Test exposing (describe, test)
import Tuple


testSuite =
    describe "About Tuples"
        [ test "tuples are like lists of fixed length" <|
            \() ->
                ( 1, 2 )
                    |> Expect.equal ( 1, 2 )
        , test "tuples may also be of mixed types" <|
            \() ->
                ( 1, "hey" )
                    |> Expect.equal ( 1, "hey" )
        , test "there is a special comma syntax for creating tuples" <|
            \() ->
                ( 1, "hey" )
                    |> Expect.equal ((,) 1 "hey")
        , test "you use as many commas as there would be in the tuple" <|
            \() ->
                ( 1, "hey", [] )
                    |> Expect.equal ((,,) 1 "hey" [])
        , test "first gets the first element of a 2-tuple" <|
            \() ->
                1
                    |> Expect.equal (Tuple.first ( 1, 2 ))
        , test "second gets the second element of a 2-tuple" <|
            \() ->
                2
                    |> Expect.equal (Tuple.second ( 1, 2 ))
        , test "case statements may be used to destructure a tuple" <|
            \() ->
                case ( 1, 2 ) of
                    ( first, second ) ->
                        (first == 1 && second == 2)
                            |> Expect.true "Should be True"
        , test "tuples may also be destructured by function arguments" <|
            \() ->
                ( 1, 2 )
                    |> (\( f, s ) -> f == 1 && s == 2)
                    |> Expect.true "Should be True"
        ]
