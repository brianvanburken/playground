module Koans03AboutComparisonOperators exposing (testSuite)

import Expect
import Utils.Test exposing (describe, test)


testSuite =
    describe "About Comparison Operators"
        [ test "== tests for equality" <|
            \() ->
                (1 == 1)
                    |> Expect.true "Should be True"
        , test "/= tests for inequality" <|
            \() ->
                (-1 /= 1)
                    |> Expect.true "Should be True"
        , test "< tests for less than" <|
            \() ->
                (1 < 2)
                    |> Expect.true "Should be True"
        , test "<= tests for less than or equal to" <|
            \() ->
                (1 <= 1)
                    |> Expect.true "Should be True"
        , test "> tests for greater than" <|
            \() ->
                (1 > 0)
                    |> Expect.true "Should be True"
        , test ">= tests for greater than or equal to" <|
            \() ->
                (1 >= 1)
                    |> Expect.true "Should be True"
        , test "Floats are comparable" <|
            \() ->
                (1.5 >= 1.4)
                    |> Expect.true "Should be True"
        , test "Strings are comparable" <|
            \() ->
                ("A string" == "A string")
                    |> Expect.true "Should be True"
        , test "Chars are comparable" <|
            \() ->
                ('a' == 'a')
                    |> Expect.true "Should be True"
        , test "max returns the maximum of two comparables" <|
            \() ->
                2
                    |> Expect.equal (max 1 2)
        , test "min returns the minimum of two comparables" <|
            \() ->
                1
                    |> Expect.equal (min 1 2)
        , test "compare returns an Order" <|
            -- valid Order values are LT, EQ, and GT
            \() ->
                LT
                    |> Expect.equal (compare 1 2)
        ]
