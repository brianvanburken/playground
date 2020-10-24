module Koans05AboutMathematicalOperators exposing (testSuite)

import Expect
import Utils.Test exposing (describe, test)
import Utils.Blank exposing (..)


testSuite =
    describe "About Mathematical Operators"
        [ test "negate gives the negative of a number" <|
            \() ->
                negate (3)
                    |> Expect.equal -3
        , test "abs gives the absolute value of a number" <|
            \() ->
                abs (-3)
                    |> Expect.equal 3
        , test "sqrt gives the square root of a Float" <|
            \() ->
                sqrt (9)
                    |> Expect.equal 3.0
        , test "+ adds numbers" <|
            \() ->
                (1 + 2)
                    |> Expect.equal 3
        , test "- subtracts numbers" <|
            \() ->
                (7 - 4)
                    |> Expect.equal 3
        , test "* multiplies numbers" <|
            \() ->
                (4 * 1.5)
                    |> Expect.equal 6
        , test "/ divides Floats" <|
            \() ->
                (5 / 2)
                    |> Expect.equal 2.5
        , test "// divides Ints" <|
            \() ->
                (5 // 2)
                    |> Expect.equal 2
        , test "rem gives the remainder after division" <|
            \() ->
                5
                    |> rem (2)
                    |> Expect.equal 2
        , test "% performs modular arithmetic (which is different)" <|
            \() ->
                4
                    |> Expect.equal (-1 % 5)
        , test "logBase returns the log of the value in the given base" <|
            \() ->
                3
                    |> Expect.equal (logBase 2 8)
        , test "clamp returns the value if it is between the given min and max" <|
            \() ->
                5
                    |> Expect.equal (clamp 1 10 5)
        , test "clamp returns the min value if the value is below the minimum" <|
            \() ->
                1
                    |> Expect.equal (clamp 1 10 -5)
        , test "clamp returns the max value if the value is above the maximum" <|
            \() ->
                10
                    |> Expect.equal (clamp 1 10 50)
        ]
