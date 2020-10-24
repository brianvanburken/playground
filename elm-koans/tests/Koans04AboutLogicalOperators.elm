module Koans04AboutLogicalOperators exposing (testSuite)

import Expect
import Utils.Test exposing (describe, test)


testSuite =
    describe "About Logical Operators"
        [ test "not negates a boolean" <|
            \() ->
                not (True)
                    |> Expect.equal False
        , test "&& is a logical AND" <|
            \() ->
                True
                    && True
                    |> Expect.true "Should be True"
        , test "|| is a logical OR" <|
            \() ->
                False
                    || True
                    |> Expect.true "Should be True"
        , test "xor is a logical XOR" <|
            \() ->
                False
                    |> xor (True)
                    |> Expect.true "Should be True"
        ]
