module Koans16AboutTime exposing (testSuite)

import Expect
import Utils.Test exposing (describe, test)
import Time


testSuite =
    describe "About Time"
        [ test "time is just a Float" <|
            \() ->
                123.45
                    |> Expect.equal 123.45
        , test "a constant exists for hour" <|
            \() ->
                3600000
                    |> Expect.equal (1 * Time.hour)
        , test "and minute" <|
            \() ->
                60000
                    |> Expect.equal (1 * Time.minute)
        , test "and second" <|
            \() ->
                1000
                    |> Expect.equal (1 * Time.second)
        , test "and millisecond" <|
            \() ->
                1
                    |> Expect.equal (1 * Time.millisecond)
        , test "helpers exist to convert back to Floats" <|
            \() ->
                1
                    |> Expect.equal (Time.inSeconds 1000)
        ]
