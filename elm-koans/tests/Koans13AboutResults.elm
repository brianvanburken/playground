module Koans13AboutResults exposing (testSuite)

import Expect
import Utils.Test exposing (describe, test)


testSuite =
    describe "About Result"
        [ test "results represent the result of a function" <|
            \() ->
                Ok 42
                    |> Expect.equal (Ok 42)
        , test "but could be an error" <|
            \() ->
                Err "there was an error"
                    |> Expect.equal (Err "there was an error")
        , test "a result can be converted to a maybe" <|
            \() ->
                Just 42
                    |> Expect.equal (Result.toMaybe (Ok 42))
        , test "but an error will become nothing" <|
            \() ->
                Nothing
                    |> Expect.equal (Result.toMaybe (Err "there was an error"))
        , test "a maybe can also be converted to a result" <|
            \() ->
                Ok 42
                    |> Expect.equal (Result.fromMaybe "there was an error" (Just 42))
        , test "and will become an error if there is nothing" <|
            \() ->
                Err "there was an error"
                    |> Expect.equal (Result.fromMaybe "there was an error" Nothing)
        ]
