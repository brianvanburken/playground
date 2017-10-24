module Koans08AboutMaybe exposing (testSuite)

import Expect
import Utils.Test exposing (describe, test)


testSuite =
    describe "About Maybe"
        [ test "maybe represents a value that may be nothing" <|
            \() ->
                Nothing
                    |> Expect.equal Nothing
        , test "or the value could be something" <|
            \() ->
                Just 5
                    |> Expect.equal (Just 5)
        , test "withDefault can be used to get the value from a maybe" <|
            \() ->
                5
                    |> Expect.equal (Maybe.withDefault 3 (Just 5))
        , test "but will give the default value if there is nothing" <|
            \() ->
                3
                    |> Expect.equal (Maybe.withDefault 3 Nothing)
        , test "map will transform the value in a maybe" <|
            \() ->
                Just 2
                    |> Expect.equal (Maybe.map (\n -> n / 2) (Just 4))
        , test "but will not transform a nothing" <|
            \() ->
                Nothing
                    |> Expect.equal (Maybe.map (\n -> n / 2) Nothing)
        ]
