module Koans19AboutRecords exposing (testSuite)

import Expect
import Utils.Test exposing (describe, test)


point =
    { x = 1, y = 2 }


testSuite =
    describe "About Records"
        [ test "a record is a set of named fields" <|
            \() ->
                { x = 1, y = 2 }
                    |> Expect.equal { x = 1, y = 2 }
        , test "you can access a field with dot notation" <|
            \() ->
                1
                    |> Expect.equal point.x
        , test "the dot notation may also be used as a function" <|
            \() ->
                2
                    |> Expect.equal (.y point)
        , test "fields may be updated" <|
            \() ->
                { x = 3, y = 2 }
                    |> Expect.equal { point | x = 3 }
        ]
