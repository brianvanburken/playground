module Koans18AboutSets exposing (testSuite)

import Expect
import Set
import Utils.Test exposing (describe, test)
import Utils.Blank exposing (..)


assertEqualSets set1 set2 =
    Expect.equal (Set.toList set1) (Set.toList set2)


oneTwoThree =
    Set.fromList [ 1, 2, 3 ]


testSuite =
    describe "About Sets"
        [ test "a set can be created from a list" <|
            \() ->
                Set.fromList [ 1, 2, 3 ]
                    |> assertEqualSets (Set.fromList [ 1, 2, 3 ])
        , test "a set cannot contain duplicates" <|
            \() ->
                Set.fromList [ 1, 2, 3 ]
                    |> assertEqualSets (Set.fromList [ 1, 2, 3, 3, 2, 1 ])
        , test "sets may be empty" <|
            \() ->
                Set.empty
                    |> assertEqualSets Set.empty
        , test "or contain a single value" <|
            \() ->
                Set.fromList [ 0 ]
                    |> assertEqualSets (Set.singleton 0)
        , test "insert may add a new element to a set" <|
            \() ->
                Set.fromList [ 1, 2, 3, 4 ]
                    |> assertEqualSets (Set.insert 4 oneTwoThree)
        , test "but may not add duplicates" <|
            \() ->
                Set.fromList [ 1, 2, 3 ]
                    |> assertEqualSets (Set.insert 1 oneTwoThree)
        , test "remove may subtract an element from a set" <|
            \() ->
                Set.fromList [ 2, 3 ]
                    |> assertEqualSets (Set.remove 1 oneTwoThree)
        , test "but only if it is there" <|
            \() ->
                Set.fromList [ 1, 2, 3 ]
                    |> assertEqualSets (Set.remove 4 oneTwoThree)
        , test "member can check if an element is in a set" <|
            \() ->
                True
                    |> Expect.equal (Set.member 2 oneTwoThree)
        , test "union will add two sets" <|
            \() ->
                Set.fromList [ 1, 2, 3, 4 ]
                    |> assertEqualSets (Set.union oneTwoThree (Set.fromList [ 3, 4 ]))
        , test "intersect will get the intersection" <|
            \() ->
                Set.fromList [ 3 ]
                    |> assertEqualSets (Set.intersect oneTwoThree (Set.fromList [ 3, 4 ]))
        , test "diff is the difference between the first and second sets" <|
            \() ->
                Set.fromList [ 2 ]
                    |> assertEqualSets (Set.diff oneTwoThree (Set.fromList [ 1, 3 ]))
        ]
