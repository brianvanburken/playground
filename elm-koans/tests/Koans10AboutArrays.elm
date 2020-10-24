module Koans10AboutArrays exposing (testSuite)

import Array exposing (fromList)
import Expect
import Utils.Test exposing (describe, test)


testSuite =
    describe "About Arrays"
        [ test "arrays can be created from a list" <|
            \() ->
                fromList ([ 0, 1 ])
                    |> Expect.equal (fromList [ 0, 1 ])
        , test "arrays can also be turned into lists" <|
            \() ->
                [ 0, 1 ]
                    |> Expect.equal (Array.toList <| fromList [ 0, 1 ])
        , test "length gives the number of elements" <|
            \() ->
                3
                    |> Expect.equal (Array.length <| fromList [ 0, 1, 2 ])
        , test "values can be pushed on to an array" <|
            \() ->
                fromList ([ 1, 2, 3 ])
                    |> Expect.equal (Array.push 3 <| fromList [ 1, 2 ])
        , test "or two arrays can become one" <|
            \() ->
                fromList ([ 1, 2, 3 ])
                    |> Expect.equal (Array.append (fromList [ 1, 2 ]) (fromList [ 3 ]))
        , test "it is easy to create an empty array" <|
            \() ->
                fromList ([ 1 ])
                    |> Expect.equal (Array.push 1 <| Array.empty)
        , test "or to create an array of all the same value" <|
            \() ->
                fromList ([ "a", "a", "a" ])
                    |> Expect.equal (Array.repeat 3 "a")
        , test "initialize creates an array based on the index" <|
            \() ->
                fromList ([ 0, 2, 4 ])
                    |> Expect.equal (Array.initialize 3 (\i -> i * 2))
        , test "you can test whether an array is empty" <|
            \() ->
                True
                    |> Expect.equal (Array.isEmpty Array.empty)
        , test "mapping is just like with lists"
          -- so is indexedMap, filter, foldl, foldr
          <|
            \() ->
                fromList ([ 0, 3, 6 ])
                    |> Expect.equal (Array.map (\i -> i * 3) <| fromList [ 0, 1, 2 ])
        , test "unlike with lists, you can get the value at an index" <|
            \() ->
                Just (1)
                    |> Expect.equal (Array.get 1 <| fromList [ 0, 1, 2 ])
        , test "and set the value at an index" <|
            \() ->
                fromList ([ 0, 5, 2 ])
                    |> Expect.equal (Array.set 1 5 <| fromList [ 0, 1, 2 ])
        , test "or get a slice of the array" <|
            \() ->
                fromList ([ 1, 2, 3 ])
                    |> Expect.equal (Array.slice 1 4 <| fromList [ 0, 1, 2, 3, 4 ])
        ]
