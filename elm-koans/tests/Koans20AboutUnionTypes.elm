module Koans20AboutUnionTypes exposing (testSuite)

import Expect
import Utils.Test exposing (describe, test)
import Utils.Blank exposing (..)


type Nucleotide
    = A
    | C
    | G
    | T


type DNA
    = Base Nucleotide
    | Strand (List Nucleotide)


testSuite =
    describe "About UnionTypes"
        [ test "simple types are similar to enums in other languages" <|
            \() ->
                C
                    |> Expect.equal C
        , test "more complex types can be built with a 'tag' and additional data" <|
            \() ->
                Base (C)
                    |> Expect.equal (Base C)
        , test "all types in the union type are the same type" <|
            \() ->
                Base A
                    |> Expect.equal (Base A)
        , test "case statements may be used to extract the data from the type" <|
            case Base A of
                Strand nucleotides ->
                    \() ->
                        [ A ]
                            |> Expect.equal nucleotides

                Base nucleotide ->
                    \() ->
                        A
                            |> Expect.equal nucleotide
        ]
