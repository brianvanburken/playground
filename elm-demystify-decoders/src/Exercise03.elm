module Exercise03 exposing (decoder)

import Json.Decode exposing (Decoder, list, string, map)


{- So, this one is a tiny bit more challenging, since we don't want _just_ the
   contents of the JSON. No, we want them to be modified.

   input:

       var input = [ "foo", "bar" ];

   output:
       [ "FOO", "BAR" ]

-}


decoder : Decoder (List String)
decoder =
    list (string |> map String.toUpper)



{- Once you think you're done, run the tests for this exercise from the root of
   the project:

   - If you have installed `elm-test` globally:
        `elm test tests/Exercise03`

   - If you have installed locally using `npm`:
        `npm run elm-test tests/Exercise03`

   - If you have installed locally using `yarn`:
        `yarn elm-test tests/Exercise03`
-}
