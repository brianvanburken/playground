module Exercise10 exposing (decoder, Person, PersonDetails, Role(..))

import Json.Decode exposing (fail, Decoder, map2, map3, field, string, succeed, andThen, list)


{- Let's try and do a complicated decoder, this time. No worries, nothing new
   here: applying the techniques you've used in the previous decoders should
   help you through this one.

   A couple of pointers:
    - try working "inside out". Write decoders for the details and role first
    - combine those decoders + the username and map them into the Person constructor
    - finally, wrap it all together to build it into a list of people


   Example input:

        [ { "username": "Phoebe"
          , "role": "regular"
          , "details":
            { "registered": "yesterday"
            , "aliases": [ "Phoebs" ]
            }
          }
        ]
-}


type alias Person =
    { username : String
    , role : Role
    , details : PersonDetails
    }


type alias PersonDetails =
    { registered : String
    , aliases : List String
    }


type Role
    = Newbie
    | Regular
    | OldFart


decoder : Decoder (List Person)
decoder =
    list personDecoder


personDecoder : Decoder Person
personDecoder =
    map3 Person
        (field "username" string)
        (field "role" roleDecoder)
        (field "details" personDetailsDecoder)


roleDecoder : Decoder Role
roleDecoder =
    string |> andThen decodeRole


decodeRole : String -> Decoder Role
decodeRole role =
    case role of
        "regular" ->
            succeed Regular

        "newbie" ->
            succeed Newbie

        "old_fart" ->
            succeed OldFart

        _ ->
            fail "Unkown role"


personDetailsDecoder : Decoder PersonDetails
personDetailsDecoder =
    map2 PersonDetails
        (field "registered" string)
        (field "aliases" (list string))



{- Once you think you're done, run the tests for this exercise from the root of
   the project:

   - If you have installed `elm-test` globally:
        `elm test tests/Exercise10`

   - If you have installed locally using `npm`:
        `npm run elm-test tests/Exercise10`

   - If you have installed locally using `yarn`:
        `yarn elm-test tests/Exercise10`
-}
