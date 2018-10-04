module Athlete exposing (Athlete, athleteDecoder)

import Json.Decode as Decode exposing (Decoder, int, string)


type alias Athlete =
    { id : Int
    , firstname : String
    , profile : String
    , profile_medium : String
    }


athleteDecoder : Decoder Athlete
athleteDecoder =
    Decode.map4
        Athlete
        (Decode.field "id" int)
        (Decode.field "firstname" string)
        (Decode.field "profile" string)
        (Decode.field "profile_medium" string)
