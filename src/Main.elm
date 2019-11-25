module Main exposing (..)

import Dict exposing (Dict)
import String exposing (length, slice)


{-| Represents a union of possible JSON Values
-}
type JsonValue
    = JsonNull
    | JsonString String
    | JsonNumber Float
    | JsonArray (List JsonValue)
    | JsonObject (Dict String JsonValue)


{-| A function that takes an input string and parses it. If the parsing is unsuccessful, Nothing is returned.
If it is successful, a Maybe type is returned containing the remainder string to parse as well as the parsed type
-}
type alias Parser a =
    String -> Maybe ( String, a )


{-| Given an integer N, tries to consume and return the first N characters from the input String, returning a tuple
of the consumed characters and the remaining characters. If the input is less than N characters, then the whole input
is consumed without errors
-}
take : Int -> String -> ( String, String )
take n s =
    let
        len =
            length s

        s1 =
            slice 0 n s

        s2 =
            slice n (len - n) s
    in
    ( s1, s2 )


jsonNullParser : Parser JsonValue
jsonNullParser s =
    let
        ( s1, s2 ) =
            take 4 s
    in
    if s1 == "null" then
        Just ( s2, JsonNull )

    else
        Nothing
