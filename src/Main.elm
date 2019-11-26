module Main exposing (..)

import Dict exposing (Dict)
import Maybe
import String exposing (dropLeft, length, startsWith)


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


{-| Try to consume the needs from the start of the haystack, maybe returning the remainder of the haystack.
-}
consume : String -> String -> Maybe String
consume needle haystack =
    case startsWith needle haystack of
        True ->
            Just (dropLeft (length needle) haystack)

        False ->
            Nothing


jsonNullParser : Parser JsonValue
jsonNullParser s =
    Maybe.map (\s_ -> ( s_, JsonNull )) (consume "null" s)
