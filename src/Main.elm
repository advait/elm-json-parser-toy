module Main exposing (..)

import Dict exposing (Dict)


{-| Represents a union of possible JSON Values
-}
type JsonValue
    = JsonNull
    | JsonString String
    | JsonNumber Float
    | JsonArray (List JsonValue)
    | JsonObject (Dict String JsonValue)
