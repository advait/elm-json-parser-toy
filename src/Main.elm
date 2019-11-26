module Main exposing (..)

import Debug
import Dict exposing (Dict)
import Maybe
import String


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


{-| Applies the transformation function f to the parsed result
-}
mapParsed : (a -> b) -> Maybe ( String, a ) -> Maybe ( String, b )
mapParsed f res =
    case res of
        Nothing ->
            Nothing

        Just ( s, a ) ->
            Just ( s, f a )


{-| Applies the transformation function f to `Parser a` returning a `Parser b`
-}
mapParser : (a -> b) -> Parser a -> Parser b
mapParser f pA s =
    case pA s of
        Just ( s_, a ) ->
            Just ( s_, f a )

        Nothing ->
            Nothing


seqParsers : List (Parser a) -> Parser (List a)
seqParsers parsers s =
    case parsers of
        -- Empty parsers does not match on anything
        [] ->
            Nothing

        -- If we have a single parser, just wrap the result parsed in a list
        [ p ] ->
            p s |> mapParsed (\a -> [ a ])

        -- If we have multiple parsers, recursively apply them
        p :: ps ->
            case p s of
                Nothing ->
                    Nothing

                Just ( nextS, a ) ->
                    case seqParsers ps nextS of
                        Nothing ->
                            Nothing

                        Just ( finalS, ays ) ->
                            Just ( finalS, a :: ays )


{-| Returns a parser for the given Char c
-}
charP : Char -> Parser Char
charP c s =
    case String.uncons s of
        Nothing ->
            Nothing

        Just ( c_, s_ ) ->
            if c /= c_ then
                Nothing

            else
                Just ( s_, c_ )


{-| Returns a parser for the given String S
-}
stringP : String -> Parser String
stringP needle =
    let
        charParsers =
            String.toList needle |> List.map charP

        parser =
            seqParsers charParsers
    in
    mapParser String.fromList parser


jsonNullParser : Parser JsonValue
jsonNullParser =
    stringP "null" |> mapParser (always JsonNull)
