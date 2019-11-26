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
    | JsonNumber Float -- TODO(advait): Support decimals?
    | JsonArray (List JsonValue)
    | JsonObject (Dict String JsonValue)


{-| A function that takes an input string and parses it. If the parsing is unsuccessful, Nothing is returned.
If it is successful, a Maybe type is returned containing the remainder string to parse as well as the parsed type
-}
type alias Parser a =
    String -> Maybe ( String, a )


{-| Applies the transformation function f to the parsed result, keeping the suffix.
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


{-| Apply a List of parsers in sequence returning a list of the parsed results
-}
seqParsers : List (Parser a) -> Parser (List a)
seqParsers parsers s =
    case parsers of
        -- Empty parsers does not match on anything
        [] ->
            Nothing

        -- If we have a single parser, just wrap the result parsed in a list
        [ p ] ->
            p s |> mapParsed (\a -> [ a ])

        -- If we have multiple parsers, recursively apply them in sequence
        p :: ps ->
            case p s of
                Nothing ->
                    Nothing

                Just ( nextS, a ) ->
                    seqParsers ps nextS |> mapParsed (\ays -> a :: ays)


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


{-| Returns a parser that matches the input for a single char that is not c
-}
exceptCharP : Char -> Parser Char
exceptCharP c s =
    case String.uncons s of
        Nothing ->
            Nothing

        Just ( c_, s_ ) ->
            if c == c_ then
                Nothing

            else
                Just ( s_, c_ )


{-| Given a parser, return a new parser that tries to repeatedly consume the input given the input parser until no
more matches, returning the final suffix.
-}
zeroOrMore : Parser a -> Parser (List a)
zeroOrMore p s =
    case p s of
        Nothing ->
            Just ( s, [] )

        Just ( nextS, a ) ->
            zeroOrMore p nextS |> mapParsed (\ays -> a :: ays)


{-| Given a parser, return a new parser that consumes the input one or more times.
-}
oneOrMore : Parser a -> Parser (List a)
oneOrMore p =
    seqParsers [ p |> mapParser (\a -> [ a ]), zeroOrMore p ] |> mapParser List.concat


{-| Given a list of parsers, proxy to the first parser that matches.
-}
anyOneOf : List (Parser a) -> Parser a
anyOneOf ps s =
    case ps of
        [] ->
            Nothing

        p :: remainingPs ->
            case p s of
                Nothing ->
                    anyOneOf remainingPs s

                match ->
                    match


{-| Parses the literal JSON null token.
-}
jsonNullParser : Parser JsonValue
jsonNullParser =
    stringP "null" |> mapParser (always JsonNull)


{-| Parses any JSON string.
-}
jsonStringParser : Parser JsonValue
jsonStringParser =
    let
        -- Parse opening and closing quotes, but drops the quote
        quoteParser =
            charP '"' |> mapParser (\_ -> "")

        -- Parses the body of the string
        bodyParser =
            exceptCharP '"' |> zeroOrMore |> mapParser String.fromList
    in
    seqParsers [ quoteParser, bodyParser, quoteParser ] |> mapParser String.concat |> mapParser JsonString


{-| Parses JSON numbers. Note that decimals, negatives, and floating points are not supported.
-}
jsonNumberParser : Parser JsonValue
jsonNumberParser =
    let
        singleDigitParser =
            anyOneOf [ charP '0', charP '1', charP '2', charP '3', charP '4', charP '5', charP '6', charP '7', charP '8', charP '9' ]

        stringToFloat s =
            String.toFloat s |> Maybe.withDefault -999

        -- Note that withDefault case should never happen
        numberParser =
            oneOrMore singleDigitParser |> mapParser String.fromList |> mapParser stringToFloat
    in
    numberParser |> mapParser JsonNumber
