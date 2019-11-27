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
mapParsed f parseResult =
    case parseResult of
        Nothing ->
            Nothing

        Just ( tail, a ) ->
            Just ( tail, f a )


{-| Applies the transformation function f to `Parser a` returning a `Parser b`
-}
mapParser : (a -> b) -> Parser a -> Parser b
mapParser f pA s =
    case pA s of
        Just ( tail, a ) ->
            Just ( tail, f a )

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

                Just ( tail, a ) ->
                    seqParsers ps tail |> mapParsed (\ays -> a :: ays)


{-| Returns a parser for the given Char c
-}
charP : Char -> Parser Char
charP c s =
    case String.uncons s of
        Nothing ->
            Nothing

        Just ( head, tail ) ->
            if c /= head then
                Nothing

            else
                Just ( tail, head )


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
    parser |> mapParser String.fromList


{-| Returns a parser that matches the input for a single char that is not c
-}
exceptCharP : Char -> Parser Char
exceptCharP c s =
    case String.uncons s of
        Nothing ->
            Nothing

        Just ( head, tail ) ->
            if c == head then
                Nothing

            else
                Just ( tail, c )


{-| Given a parser, return a new parser that tries to repeatedly consume the input given the input parser until no
more matches, returning the final suffix.
-}
zeroOrMore : Parser a -> Parser (List a)
zeroOrMore p s =
    case p s of
        Nothing ->
            Just ( s, [] )

        Just ( tail, a ) ->
            zeroOrMore p tail |> mapParsed (\ays -> a :: ays)


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


{-| Given a delimiter parser, and a core parser, parse delimited sequence returning a list of core items
-}
delimitedBy : Parser b -> Parser a -> Parser (List a)
delimitedBy =
    Debug.todo "Implement"


{-| Given a parser, return a new parser that tolerates, consumes, and drops whitespace on either side
-}
wrappedWhitespace : Parser a -> Parser a
wrappedWhitespace =
    Debug.todo "Implement"


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

        -- Note that withDefault case should never happen
        stringToFloat s =
            String.toFloat s |> Maybe.withDefault -999

        numberParser =
            oneOrMore singleDigitParser |> mapParser String.fromList |> mapParser stringToFloat
    in
    numberParser |> mapParser JsonNumber
