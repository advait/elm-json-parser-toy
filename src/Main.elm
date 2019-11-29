module Main exposing (..)

import Debug
import Dict exposing (Dict)
import Maybe
import String
import Tuple


{-| Represents a union of possible JSON Values
-}
type JsonValue
    = JsonNull
    | JsonBool Bool
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
mapParser f parser input =
    parser input |> mapParsed f


{-| Apply a List of parsers in sequence returning a list of the parsed results.
-}
seqParsers : List (Parser a) -> Parser (List a)
seqParsers parsers input =
    case parsers of
        -- Empty parsers does not match on anything
        [] ->
            Nothing

        -- If we have a single parser, just wrap the result parsed in a list
        [ p ] ->
            p input |> mapParsed (\ret -> [ ret ])

        -- If we have multiple parsers, recursively apply them in sequence
        p :: parsersTails ->
            case p input of
                Nothing ->
                    Nothing

                Just ( inputTail, ret ) ->
                    seqParsers parsersTails inputTail |> mapParsed (\rets -> ret :: rets)


{-| nullP is a Parser that always matches the input.
-}
nullP : Parser ()
nullP input =
    Just ( input, () )


{-| Returns a parser for the given Char c.
-}
charP : Char -> Parser Char
charP char input =
    case String.uncons input of
        Nothing ->
            Nothing

        Just ( head, tail ) ->
            if char /= head then
                Nothing

            else
                Just ( tail, head )


{-| Returns a parser for the given String needle.
-}
stringP : String -> Parser String
stringP needle =
    let
        charParsers =
            String.toList needle |> List.map charP
    in
    seqParsers charParsers |> mapParser String.fromList


{-| Returns a parser that matches the input for a single char that is not c
-}
exceptCharP : Char -> Parser Char
exceptCharP char input =
    case String.uncons input of
        Nothing ->
            Nothing

        Just ( head, tail ) ->
            if char == head then
                Nothing

            else
                Just ( tail, head )


{-| Given a parser, return a new parser that tries to repeatedly consume the input given the input parser until no
more matches, returning the final suffix.
-}
zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser input =
    case parser input of
        Nothing ->
            Just ( input, [] )

        Just ( tail, ret ) ->
            zeroOrMore parser tail |> mapParsed (\rets -> ret :: rets)


{-| Given a parser, return a new parser that consumes the input one or more times.
-}
oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    seqParsers [ parser |> mapParser (\ret -> [ ret ]), zeroOrMore parser ] |> mapParser List.concat


{-| Given a list of parsers, proxy to the first parser that matches.
-}
anyOneOf : List (Parser a) -> Parser a
anyOneOf parsers input =
    case parsers of
        [] ->
            Nothing

        parser :: parsersTail ->
            case parser input of
                Nothing ->
                    anyOneOf parsersTail input

                match ->
                    match


{-| Given a delimiter parser, and a core parser, parse delimited sequence returning a list of core items.
-}
delimitedBy : Parser b -> Parser a -> Parser (List a)
delimitedBy delimiterP elementP =
    let
        -- Matches the head of a multi-element sequence.
        headElementParser =
            elementP |> mapParser (\ret -> [ ret ])

        -- Matches the tail of a multi-element sequence (sequence of delimiter+element pairs).
        tailElementsParser =
            discard delimiterP elementP |> zeroOrMore

        -- Stitches the headElementParser and tailElementParser.
        multiElementParser =
            seqParsers [ headElementParser, tailElementsParser ] |> mapParser List.concat

        -- Matches empty sequences. Always successfully matches.
        fallbackParser input =
            Just ( input, [] )
    in
    anyOneOf [ multiElementParser, fallbackParser ]


{-| Runs Parser a, from the output, runs Parser b, discarding the output of a and returning the result of Parser b.
-}
discard : Parser a -> Parser b -> Parser b
discard parserA parserB input =
    case parserA input of
        Nothing ->
            Nothing

        Just ( tail, _ ) ->
            parserB tail


{-| Runs Parser a, from the output, runs Parser b, returning the tail of Parser b but returning the parsed value of
Parser a.
-}
discardRight : Parser a -> Parser b -> Parser a
discardRight parserA parserB input =
    case parserA input of
        Nothing ->
            Nothing

        Just ( tail, ret ) ->
            parserB tail |> mapParsed (always ret)


{-| Returns a parser that's enclosed by a and c, discarding them, and returning the value of b.
-}
enclosedBy : Parser a -> Parser b -> Parser c -> Parser b
enclosedBy open body close =
    discardRight (discard open body) close


{-| Parses zero or more characters of whitespace.
-}
wsP : Parser ()
wsP =
    anyOneOf [ charP ' ', charP '\n', charP '\t' ] |> zeroOrMore |> mapParser (always ())


{-| Wraps a parser, consuming and discard whitespace on either side.
-}
enclosedByWhitespace : Parser b -> Parser b
enclosedByWhitespace parser =
    enclosedBy wsP parser wsP


{-| Given a Parser, return a new Parser that first consumes all whitespace, then defers to the provided parser
-}
wsLeft : Parser a -> Parser a
wsLeft parser s =
    case wsP s of
        Nothing ->
            Nothing

        Just ( tail, _ ) ->
            parser tail


{-| Given a Parser, return a new Parser that defers to the provided parser, then consumes all whitespace,
finally, returning the original Parser's value.
-}
wsRight : Parser a -> Parser a
wsRight parser s =
    case parser s of
        Nothing ->
            Nothing

        Just ( tail, ret ) ->
            wsP tail |> mapParsed (always ret)


{-| Parses the literal JSON null token.
-}
jsonNullParser : Parser JsonValue
jsonNullParser =
    stringP "null" |> mapParser (always JsonNull)


{-| Parses the literal JSON true and false booleans.
-}
jsonBoolParser : Parser JsonValue
jsonBoolParser =
    let
        trueParser =
            stringP "true" |> mapParser (always (JsonBool True))

        falseParser =
            stringP "false" |> mapParser (always (JsonBool False))
    in
    anyOneOf [ trueParser, falseParser ]


{-| Parses strings up until a double quote is reached.
-}
stringLiteralParser : Parser String
stringLiteralParser =
    let
        -- Consume all characters except quotes. Note that we don't support escape sequences.
        stringBodyParser =
            exceptCharP '"' |> zeroOrMore |> mapParser String.fromList
    in
    enclosedBy (charP '"') stringBodyParser (charP '"')


{-| Parses any JSON string.
-}
jsonStringParser : Parser JsonValue
jsonStringParser =
    stringLiteralParser |> mapParser JsonString


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


{-| Parser JSON Arrays.
-}
jsonArrayParser : Parser JsonValue
jsonArrayParser =
    let
        openBracket =
            charP '[' |> wsRight

        delimiter =
            charP ',' |> enclosedByWhitespace

        closeBacket =
            wsLeft <| charP ']'

        elementsParser =
            delimitedBy delimiter jsonValueParser
    in
    enclosedBy openBracket elementsParser closeBacket |> mapParser JsonArray


jsonObjectParser : Parser JsonValue
jsonObjectParser =
    let
        openBracket =
            charP '{' |> wsRight

        delimiter =
            charP ',' |> enclosedByWhitespace

        closeBacket =
            wsLeft <| charP '}'

        colonParser =
            charP ':' |> enclosedByWhitespace

        keyParser =
            discardRight stringLiteralParser colonParser

        -- TODO(advait): This is craving a flatMap-like operation where you can chain multiple parsers together
        -- where *both* the output string and the output value of parser 1 feed into parser 2.
        elementParser : Parser ( String, JsonValue )
        elementParser s =
            case keyParser s of
                Nothing ->
                    Nothing

                Just ( tail1, key ) ->
                    case jsonValueParser tail1 of
                        Nothing ->
                            Nothing

                        Just ( tail2, value ) ->
                            Just ( tail2, ( key, value ) )

        elementsParser =
            delimitedBy delimiter elementParser
    in
    enclosedBy openBracket elementsParser closeBacket |> mapParser Dict.fromList |> mapParser JsonObject


{-| Parser for any JsonValue
-}
jsonValueParser : Parser JsonValue
jsonValueParser =
    -- Elm does not allow for values to be recursively defined at compile time. Here we must insert a "lazy"
    -- recursive reference that evaluates at runtime, breaking the recursive compile time chain. This is why this
    -- function returns a lambda.
    -- See: https://github.com/elm/compiler/blob/master/hints/bad-recursion.md
    \s ->
        anyOneOf [ jsonNullParser, jsonBoolParser, jsonStringParser, jsonNumberParser, jsonArrayParser, jsonObjectParser ] s


{-| Final exported parser that returns parsed JSON.
-}
parseJson : String -> Maybe JsonValue
parseJson s =
    (jsonValueParser |> enclosedByWhitespace) s |> Maybe.map Tuple.second
