module MainTest exposing (..)

import Basics exposing (toFloat)
import Dict
import Expect
import Fuzz exposing (Fuzzer, intRange, string)
import Main exposing (..)
import Random
import String
import Test exposing (..)


suite : Test
suite =
    concat
        [ describe "charP"
            [ test "does not consume on mismatch" <| \_ -> Expect.equal Nothing (charP 'c' "hi")
            , test "consumes on match" <| \_ -> Expect.equal (Just ( "", 'c' )) (charP 'c' "c")
            , fuzz string "returns suffix on match" <| \s -> Expect.equal (Just ( s, 'c' )) (charP 'c' (String.cons 'c' s))
            , fuzz string "returns Nothing regardless of suffix" <| \s -> Expect.equal Nothing (charP 'c' (String.cons 'b' s))
            ]
        , describe
            "exceptCharP"
          <|
            let
                p1 =
                    exceptCharP '"'
            in
            [ test "consumes on mismatch" <| \_ -> Expect.equal (Just ( "", 'c' )) (p1 "c")
            , test "does not consume on match" <| \_ -> Expect.equal Nothing (p1 "\"")
            ]
        , describe
            "seqParsers"
          <|
            let
                p1 =
                    seqParsers [ charP 'c', charP 'a' ]
            in
            [ test "does not consume on mismatch" <| \_ -> Expect.equal Nothing (p1 "hello")
            , test "consumes on match" <| \_ -> Expect.notEqual Nothing (p1 "ca")
            , fuzz string "returns suffix on match" <| \s -> Expect.equal (Just ( s, [ 'c', 'a' ] )) (p1 ("ca" ++ s))
            , fuzz string "returns Nothing regardless of suffix" <| \s -> Expect.equal Nothing (p1 ("d" ++ s))
            ]
        , describe
            "atLeastZero"
          <|
            let
                cParser =
                    zeroOrMore (charP 'c') |> mapParser String.fromList
            in
            [ test "succeeds on empty string" <| \_ -> Expect.equal (Just ( "", "" )) (cParser "")
            , test "consumes a single char" <| \_ -> Expect.equal (Just ( "", "c" )) (cParser "c")
            , test "consumes multiple chars" <| \_ -> Expect.equal (Just ( "", "ccc" )) (cParser "ccc")
            , test "returns suffix" <| \_ -> Expect.equal (Just ( "d", "ccc" )) (cParser "cccd")
            ]
        , describe
            "delimitedBy"
          <|
            let
                p =
                    delimitedBy (charP ',') (charP 'c')
            in
            [ test "succeeds on empty string" <| \_ -> Expect.equal (Just ( "", [] )) (p "")
            , test "consumes a single char" <| \_ -> Expect.equal (Just ( "", [ 'c' ] )) (p "c")
            , test "consumes multiple chars" <| \_ -> Expect.equal (Just ( "cc", [ 'c' ] )) (p "ccc")
            , test "returns suffix" <| \_ -> Expect.equal (Just ( "", [ 'c', 'c' ] )) (p "c,c")
            ]
        , describe "jsonNullParser"
            [ test "null" <| \_ -> Expect.equal (Just ( "", JsonNull )) (jsonNullParser "null")
            , test "empty string" <| \_ -> Expect.equal Nothing (jsonNullParser "")
            , fuzz string "returns any suffix if starts with null" <| \s -> Expect.equal (Just ( s, JsonNull )) (jsonNullParser ("null" ++ s))
            , fuzz string "returns Nothing if not starts with null" <| \s -> Expect.equal Nothing (jsonNullParser ("nope" ++ s))
            ]
        , describe "jsonStringParser"
            [ test "digit should not match" <| \_ -> Expect.equal Nothing (jsonStringParser "1")
            , test "space should match" <| \_ -> Expect.equal (Just ( "", JsonString " " )) (jsonStringParser "\" \"")
            , test "empty string should not match" <| \_ -> Expect.equal Nothing (jsonStringParser "")
            , test "empty JSON string should match" <| \_ -> Expect.equal (Just ( "", JsonString "" )) (jsonStringParser "\"\"")
            , fuzz string
                "arbitrary string bodies in quotes should match"
              <|
                \dirtyString ->
                    let
                        s =
                            dirtyString |> String.filter (\c -> c /= '"')
                    in
                    Expect.equal (Just ( "", JsonString s )) (jsonStringParser ("\"" ++ s ++ "\""))
            ]
        , describe "jsonNumberParser"
            [ test "empty string" <| \_ -> Expect.equal Nothing (jsonNullParser "")
            , fuzz (intRange 0 Random.maxInt) "valid integers" <| \i -> Expect.equal (Just ( "", JsonNumber (toFloat i) )) (jsonNumberParser (String.fromInt i))
            ]
        , describe "jsonArrayParser"
            [ test "empty string" <| \_ -> Expect.equal Nothing (jsonArrayParser "")
            , test "empty array" <| \_ -> Expect.equal (Just ( "", JsonArray [] )) (jsonArrayParser "[]")
            , test "empty array with whitespace" <| \_ -> Expect.equal (Just ( "", JsonArray [] )) (jsonArrayParser "[ \t\n ]")
            , test "single element" <| \_ -> Expect.equal (Just ( "", JsonArray [ JsonNull ] )) (jsonArrayParser "[ null ]")
            , test "multiple elements" <| \_ -> Expect.equal (Just ( "", JsonArray [ JsonNull, JsonNumber 1 ] )) (jsonArrayParser "[ null, 1 ]")
            , test "nested arrays" <| \_ -> Expect.equal (Just ( "", JsonArray [ JsonArray [ JsonNull ], JsonNumber 1 ] )) (jsonArrayParser "[ [null], 1 ]")
            ]
        , describe "jsonObjectParser"
            [ test "empty string" <| \_ -> Expect.equal Nothing (jsonArrayParser "")
            , test "empty object" <| \_ -> Expect.equal (Just ( "", JsonObject Dict.empty )) (jsonObjectParser "{}")
            , test "empty object with whitespace" <| \_ -> Expect.equal (Just ( "", JsonObject Dict.empty )) (jsonObjectParser "{ \t\n }")
            , test "single element" <| \_ -> Expect.equal (Just ( "", JsonObject (Dict.fromList [ ( "hello", JsonString "world" ) ]) )) (jsonObjectParser "{ \"hello\" : \"world\" }")
            , test "nested objects" <| \_ -> Expect.equal (Just ( "", JsonObject (Dict.fromList [ ( "hello", JsonObject (Dict.fromList [ ( "world", JsonNull ) ]) ) ]) )) (jsonObjectParser "{ \"hello\": {\"world\": null} }")
            ]
        ]
