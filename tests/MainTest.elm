module MainTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer, string)
import Main exposing (..)
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
            "seqParsers"
            (let
                p =
                    seqParsers [ charP 'c', charP 'a' ]
             in
             [ test "does not consume on mismatch" <| \_ -> Expect.equal Nothing (p "hello")
             , test "consumes on match" <| \_ -> Expect.notEqual Nothing (p "ca")
             , fuzz string "returns suffix on match" <| \s -> Expect.equal (Just ( s, [ 'c', 'a' ] )) (p ("ca" ++ s))
             , fuzz string "returns Nothing regardless of suffix" <| \s -> Expect.equal Nothing (p ("d" ++ s))
             ]
            )
        , describe
            "atLeastZero"
            (let
                p =
                    zeroOrMore (charP 'c') |> mapParser String.fromList
             in
             [ test "succeeds on empty string" <| \_ -> Expect.equal (Just ( "", "" )) (p "")
             , test "consumes a single char" <| \_ -> Expect.equal (Just ( "", "c" )) (p "c")
             , test "consumes multiple chars" <| \_ -> Expect.equal (Just ( "", "ccc" )) (p "ccc")
             , test "returns suffix" <| \_ -> Expect.equal (Just ( "d", "ccc" )) (p "cccd")
             ]
            )
        , describe "jsonNullParser"
            [ test "null" <| \_ -> Expect.equal (Just ( "", JsonNull )) (jsonNullParser "null")
            , test "empty string" <| \_ -> Expect.equal Nothing (jsonNullParser "")
            , fuzz string "returns any suffix if starts with null" <| \s -> Expect.equal (Just ( s, JsonNull )) (jsonNullParser ("null" ++ s))
            , fuzz string "returns Nothing if not starts with null" <| \s -> Expect.equal Nothing (jsonNullParser ("nope" ++ s))
            ]
        ]
