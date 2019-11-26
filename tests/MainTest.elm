module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "jsonNullParser"
        [ test "null" <| \_ -> Expect.equal (Just ( "", JsonNull )) (jsonNullParser "null")
        , test "empty string" <| \_ -> Expect.equal Nothing (jsonNullParser "")
        , fuzz string "returns any suffix if starts with null" <| \s -> Expect.equal (Just ( s, JsonNull )) (jsonNullParser ("null" ++ s))
        , fuzz string "returns Nothing if not starts with null" <| \s -> Expect.equal Nothing (jsonNullParser ("nope" ++ s))
        ]
