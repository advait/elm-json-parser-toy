module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "jsonNullParser"
        [ test "null" <| \_ -> Expect.equal (Just ( "", JsonNull )) (jsonNullParser "null")
        , test "nulls" <| \_ -> Expect.equal (Just ( "s", JsonNull )) (jsonNullParser "nulls")
        , test "empty string" <| \_ -> Expect.equal Nothing (jsonNullParser "")
        ]
