Elm JSON Parser
===============

This is an experimental JSON parser written in Elm. The parser is built via [parser combinators](https://en.wikipedia.org/wiki/Parser_combinator).

The Parser is a basic function that takes an input string and parses it. If the parsing is unsuccessful, Nothing is returned.
If it is successful, a Maybe type is returned containing the remainder string to parse as well as the parsed type.
-}
type alias Parser a =
    String -> Maybe ( String, a )

```elm
type alias Parser a =
    String -> Maybe ( String, a )
```

Possible improvements maybe to:
- Keep track of the input character positions for better error reporting.
- Support decimals
- Support string escaping
- Add fuzz tests that probabilistically generates random JsonValues, serializes them to JSON, parses them back to JsonValues, and asserts that it works.
