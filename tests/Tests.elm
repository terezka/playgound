module Tests exposing (..)

import Parser.Syntax exposing (Syntax(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Language.SimpleLambdaCalculus


suite : Test
suite =
  describe "Parser."
    [ describe "Syntax"
        [ test "Can parse single symbols" <| \_ ->
            Expect.equal (Ok [Symbol "λ", Variable "x", Symbol ".", Spaces, Variable "e"]) (parse "λx. e")
        , test "Can parse double symbols" <| \_ ->
            Expect.equal (Ok [Variable "e", Spaces, Symbol "||", Spaces, Variable "e"]) (parse "e || e")
        , test "Can parse double char variables" <| \_ ->
            Expect.equal (Ok [Variable "e₁", Spaces, Symbol "||", Spaces, Variable "e₂"]) (parse "e₁ || e₂")
        , test "Can parse correctly without spaces: e₁||e₂" <| \_ ->
            Expect.equal (Ok [Variable "e₁", Symbol "||", Variable "e₂"]) (parse "e₁||e₂")
        , test "Can parse correctly without spaces: λx.e" <| \_ ->
            Expect.equal (Ok [Symbol "λ", Variable "x", Symbol ".", Variable "e"]) (parse "λx.e")
        ]
    ]



parse : String -> Result String (List Syntax)
parse =
  Parser.Syntax.fromString Language.SimpleLambdaCalculus.config