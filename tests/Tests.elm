module Tests exposing (..)

import Parser.Grammar exposing (Grammar(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Language.SimpleLambdaCalculus


suite : Test
suite =
  describe "Parser.Grammar"
    [ test "Can parse single symbols" <| \_ ->
        Expect.equal (Ok [Symbol "λ", Variable "x", Symbol ".", Variable "e"]) (parse "λx. e")
    , test "Can parse double symbols" <| \_ ->
        Expect.equal (Ok [Variable "e", Symbol "||", Variable "e"]) (parse "e || e")
    , test "Can parse double char variables" <| \_ ->
        Expect.equal (Ok [Variable "e₁", Symbol "||", Variable "e₂"]) (parse "e₁ || e₂")
    , test "Can parse correctly without spaces: e₁||e₂" <| \_ ->
        Expect.equal (Ok [Variable "e₁", Symbol "||", Variable "e₂"]) (parse "e₁||e₂")
    , test "Can parse correctly without spaces: λx.e" <| \_ ->
        Expect.equal (Ok [Symbol "λ", Variable "x", Symbol ".", Variable "e"]) (parse "λx.e")
    ]


parse : String -> Result String (List Grammar)
parse =
  Parser.Grammar.fromString Language.SimpleLambdaCalculus.config