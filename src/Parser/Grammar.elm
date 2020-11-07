module Parser.Grammar exposing (Grammar(..), fromString)

import Parser exposing (..)
import Data.Language as Language exposing (Language)
import Data.Domain as Domain exposing (Domain(..))
import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))


type Grammar
  = Symbol String
  | Variable String


fromString : Language -> String -> Result String (List Grammar)
fromString language =
  run (parser language)
    >> Result.mapError deadEndsToString


-- PARSER


parser : Language -> Parser (List Grammar)
parser language =
  succeed finalize
    |. spaces
    |= loop [] (pGrammar language)
    |. end


finalize : List Grammar -> List Grammar
finalize syntaxes =
  let fold syntax last =
        case (last, syntax) of
          (Symbol c1 :: rest, Symbol c2) -> Symbol (c1 ++ c2) :: rest
          _ -> syntax :: last
  in
  List.foldl fold [] syntaxes
    |> List.reverse


pGrammar : Language -> List Grammar -> Parser (Step (List Grammar) (List Grammar))
pGrammar language done =
  oneOf
    [ succeed (\syntax -> Loop (syntax :: done))
        |= pVariables language
    , succeed (\syntax -> Loop (Symbol syntax :: done))
        |= pOneChar
    , succeed (Loop done)
        |. pSpaces
    , succeed ()
        |> map (\_ -> Done (List.reverse done))
    ]


pVariables : Language -> Parser Grammar
pVariables language =
  let pVariable (_, variable) =
        succeed (Variable variable)
          |. symbol variable
  in
  oneOf (List.map pVariable (toVariables language))


pOneChar : Parser String
pOneChar =
  getChompedString <|
    succeed ()
      |. chompIf (not << isWhitespace)


pSpaces : Parser String
pSpaces =
  getChompedString <|
    succeed ()
      |. chompIf isWhitespace
      |. chompWhile isWhitespace



-- UTILS


isWhitespace : Char -> Bool
isWhitespace c =
  c == ' ' || c == '\t' || c == '\n' || c == '\r'


toVariables : Language -> List (String, String)
toVariables language =
  OneOrMore.all language.domains
    |> List.concatMap (\d -> List.map (\v -> (Domain.name d, v)) (Domain.variables d))
    |> List.sortBy (\(d, v) -> String.length v)
    |> List.reverse

