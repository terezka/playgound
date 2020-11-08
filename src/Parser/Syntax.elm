module Parser.Syntax exposing (Syntax(..), fromString)

import Parser exposing (..)
import Data.Language as Language exposing (Language)
import Data.Domain as Domain exposing (Domain(..))
import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))
import Set exposing (Set)

type Syntax
  = Symbol String
  | Variable String
  | Spaces


fromString : Language -> String -> Result String (List Syntax)
fromString language =
  run (parser language)
    >> Result.mapError deadEndsToString


-- PARSER


parser : Language -> Parser (List Syntax)
parser language =
  succeed finalize
    |. spaces
    |= loop [] (pSyntax language)
    |. end


finalize : List Syntax -> List Syntax
finalize syntaxes =
  let fold syntax last =
        case (last, syntax) of
          (Symbol c1 :: rest, Symbol c2) -> Symbol (c1 ++ c2) :: rest
          _ -> syntax :: last
  in
  List.foldl fold [] syntaxes
    |> List.reverse


pSyntax : Language -> List Syntax -> Parser (Step (List Syntax) (List Syntax))
pSyntax language done =
  oneOf
    [ succeed (\syntax -> Loop (syntax :: done))
        |= pVariables language
    , succeed (\syntax -> Loop (Symbol syntax :: done))
        |= pOneChar
    , succeed (\_ -> Loop (Spaces :: done))
        |= pSpaces
    , succeed ()
        |> map (\_ -> Done (List.reverse done))
    ]


pVariables : Language -> Parser Syntax
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
  let toTuple domain variable = (Domain.name domain, variable)
      toTuples domain =
        Domain.variables domain
          |> Set.toList
          |> List.map (toTuple domain)
  in
  OneOrMore.values language.domains
    |> List.sortBy (Set.size << Domain.variables)
    |> List.concatMap toTuples
    |> List.reverse

