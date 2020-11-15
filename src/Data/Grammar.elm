module Data.Grammar exposing
  ( Grammar, Syntax(..), Piece(..)
  , variable, syntaxes, find, init
  , syntaxToString, syntaxFromString
  , Branch(..), toBranches
  )

import Parser exposing (..)
import Data.Domain as Domain exposing (Domain(..))
import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))
import List.Extra
import Set exposing (Set)


type Grammar
  = Grammar String (OneOrMore Syntax)


type Syntax =
  Syntax (List Piece)


type Piece
  = Symbol String
  | Variable String
  | Spaces


init : String -> OneOrMore Syntax -> Grammar
init name syntaxes_ =
  Grammar name syntaxes_


syntaxes : Grammar -> OneOrMore Syntax
syntaxes (Grammar _ syntaxes_) =
  syntaxes_


variable : Grammar -> String
variable (Grammar name _) =
  name


find : Domain -> OneOrMore Grammar -> Maybe Grammar
find domain grammars =
  List.Extra.find
    (\(Grammar name _) -> Set.member name (Domain.variables domain))
    (OneOrMore.values grammars)


syntaxToString : Syntax -> String
syntaxToString (Syntax pieces) =
  String.concat (List.map syntaxPieceToString pieces)


syntaxPieceToString : Piece -> String
syntaxPieceToString piece =
  case piece of
    Symbol symbol -> symbol
    Variable var -> var
    Spaces -> " "


syntaxFromString : OneOrMore Domain -> String -> Result String Syntax
syntaxFromString domains string =
  run (parser domains) string
    |> Result.map Syntax
    |> Result.mapError deadEndsToString


parser : OneOrMore Domain -> Parser (List Piece)
parser domains =
  succeed finalize
    |. spaces
    |= loop [] (pPieces domains)
    |. end


finalize : List Piece -> List Piece
finalize pieces =
  let fold syntax last =
        case (last, syntax) of
          (Symbol c1 :: rest, Symbol c2) ->
            Symbol (c1 ++ c2) :: rest

          _ ->
            syntax :: last
  in
  List.foldl fold [] pieces
    |> List.reverse


pPieces : OneOrMore Domain -> List Piece -> Parser (Step (List Piece) (List Piece))
pPieces domains done =
  oneOf
    [ succeed (\syntax -> Loop (syntax :: done))
        |= pVariables domains
    , succeed (\syntax -> Loop (Symbol syntax :: done))
        |= pOneChar
    , succeed (\_ -> Loop (Spaces :: done))
        |= pSpaces
    , succeed ()
        |> map (\_ -> Done (List.reverse done))
    ]


pVariables : OneOrMore Domain -> Parser Piece
pVariables domains =
  let pVariable (_, var) =
        succeed (Variable var)
          |. symbol var
  in
  oneOf (List.map pVariable (toVariables domains))


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


toVariables : OneOrMore Domain -> List (String, String)
toVariables domains =
  let toTuple domain var = (Domain.name domain, var)
      toTuples domain =
        Domain.variables domain
          |> Set.toList
          |> List.map (toTuple domain)
  in
  OneOrMore.values domains
    |> List.sortBy (Set.size << Domain.variables)
    |> List.concatMap toTuples
    |> List.reverse



-- BRANCHING


type Branch
  = Branch Bool Piece (List Branch)


toBranches : Domain -> Grammar -> ( List Branch, List Branch )
toBranches domain grammar =
  let addSyntaxes : List Syntax -> ( List Branch, List Branch ) -> ( List Branch, List Branch )
      addSyntaxes syntaxes_ ( mains, inner ) =
        case syntaxes_ of
          Syntax (Variable var :: pieces) :: rest ->
            if Domain.belongs var domain
              then addSyntaxes rest ( mains, addPieces pieces inner )
              else addSyntaxes rest ( addPiece (Variable var) pieces mains, inner )

          Syntax (head :: pieces) :: rest ->
            addSyntaxes rest ( addPiece head pieces mains, inner )

          _ ->
            ( mains, inner )

      addPieces : List Piece -> List Branch -> List Branch
      addPieces pieces inners =
        case pieces of
          piece :: rest -> addPiece piece rest inners
          [] -> inners

      addPiece : Piece -> List Piece -> List Branch -> List Branch
      addPiece piece rest branches =
        case tryAll (tryOne piece rest) [] branches of
          Just updated -> updated
          Nothing -> toNew piece rest :: branches

      tryAll : (Branch -> Maybe Branch) -> List Branch -> List Branch -> Maybe (List Branch)
      tryAll update tried untried =
        case untried of
          branch :: more ->
            case update branch of
              Just updated -> Just (tried ++ updated :: more)
              Nothing -> tryAll update (tried ++ [branch]) more

          [] ->
            Nothing

      tryOne : Piece -> List Piece -> Branch -> Maybe Branch
      tryOne p1 rest (Branch ends p2 branches) =
        if p1 == p2
          then Just (Branch ends p2 (addPieces rest branches))
          else Nothing

      toNew : Piece -> List Piece -> Branch
      toNew piece rest =
        case rest of
          one :: more -> Branch False piece [toNew one more]
          [] -> Branch True piece []
  in
  addSyntaxes (OneOrMore.values <| syntaxes grammar) ([], [])



