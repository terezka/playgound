module Data.Grammar exposing (Grammar(..), variable, syntaxes, init, empty, isEmpty, withEmpty, clean)

import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))

type Grammar
  = Grammar String (OneOrMore String)


syntaxes : Grammar -> OneOrMore String
syntaxes (Grammar _ syntaxes_) =
  syntaxes_


variable : Grammar -> String
variable (Grammar var _) =
  var


init : String -> String -> List String -> Grammar
init name one more =
  Grammar name (OneOrMore one more)


empty : Grammar
empty =
  init "" "" []


isEmpty : Grammar -> Bool
isEmpty (Grammar name syntaxes_) =
  case OneOrMore.filter (String.isEmpty << String.trim) syntaxes_ of
    Just syntaxes__ -> String.isEmpty (String.trim name)
    Nothing -> False


withEmpty : Grammar -> Grammar
withEmpty (Grammar name syntaxes_) =
  Grammar name (OneOrMore.add "" syntaxes_)


clean : Grammar -> Maybe Grammar
clean (Grammar name syntaxes_) =
  case OneOrMore.filter (not << String.isEmpty) syntaxes_ of
    Just syntaxes__ -> Just (Grammar (String.trim name) syntaxes__)
    Nothing -> Nothing
