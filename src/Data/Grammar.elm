module Data.Grammar exposing (Grammar(..), init, isEmpty, withPlaceholder, clean)

import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))

type Grammar
  = Grammar String (OneOrMore String)


init : String -> String -> List String -> Grammar
init name one more =
  Grammar name (OneOrMore one more)


isEmpty : Grammar -> Bool
isEmpty (Grammar name syntaxes) =
  case OneOrMore.filter String.isEmpty syntaxes of
    Just syntaxes_ -> String.isEmpty (String.trim name)
    Nothing -> False


withPlaceholder : Grammar -> Grammar
withPlaceholder (Grammar name syntaxes) =
  Grammar name (OneOrMore.add "" syntaxes)


clean : Grammar -> Maybe Grammar
clean (Grammar name syntaxes) =
  case OneOrMore.filter String.isEmpty syntaxes of
    Just syntaxes_ -> Just (Grammar (String.trim name) syntaxes_)
    Nothing -> Nothing
