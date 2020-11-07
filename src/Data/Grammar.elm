module Data.Grammar exposing (Grammar, variable, syntaxes, init)

import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))


type Grammar
  = Grammar String (OneOrMore String)


init : String -> OneOrMore String -> Grammar
init name syntaxes_ =
  Grammar name syntaxes_


syntaxes : Grammar -> OneOrMore String
syntaxes (Grammar _ syntaxes_) =
  syntaxes_


variable : Grammar -> String
variable (Grammar name _) =
  name


