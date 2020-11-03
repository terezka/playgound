module Data.Language exposing (Language)

import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))
import Data.Domain as Domain
import Data.Grammar as Grammar
import Data.Rule as Rule


type alias Language =
  { domains : OneOrMore Domain.Domain
  , grammars : OneOrMore Grammar.Grammar
  , semantics : OneOrMore Rule.Rule
  , expression : String
  }