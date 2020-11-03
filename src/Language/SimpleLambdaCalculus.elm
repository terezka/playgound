module Language.SimpleLambdaCalculus exposing (config)

import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))
import Data.Language as Language
import Data.Domain as Domain
import Data.Grammar as Grammar
import Data.Rule as Rule exposing (Rule(..), Step(..))


config : Language.Language
config =
  { domains =
        OneOrMore (Domain.init "Variables" ["x", "y", "z", "w"])
          [ Domain.init "Expressions" ["e", "e₀", "e₁", "e₂", "e₁′", "e′"]
          ]
    , grammars =
        OneOrMore (Grammar.init "e" "λx. e" [ "e₁ e₂", "x", "(e)"])
          []
    , semantics =
        OneOrMore (Rule [Step "e₁" "e₁′"] (Step "e₁ e₂" "e₁ e₂"))
          [ Rule [Step "e" "e′"] (Step "v e" "v e′")
          , Rule [] (Step "(λx. e) v" "e{v/x}")
          ]
    , expression = "(λx. x)(λx. x)"
  }

