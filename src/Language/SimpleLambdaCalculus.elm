module Language.SimpleLambdaCalculus exposing (config)

import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))
import Data.Language as Language
import Data.Domain as Domain
import Data.Grammar as Grammar exposing (Syntax(..), Piece(..))
import Data.Rule as Rule exposing (Rule(..), Step(..))


config : Language.Language
config =
  { domains =
        OneOrMore (Domain.init "Expressions" ["e", "e₀", "e₁", "e₂", "e₁′", "e′"])
          [ Domain.init "Variables" ["x", "y", "z", "w"]
          ]
    , grammars =
        let lambda = Syntax [Symbol "λ", Variable "x", Symbol ".", Spaces, Variable "e"]
            application = Syntax [Variable "e₁", Spaces, Variable "e₂"]
            variable = Syntax [Variable "x"]
            parentes = Syntax [Symbol "(", Variable "e", Symbol ")"]
        in
        OneOrMore (Grammar.init "e" (OneOrMore.init lambda [application, variable, parentes]))
          []
    , semantics =
        OneOrMore (Rule [Step "e₁" "e₁′"] (Step "e₁ e₂" "e₁ e₂"))
          [ Rule [Step "e" "e′"] (Step "v e" "v e′")
          , Rule [] (Step "(λx. e) v" "e{v/x}")
          ]
    , expression = "(λx. x)(λx. x)"
  }

