module Data.Rule exposing (Rule(..), Step(..), conditions, empty, withEmpty)


type Rule
  = Rule (List Step) Step


type Step
  = Step String String


conditions : Rule -> List Step
conditions (Rule conds _) =
  conds


empty : Rule
empty =
  Rule [] (Step "" "")


withEmpty : Rule -> Rule
withEmpty (Rule conds conclusion) =
  Rule (conds ++ [Step "" ""]) conclusion