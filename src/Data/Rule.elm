module Data.Rule exposing (Rule(..), Step(..), conditions, empty, withEmpty, clean)


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


clean : Rule -> Maybe Rule
clean (Rule conds conclusion) =
  let isStepEmpty (Step a b) =
        String.isEmpty (String.trim a) || String.isEmpty (String.trim b)
  in
  if isStepEmpty conclusion then
    Nothing
  else
    Just (Rule (List.filter (not << isStepEmpty) conds) conclusion)
