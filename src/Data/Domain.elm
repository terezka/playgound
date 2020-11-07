module Data.Domain exposing (Domain(..), name, variables, init)

import Set exposing (Set)


type Domain
  = Domain String (Set String)


init : String -> List String -> Domain
init name_ variables_ =
  Domain name_ (Set.fromList variables_)


name : Domain -> String
name (Domain name_ _) =
  name_


variables : Domain -> Set String
variables (Domain _ vars) =
  vars

