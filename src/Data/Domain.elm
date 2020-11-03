module Data.Domain exposing (Domain(..), name, variables, init, empty, isEmpty)


type Domain
  = Domain String (List String)


init : String -> List String -> Domain
init =
  Domain


name : Domain -> String
name (Domain name_ _) =
  name_


variables : Domain -> List String
variables (Domain _ vars) =
  vars


empty : Domain
empty =
  Domain "" []


isEmpty : Domain -> Bool
isEmpty (Domain name_ vars) =
  String.isEmpty (String.trim name_) || List.isEmpty (List.map String.trim vars)
