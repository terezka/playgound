module Data.Domain exposing (Domain(..), isEmpty)


type Domain
  = Domain String (List String)


isEmpty : Domain -> Bool
isEmpty (Domain name vars) =
  String.isEmpty (String.trim name) && List.isEmpty (List.map String.trim vars)
