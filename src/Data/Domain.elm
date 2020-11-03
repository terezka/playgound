module Data.Domain exposing (Domain(..), empty, isEmpty)


type Domain
  = Domain String (List String)


empty : Domain
empty =
  Domain "" []


isEmpty : Domain -> Bool
isEmpty (Domain name vars) =
  String.isEmpty (String.trim name) || List.isEmpty (List.map String.trim vars)


