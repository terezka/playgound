module Data.Domain exposing (Domain(..), name, variables, init, find, belongs)

import Set exposing (Set)
import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))
import List.Extra


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


find : String -> OneOrMore Domain -> Maybe Domain
find var domains =
  List.Extra.find (belongs var) (OneOrMore.values domains)


belongs : String -> Domain -> Bool
belongs var (Domain _ vars) =
  Set.member var vars