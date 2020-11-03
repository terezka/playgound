module Data.OneOrMore exposing (OneOrMore(..), map, indexedMap, filter, all, add, updateAt)


type OneOrMore a
  = OneOrMore a (List a)


all : OneOrMore a -> List a
all (OneOrMore first rest) =
  first :: rest


add : a -> OneOrMore a -> OneOrMore a
add value (OneOrMore first rest) =
  OneOrMore first (rest ++ [value])


filter : (a -> Bool) -> OneOrMore a -> Maybe (OneOrMore a)
filter func values =
  case List.filter func (all values) of
    [] -> Nothing
    first :: rest -> Just (OneOrMore first rest)


map : (a -> b) -> OneOrMore a -> OneOrMore b
map func (OneOrMore first rest) =
  OneOrMore (func first) (List.map func rest)


indexedMap : (Int -> a -> b) -> OneOrMore a -> OneOrMore b
indexedMap func (OneOrMore first rest) =
  OneOrMore (func 0 first) (List.indexedMap (\i a -> func (i + 1) a) rest)


updateAt : Int -> (a -> a) -> OneOrMore a -> OneOrMore a
updateAt searched func =
  indexedMap <| \index value ->
    if index == searched then
      func value
    else
      value