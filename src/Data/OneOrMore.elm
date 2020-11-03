module Data.OneOrMore exposing (OneOrMore(..), map, indexedMap, all)


type OneOrMore a
  = OneOrMore a (List a)


all : OneOrMore a -> List a
all (OneOrMore first rest) =
  first :: rest


map : (a -> b) -> OneOrMore a -> OneOrMore b
map func (OneOrMore first rest) =
  OneOrMore (func first) (List.map func rest)


indexedMap : (Int -> a -> b) -> OneOrMore a -> OneOrMore b
indexedMap func (OneOrMore first rest) =
  OneOrMore (func 0 first) (List.indexedMap (\i a -> func (i + 1) a) rest)
