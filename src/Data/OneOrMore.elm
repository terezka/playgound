module Data.OneOrMore exposing (OneOrMore(..), map, all)


type OneOrMore a
  = OneOrMore a (List a)


all : OneOrMore a -> List a
all (OneOrMore first rest) =
  first :: rest


map : (a -> b) -> OneOrMore a -> OneOrMore b
map func (OneOrMore first rest) =
  OneOrMore (func first) (List.map func rest)

