module Data.OneOrMore exposing (OneOrMore(..), length, map, indexedMap, filter, filterMap, all, add, updateAt, getAt, isLast)


type OneOrMore a
  = OneOrMore a (List a)


length : OneOrMore a -> Int
length =
  List.length << all


all : OneOrMore a -> List a
all (OneOrMore one rest) =
  one :: rest


first : OneOrMore a -> a
first (OneOrMore one _) =
  one


add : a -> OneOrMore a -> OneOrMore a
add value (OneOrMore one rest) =
  OneOrMore one (rest ++ [value])


filter : (a -> Bool) -> OneOrMore a -> Maybe (OneOrMore a)
filter func values =
  case List.filter func (all values) of
    [] -> Nothing
    one :: rest -> Just (OneOrMore one rest)


filterMap : (a -> Maybe b) -> OneOrMore a -> Maybe (OneOrMore b)
filterMap func values =
  case List.filterMap func (all values) of
    [] -> Nothing
    one :: rest -> Just (OneOrMore one rest)


map : (a -> b) -> OneOrMore a -> OneOrMore b
map func (OneOrMore one rest) =
  OneOrMore (func one) (List.map func rest)


indexedMap : (Int -> a -> b) -> OneOrMore a -> OneOrMore b
indexedMap func (OneOrMore one rest) =
  OneOrMore (func 0 one) (List.indexedMap (\i a -> func (i + 1) a) rest)


updateAt : Int -> (a -> a) -> OneOrMore a -> OneOrMore a
updateAt searched func =
  indexedMap <| \index value ->
    if index == searched then
      func value
    else
      value


getAt : Int -> OneOrMore a -> Maybe a
getAt searched values =
  values
    |> indexedMap (\index value -> if index == searched then Just value else Nothing)
    |> filterMap identity
    |> Maybe.map first


isLast : Int -> OneOrMore a -> Bool
isLast index values =
  index == length values - 1
