module Data.OneOrMore exposing (OneOrMore(..), init, any, all, length, map, indexedMap, filter, filterMap, values, add, updateAt, getAt, isLast, trailing)


type OneOrMore a
  = OneOrMore a (List a)


init : a -> List a -> OneOrMore a
init =
  OneOrMore


length : OneOrMore a -> Int
length =
  List.length << values


values : OneOrMore a -> List a
values (OneOrMore one rest) =
  one :: rest


fromList : List a -> Maybe (OneOrMore a)
fromList values_ =
  case values_ of
    one :: more -> Just (OneOrMore one more)
    _ -> Nothing


first : OneOrMore a -> a
first (OneOrMore one _) =
  one


add : a -> OneOrMore a -> OneOrMore a
add value (OneOrMore one rest) =
  OneOrMore one (rest ++ [value])


any : (a -> Bool) -> OneOrMore a -> Bool
any func values_ =
  List.any func (values values_)


all : (a -> Bool) -> OneOrMore a -> Bool
all func values_ =
  List.all func (values values_)


filter : (a -> Bool) -> OneOrMore a -> Maybe (OneOrMore a)
filter func values_ =
  case List.filter func (values values_) of
    [] -> Nothing
    one :: rest -> Just (OneOrMore one rest)


filterMap : (a -> Maybe b) -> OneOrMore a -> Maybe (OneOrMore b)
filterMap func values_ =
  case List.filterMap func (values values_) of
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
getAt searched values_ =
  values_
    |> indexedMap (\index value -> if index == searched then Just value else Nothing)
    |> filterMap identity
    |> Maybe.map first


isLast : Int -> OneOrMore a -> Bool
isLast index values_ =
  index == length values_ - 1


trailing : (a -> Bool) -> OneOrMore a -> OneOrMore a
trailing func (OneOrMore one more) =
  let loop vs =
        case vs of
          one_ :: more_ -> if func one_ then loop more_ else List.reverse vs
          [] -> []
  in
  OneOrMore one (loop (List.reverse more))




