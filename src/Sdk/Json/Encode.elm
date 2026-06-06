module Sdk.Json.Encode exposing
  ( Value
  , object
  , pair
  , list
  , string
  , toRaw
  )

import Json.Encode
import Sdk.Key as Key exposing (Key)



type Value supported = Value Json.Encode.Value


type KeyValuePair supported =
  KeyValuePair String Json.Encode.Value


object : List (KeyValuePair supported) -> Value { provides | obj : supported }
object keyValuePairs =
  Value <| Json.Encode.object <|
    List.map (\(KeyValuePair k v) -> (k, v)) keyValuePairs


pair : Key provides supported -> Value supported -> KeyValuePair provides
pair k (Value v) =
    KeyValuePair (Key.toValue k) v


list : (a -> Value supported) -> List a -> Value { provides | array : supported }
list f xs =
  Value <| Json.Encode.list (\a -> toRaw (f a)) xs


string : String -> Value { provides | str : Key.Supported }
string value =
  Value <| Json.Encode.string value


toRaw : Value supported -> Json.Encode.Value
toRaw (Value value) =
  value


