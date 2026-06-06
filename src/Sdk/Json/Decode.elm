module Sdk.Json.Decode exposing
  ( Decoder
  , succeed
  , toRaw
  , required
  , optional
  , string
  , list
  , bool
  , int
  , field
  )


import Json.Decode as JD
import Sdk.Key as Key exposing (Key)


type Decoder supported a =
    Decoder (JD.Decoder a)


succeed : a -> Decoder supported a
succeed a =
    Decoder (JD.succeed a)


toRaw : Decoder supported a -> JD.Decoder a
toRaw (Decoder decoder) =
    decoder


andThen : (a -> Decoder supported b) -> Decoder supported a ->  Decoder supported b
andThen (aToDecoderB) (Decoder a) =
    Decoder (JD.andThen (toRaw << aToDecoderB) a)


field :
  Key p1 { supported | required : Key.Supported }
  -> Decoder supported a
  -> Decoder { p2 | obj : p1 } a
field key (Decoder decoder) =
    Decoder (JD.field (Key.toValue key) decoder)


required :
  Key p1 { supported | required : Key.Supported }
  -> Decoder supported a
  -> (a -> Decoder { p2 | obj : p1 } b)
  -> Decoder { p2 | obj : p1 } b
required key (Decoder decoder) (aToDecoderB) =
    Decoder (JD.andThen (toRaw << aToDecoderB) (JD.field (Key.toValue key) decoder))


optional :
  Key p1 supported
  -> Decoder supported a
  -> a
  -> (a -> Decoder { p2 | obj : p1 } b)
  -> Decoder { p2 | obj : p1 } b
optional key (Decoder decoder) default (aToDecoderB) =
    Decoder
        ( JD.andThen (toRaw << aToDecoderB)
            ( JD.oneOf
                [ JD.field (Key.toValue key) decoder
                , JD.succeed default
                ]
            )
        )


string : Decoder { provides | str : Key.Supported } String
string =
    Decoder (JD.string)


bool : Decoder { provides | bool : Key.Supported } Bool
bool =
    Decoder (JD.bool)


int : Decoder { provides | int : Key.Supported } Int
int =
    Decoder (JD.int)


list : Decoder supported a -> Decoder { provides | array : supported } (List a)
list (Decoder decoder) =
    Decoder (JD.list decoder)