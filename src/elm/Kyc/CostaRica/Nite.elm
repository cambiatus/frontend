module Kyc.CostaRica.Nite exposing
    ( Nite
    , decoder
    , encode
    , fromString
    , isValid
    , toString
    )

import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Regex exposing (Regex)


type Nite
    = Nite String


toString : Nite -> String
toString (Nite str) =
    str


fromString : String -> Maybe Nite
fromString str =
    if isValid str then
        Just (Nite str)

    else
        Nothing


validNumber : Regex
validNumber =
    Regex.fromString "[1-9]{1}\\d{9}"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid n =
    Regex.contains validNumber n


encode : Nite -> Value
encode n =
    Encode.string (toString n)


decoder =
    Decode.map Nite Decode.string
