module Kyc.CostaRica.Nite exposing
    ( NiteNumber
    , decoder
    , encode
    , fromString
    , isValid
    , toString
    )

{-| NITE is Número de Indetificación Tributario Especial.

NITE has 10 digits without any separators: `XXXXXXXXXX`.

-}

import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Regex exposing (Regex)


type NiteNumber
    = NiteNumber String


toString : NiteNumber -> String
toString (NiteNumber str) =
    str


fromString : String -> Maybe NiteNumber
fromString str =
    if isValid str then
        Just (NiteNumber str)

    else
        Nothing


validNumber : Regex
validNumber =
    Regex.fromString "[1-9]{1}\\d{9}"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid n =
    Regex.contains validNumber n


encode : NiteNumber -> Value
encode n =
    Encode.string (toString n)


decoder =
    Decode.map NiteNumber Decode.string
