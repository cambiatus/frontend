module Kyc.CostaRica.CedulaDeIdentidadNumber exposing
    ( CedulaDeIdentidadNumber
    , decoder
    , encode
    , format
    , fromString
    , isValid
    , toString
    )

{-| "Cédula de identidad" is the personal 9-digits number that any citizen of Costa Rica has.
The format could be like this: 123456789 or like this: 1-2345-6789.

This module contains an opaque type for the Cedula-number and its purpose is to store only valid numbers.

Cedulas could be created from strings and converted to strings:

    fromString : String -> Maybe CedulaDeIdentidadNumber

    toString : CedulaDeIdentidadNumber -> String

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Regex exposing (Regex)


type CedulaDeIdentidadNumber
    = CedulaDeIdentidadNumber String


toString : CedulaDeIdentidadNumber -> String
toString (CedulaDeIdentidadNumber str) =
    str


format : String -> String
format str =
    let
        first =
            String.slice 0 1 str

        nextFourDigits =
            String.slice 1 5 str

        lastFourDigits =
            String.slice 5 10 str
    in
    first ++ "-" ++ nextFourDigits ++ "-" ++ lastFourDigits


fromString : String -> Maybe CedulaDeIdentidadNumber
fromString str =
    if isValid str then
        Just (CedulaDeIdentidadNumber str)

    else
        Nothing


validNumber : Regex
validNumber =
    Regex.fromString "^[1-9]-?\\d{4}-?\\d{4}$"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid n =
    Regex.contains validNumber n


encode : CedulaDeIdentidadNumber -> Value
encode n =
    Encode.string (toString n)


decoder =
    Decode.map CedulaDeIdentidadNumber Decode.string
