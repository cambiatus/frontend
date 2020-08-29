module Kyc.CostaRica.Dimex exposing (DimexNumber, isValid, toString)

{-| DIMEX is Documento de identificación Tributario Especial.

DIMEX could have 11 or 12 digits without any separators: `XXXXXXXXXXX`.

-}

import Regex exposing (Regex)


type DimexNumber
    = DimexNumber String


toString : DimexNumber -> String
toString (DimexNumber str) =
    str


validNumber : Regex
validNumber =
    Regex.fromString "[1-9]{1}\\d{10,11}"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid n =
    Regex.contains validNumber n
