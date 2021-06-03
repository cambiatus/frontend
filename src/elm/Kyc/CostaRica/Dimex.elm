module Kyc.CostaRica.Dimex exposing (isValid)

{-| DIMEX is Documento de identificación Tributario Especial.

DIMEX could have 11 or 12 digits without any separators: `XXXXXXXXXXX`.

-}

import Regex exposing (Regex)


validNumber : Regex
validNumber =
    Regex.fromString "[1-9]{1}\\d{10,11}"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid n =
    Regex.contains validNumber n
