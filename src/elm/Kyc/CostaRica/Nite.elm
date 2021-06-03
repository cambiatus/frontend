module Kyc.CostaRica.Nite exposing (isValid)

{-| NITE is Número de Indetificación Tributario Especial.

NITE has 10 digits without any separators: `XXXXXXXXXX`.

-}

import Regex exposing (Regex)


validNumber : Regex
validNumber =
    Regex.fromString "[1-9]{1}\\d{9}"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid n =
    Regex.contains validNumber n
