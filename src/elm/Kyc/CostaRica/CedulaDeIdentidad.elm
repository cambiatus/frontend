module Kyc.CostaRica.CedulaDeIdentidad exposing (isValid)

{-| "Cédula de identidad" is the personal 9-digits number that any citizen of Costa Rica has.
The format could be like this: 123456789 or like this: 1-2345-6789.
-}

import Regex exposing (Regex)


validNumber : Regex
validNumber =
    Regex.fromString "^[1-9]-?\\d{4}-?\\d{4}$"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid n =
    Regex.contains validNumber n
