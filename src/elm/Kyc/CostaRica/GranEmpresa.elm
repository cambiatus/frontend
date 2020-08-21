module Kyc.CostaRica.GranEmpresa exposing (isValid, toString)

import Regex exposing (Regex)


type GranEmpresa
    = GranEmpresa String


toString : GranEmpresa -> String
toString (GranEmpresa str) =
    str


validNumber : Regex
validNumber =
    Regex.fromString "\\d-?\\d{3}-?\\d{6}"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid n =
    Regex.contains validNumber n
