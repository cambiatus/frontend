module Kyc.CostaRica.Dimex exposing (..)

import Regex exposing (Regex)


type Dimex
    = Dimex String


toString : Dimex -> String
toString (Dimex str) =
    str


validNumber : Regex
validNumber =
    Regex.fromString "[1-9]{1}\\d{10,11}"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid n =
    Regex.contains validNumber n
