module Kyc.CostaRica.Mipyme exposing (isValid, toString)

import Regex exposing (Regex)


type Mipyme
    = Mipyme String


toString : Mipyme -> String
toString (Mipyme str) =
    str


validNumber : Regex
validNumber =
    Regex.fromString "\\d-?\\d{3}-?\\d{6}"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid n =
    Regex.contains validNumber n
