module Kyc.CostaRica.Phone exposing (isValid)

import Regex exposing (Regex)


{-| Costa Rica's phone number is +506 XXXX-XXXX.
Here we validate only digits without a country code: XXXX-XXXX or XXXXXXXX.
-}
validPhone : Regex
validPhone =
    Regex.fromString "[1-9]{1}\\d{3}-?\\d{4}"
        |> Maybe.withDefault Regex.never


isValid : String -> Bool
isValid phone =
    Regex.contains validPhone phone
