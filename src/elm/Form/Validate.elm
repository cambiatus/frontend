module Form.Validate exposing
    ( required
    , intGreaterThan, intGreaterThanOrEqualTo
    , maskedFloat
    , markdownLongerThan
    , lengthGreaterThanOrEqualTo
    , futureDate
    )

{-| This module offers a bunch of ready-made functions to use as the `parser`
from `Form`s. These are all built using `Result`, so you can chain them together
with `Result.andThen` and other helper functions. Since these are generic
functions, they might not be enough for every case, so you might need to do
custom validation per field. However, you should use these as much as possible
(even when doing custom validations), so we get consistent error messages
throughout the app


## Generic validations

@docs required


## String inputs

@docs intGreaterThan, intGreaterThanOrEqualTo

@docs maskedFloat

@docs markdownLongerThan


## List inputs

@docs lengthGreaterThanOrEqualTo


## Dates

@docs futureDate

-}

import Date exposing (Date)
import Markdown exposing (Markdown)
import Session.Shared as Shared exposing (Shared)



-- GENERIC


required : Shared.Translators -> Maybe a -> Result String a
required { t } maybeInput =
    Result.fromMaybe (t "error.required") maybeInput



-- STRINGS


int : Shared.Translators -> String -> Result String Int
int { t } =
    String.toInt
        >> Result.fromMaybe (t "error.validator.text.only_numbers")


intGreaterThan : Int -> Shared.Translators -> String -> Result String Int
intGreaterThan lowerBound translators =
    int translators
        >> Result.andThen
            (\x ->
                if x > lowerBound then
                    Ok x

                else
                    Err <|
                        translators.tr "error.validator.number.greater_than"
                            [ ( "base", String.fromInt lowerBound ) ]
            )


intGreaterThanOrEqualTo : Int -> Shared.Translators -> String -> Result String Int
intGreaterThanOrEqualTo lowerBound translators =
    int translators
        >> Result.andThen
            (\x ->
                if x >= lowerBound then
                    Ok x

                else
                    Err
                        (translators.tr "error.validator.number.greater_than_or_equal"
                            [ ( "base", String.fromInt lowerBound ) ]
                        )
            )


float : Shared.Translators -> String -> Result String Float
float { t } =
    String.toFloat
        >> Result.fromMaybe (t "error.validator.text.only_numbers")


maskedFloat : Shared.Translators -> String -> Result String Float
maskedFloat translators =
    Shared.floatStringFromSeparatedString translators
        >> float translators


stringLongerThan : Int -> Shared.Translators -> String -> Result String String
stringLongerThan minLength { tr } stringInput =
    if String.length stringInput < minLength then
        Err <|
            tr "error.validator.text.longer_than"
                [ ( "base", String.fromInt minLength ) ]

    else
        Ok stringInput


markdownLongerThan : Int -> Shared.Translators -> Markdown -> Result String Markdown
markdownLongerThan minLength translators markdownInput =
    case stringLongerThan minLength translators (Markdown.toUnformattedString markdownInput) of
        Ok _ ->
            Ok markdownInput

        Err err ->
            Err err



-- LISTS


lengthGreaterThanOrEqualTo : Int -> Shared.Translators -> List a -> Result String (List a)
lengthGreaterThanOrEqualTo minLength { tr } items =
    if List.length items >= minLength then
        Ok items

    else
        Err <|
            tr "error.validator.number.length_greater_than_or_equal"
                [ ( "count", String.fromInt minLength ) ]



-- DATES


futureDate : Shared -> Date -> Result String Date
futureDate shared selectedDate =
    let
        today =
            Date.fromPosix shared.timezone shared.now
    in
    if Date.compare selectedDate today == LT then
        Err <| shared.translators.t "error.validator.date.invalid"

    else
        Ok selectedDate
