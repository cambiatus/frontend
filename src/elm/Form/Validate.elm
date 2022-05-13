module Form.Validate exposing
    ( Validator, succeed, validate, custom, Error, withCustomError, map
    , required
    , stringShorterThan, stringLongerThan, stringLengthExactly, url, email, eosName, time
    , int, intGreaterThan, intGreaterThanOrEqualTo, intLowerThanOrEqualTo
    , maskedFloat, floatGreaterThan, floatGreaterThanOrEqualTo, floatLowerThanOrEqualTo
    , markdownLongerThan
    , lengthGreaterThanOrEqualTo
    , futureDate
    )

{-| This module offers a bunch of ready-made functions to use as the `parser`
from `Form`s. They are meant to use in a pipeline, like so:

    parser =
        Form.Validate.succeed
            >> Form.Validate.int
            >> Form.Validate.intGreaterThan 0
            >> Form.Validate.validate translators

Since these are generic functions, they might not be enough for every case, so
you might need to do custom validation per field. You can do that with `custom`.
However, you should use these as much as possible (even when doing custom
validations), so we get consistent error messages throughout the app.


## Pipeline helpers

@docs Validator, succeed, validate, custom, Error, withCustomError, map


## Generic validations

@docs required


## String inputs

@docs stringShorterThan, stringLongerThan, stringLengthExactly, url, email, eosName, time

@docs int, intGreaterThan, intGreaterThanOrEqualTo, intLowerThanOrEqualTo

@docs maskedFloat, floatGreaterThan, floatGreaterThanOrEqualTo, floatLowerThanOrEqualTo

@docs markdownLongerThan


## List inputs

@docs lengthGreaterThanOrEqualTo


## Dates

@docs futureDate

-}

import Date exposing (Date)
import Eos.Account
import Markdown exposing (Markdown)
import Regex
import Time
import Translation
import Url



-- BUILDING PIPELINES


{-| A validator is used to store the result of a pipeline of validations.
-}
type Validator output
    = Validator (Result Error output)


{-| Errors are just functions that take translators and return a translated
string. It's designed this way so we only need to pass in `translators` once.
-}
type alias Error =
    Translation.Translators -> String


{-| Start a validation pipeline

    parser =
        Form.Validate.succeed
            |> Form.Validate.int
            |> Form.Validate.validate

-}
succeed : output -> Validator output
succeed output =
    Validator (Ok output)


{-| Change the type of output without performing additional validation
-}
map : (output -> mappedOutput) -> Validator output -> Validator mappedOutput
map fn (Validator validator) =
    Validator (Result.map fn validator)


{-| Finish a validation pipeline. You can always chain custom validations with
`Result.andThen` after calling `validate`

    parser =
        Form.Validate.succeed
            |> Form.Validate.int
            |> Form.Validate.validate

-}
validate : Translation.Translators -> Validator a -> Result String a
validate translators (Validator result) =
    Result.mapError (\error -> error translators) result


{-| Create a custom validation in your pipeline! In fact most validations are
implemented using this function.

    required : Validator (Maybe output) -> Validator output
    required =
        custom (Result.fromMaybe (\{ t } -> t "error.required"))

    parser : String -> Result String Int
    parser =
        Form.Validate.succeed
            >> Form.Validate.int
            >> Form.Validate.custom
                (\x ->
                    if x == 0 then
                        Err (\t -> "x can't be 0")

                    else
                        Ok x
                )
            >> Form.Validate.validate translators

-}
custom : (a -> Result Error b) -> Validator a -> Validator b
custom validation (Validator validator) =
    validator
        |> Result.andThen validation
        |> Validator


{-| Switch the current error. Useful when you want to provide a custom error
message that is more context-aware.
-}
withCustomError : (Translation.Translators -> String) -> Validator a -> Validator a
withCustomError newError (Validator validator) =
    case validator of
        Ok ok ->
            Validator (Ok ok)

        Err _ ->
            Validator (Err newError)



-- GENERIC


required : Validator (Maybe output) -> Validator output
required =
    custom (Result.fromMaybe (\{ t } -> t "error.required"))



-- STRINGS


int : Validator String -> Validator Int
int =
    number String.toInt


float : Validator String -> Validator Float
float =
    number String.toFloat


intGreaterThan : Int -> Validator Int -> Validator Int
intGreaterThan =
    numberGreaterThan String.fromInt


intGreaterThanOrEqualTo : Int -> Validator Int -> Validator Int
intGreaterThanOrEqualTo =
    numberGreaterThanOrEqualTo String.fromInt


intLowerThanOrEqualTo : Int -> Validator Int -> Validator Int
intLowerThanOrEqualTo =
    numberLowerThanOrEqualTo String.fromInt


floatGreaterThan : Float -> Validator Float -> Validator Float
floatGreaterThan =
    numberGreaterThan String.fromFloat


floatGreaterThanOrEqualTo : Float -> Validator Float -> Validator Float
floatGreaterThanOrEqualTo =
    numberGreaterThanOrEqualTo String.fromFloat


floatLowerThanOrEqualTo : Float -> Validator Float -> Validator Float
floatLowerThanOrEqualTo =
    numberLowerThanOrEqualTo String.fromFloat


maskedFloat : Translation.Translators -> Validator String -> Validator Float
maskedFloat translators =
    mapValidation (Translation.floatStringFromSeparatedString translators)
        >> float


stringShorterThan : Int -> Validator String -> Validator String
stringShorterThan maxLength =
    custom
        (\stringInput ->
            if String.length stringInput > maxLength then
                Err
                    (\{ tr } ->
                        tr "error.valdator.text.shorter_than" [ ( "base", String.fromInt maxLength ) ]
                    )

            else
                Ok stringInput
        )


stringLongerThan : Int -> Validator String -> Validator String
stringLongerThan minLength =
    custom
        (\stringInput ->
            if String.length stringInput < minLength then
                Err
                    (\{ tr } ->
                        tr "error.validator.text.longer_than"
                            [ ( "base", String.fromInt minLength ) ]
                    )

            else
                Ok stringInput
        )


stringLengthExactly : Int -> Validator String -> Validator String
stringLengthExactly length =
    custom
        (\stringInput ->
            if String.length stringInput == length then
                Ok stringInput

            else
                Err (\{ tr } -> tr "error.validator.text.exactly" [ ( "base", String.fromInt length ) ])
        )


url : Validator String -> Validator Url.Url
url =
    custom
        (\stringInput ->
            let
                withProtocol =
                    if String.isEmpty stringInput then
                        ""

                    else if String.startsWith "https://" stringInput || String.startsWith "http://" stringInput then
                        stringInput

                    else
                        "http://" ++ stringInput
            in
            case Url.fromString withProtocol of
                Nothing ->
                    Err (\{ t } -> t "settings.community_info.errors.website.invalid")

                Just url_ ->
                    Ok url_
        )


markdownLongerThan : Int -> Validator Markdown -> Validator Markdown
markdownLongerThan minLength =
    custom
        (\markdownInput ->
            if String.length (Markdown.toUnformattedString markdownInput) < minLength then
                Err
                    (\{ tr } ->
                        tr "error.validator.text.longer_than"
                            [ ( "base", String.fromInt minLength ) ]
                    )

            else
                Ok markdownInput
        )


eosName : Validator String -> Validator Eos.Account.Name
eosName =
    custom
        (\stringInput ->
            case Eos.Account.fromString stringInput of
                Ok account ->
                    Ok account

                Err error ->
                    Err (\translators -> Eos.Account.errorToString translators error)
        )


email : Validator String -> Validator String
email =
    custom
        (\stringInput ->
            if isValidEmail stringInput then
                Ok stringInput

            else
                Err (\{ t } -> t "error.email")
        )



-- LISTS


lengthGreaterThanOrEqualTo : Int -> Validator (List a) -> Validator (List a)
lengthGreaterThanOrEqualTo minLength =
    custom
        (\items ->
            if List.length items >= minLength then
                Ok items

            else
                Err
                    (\{ tr } ->
                        tr "error.validator.number.length_greater_than_or_equal"
                            [ ( "count", String.fromInt minLength ) ]
                    )
        )



-- DATES


futureDate : Time.Zone -> Time.Posix -> Validator Date -> Validator Date
futureDate timezone now =
    custom
        (\selectedDate ->
            let
                today =
                    Date.fromPosix timezone now
            in
            if Date.compare selectedDate today == LT then
                Err (\{ t } -> t "error.validator.date.invalid")

            else
                Ok selectedDate
        )


time : Validator String -> Validator { hour : Int, minute : Int }
time =
    custom
        (\timeString ->
            case String.split ":" timeString of
                [ hourString, minuteString ] ->
                    Maybe.map2 (\hour minute -> { hour = hour, minute = minute })
                        (String.toInt hourString)
                        (String.toInt minuteString)
                        |> Result.fromMaybe (\{ t } -> t "news.editor.error.invalid_time")

                _ ->
                    Err (\{ t } -> t "news.editor.error.invalid_time")
        )



-- INTERNAL HELPERS


mapValidation : (a -> b) -> Validator a -> Validator b
mapValidation fn (Validator validator) =
    Validator (Result.map fn validator)


number : (String -> Maybe number) -> Validator String -> Validator number
number fromString =
    custom
        (fromString
            >> Result.fromMaybe (\{ t } -> t "error.validator.text.only_numbers")
        )


numberGreaterThan : (number -> String) -> number -> Validator number -> Validator number
numberGreaterThan numberToString lowerBound =
    custom
        (\x ->
            if x > lowerBound then
                Ok x

            else
                Err
                    (\{ tr } ->
                        tr "error.validator.number.greater_than"
                            [ ( "base", numberToString lowerBound ) ]
                    )
        )


numberGreaterThanOrEqualTo : (number -> String) -> number -> Validator number -> Validator number
numberGreaterThanOrEqualTo numberToString lowerBound =
    custom
        (\x ->
            if x >= lowerBound then
                Ok x

            else
                Err
                    (\{ tr } ->
                        tr "error.validator.number.greater_than_or_equal"
                            [ ( "base", numberToString lowerBound ) ]
                    )
        )


numberLowerThanOrEqualTo : (number -> String) -> number -> Validator number -> Validator number
numberLowerThanOrEqualTo numberToString upperBound =
    custom
        (\x ->
            if x <= upperBound then
                Ok x

            else
                Err
                    (\{ tr } ->
                        tr "error.validator.number.lower_than_or_equal" [ ( "base", numberToString upperBound ) ]
                    )
        )


isValidEmail : String -> Bool
isValidEmail =
    let
        validEmailRegex =
            "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
                |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
                |> Maybe.withDefault Regex.never
    in
    Regex.contains validEmailRegex
