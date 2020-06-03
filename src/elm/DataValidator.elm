module DataValidator exposing
    ( Validator
    , addConstraints
    , custom
    , exactly
    , getInput
    , greaterThan
    , greaterThanOrEqual
    , hasErrors
    , isOdd
    , lengthGreaterThanOrEqual
    , listErrors
    , longerThan
    , lowerThan
    , lowerThanOrEqual
    , negative
    , newValidator
    , noneOf
    , oneOf
    , onlyLetters
    , onlyNumbers
    , positive
    , shorterThan
    , toCustomResult
    , updateInput
    , validate
    )

import I18Next exposing (Translations)


type alias Validator value =
    { input : Input value
    , transform : Transform value
    , mandatory : Mandatory
    , constraints : Constraints Constraint
    }


type Input value
    = Input value


type Transform value
    = Transform (value -> Maybe String)


type Mandatory
    = Required Status
    | Optional


type alias Constraints c =
    { none : List c
    , valid : List c
    , invalid : List c
    }


type alias Constraint =
    { test : TestFn
    , defaultError : TranslationFn
    }


type alias TestFn =
    String -> Bool


type alias TranslationFn =
    Translations -> String


type Status
    = None
    | Valid
    | Invalid



-- Builders


updateInput : value -> Validator value -> Validator value
updateInput value validator =
    let
        constraints =
            { none =
                validator.constraints.none
                    ++ validator.constraints.valid
                    ++ validator.constraints.invalid
            , valid = []
            , invalid = []
            }

        mandatory =
            case validator.mandatory of
                Required _ ->
                    Required None

                Optional ->
                    Optional
    in
    { validator
        | input = Input value
        , mandatory = mandatory
        , constraints = constraints
    }


addConstraints : List Constraint -> Validator value -> Validator value
addConstraints newConstraints ({ input, constraints } as validator) =
    let
        value_ =
            case input of
                Input i ->
                    i
    in
    { validator
        | constraints =
            { constraints
                | none = constraints.none ++ newConstraints
            }
    }
        |> updateInput value_


newValidator : value -> (value -> Maybe String) -> Bool -> List Constraint -> Validator value
newValidator value transform isMandatory constraints =
    let
        mandatory =
            if isMandatory then
                Required None

            else
                Optional
    in
    { input = Input value
    , transform = Transform transform
    , mandatory = mandatory
    , constraints =
        { none = constraints
        , valid = []
        , invalid = []
        }
    }


custom : TestFn -> TranslationFn -> List Constraint -> List Constraint
custom testFn translationFn constraints =
    { test = testFn
    , defaultError = translationFn
    }
        :: constraints


positive : List Constraint -> List Constraint
positive constraints =
    { test =
        \v ->
            case String.toFloat v of
                Nothing ->
                    False

                Just num ->
                    num > 0
    , defaultError =
        \translations ->
            I18Next.t translations (numTranslation "positive")
    }
        :: constraints


negative : List Constraint -> List Constraint
negative constraints =
    { test =
        \v ->
            case String.toFloat v of
                Nothing ->
                    False

                Just num ->
                    num < 0
    , defaultError =
        \translations ->
            I18Next.t translations (numTranslation "negative")
    }
        :: constraints


isOdd : List Constraint -> List Constraint
isOdd constraints =
    { test =
        \v ->
            case String.toInt v of
                Nothing ->
                    False

                Just num ->
                    modBy 2 num /= 0
    , defaultError =
        \translations ->
            I18Next.t translations (numTranslation "is_odd")
    }
        :: constraints


greaterThan : Float -> List Constraint -> List Constraint
greaterThan base constraints =
    { test =
        \v ->
            case String.toFloat v of
                Nothing ->
                    False

                Just num ->
                    num > base
    , defaultError =
        \translations ->
            I18Next.tr translations I18Next.Curly (numTranslation "greater_than") [ ( "base", String.fromFloat base ) ]
    }
        :: constraints


greaterThanOrEqual : Float -> List Constraint -> List Constraint
greaterThanOrEqual base constraints =
    { test =
        \v ->
            case String.toFloat v of
                Nothing ->
                    False

                Just num ->
                    num >= base
    , defaultError =
        \translations ->
            I18Next.tr translations I18Next.Curly (numTranslation "greater_than_or_equal") [ ( "base", String.fromFloat base ) ]
    }
        :: constraints


lengthGreaterThanOrEqual : Int -> List Constraint -> List Constraint
lengthGreaterThanOrEqual count constraints =
    { test =
        \v ->
            Maybe.withDefault 0 (String.toInt v) >= count
    , defaultError =
        \translations ->
            I18Next.tr translations I18Next.Curly (numTranslation "length_greater_than_or_equal") [ ( "count", String.fromInt count ) ]
    }
        :: constraints


lowerThan : Float -> List Constraint -> List Constraint
lowerThan base constraints =
    { test =
        \v ->
            case String.toFloat v of
                Nothing ->
                    False

                Just num ->
                    num < base
    , defaultError =
        \translations ->
            I18Next.tr translations I18Next.Curly (numTranslation "lower_than") [ ( "base", String.fromFloat base ) ]
    }
        :: constraints


lowerThanOrEqual : Float -> List Constraint -> List Constraint
lowerThanOrEqual base constraints =
    { test =
        \v ->
            case String.toFloat v of
                Nothing ->
                    False

                Just num ->
                    num <= base
    , defaultError =
        \translations ->
            I18Next.tr translations I18Next.Curly (numTranslation "lower_than_or_equal") [ ( "base", String.fromFloat base ) ]
    }
        :: constraints


onlyNumbers : List Constraint -> List Constraint
onlyNumbers constraints =
    { test =
        \v ->
            String.toList v
                |> List.all Char.isDigit
    , defaultError =
        \translations ->
            I18Next.t translations (textTranslation "only_numbers")
    }
        :: constraints


onlyLetters : List Constraint -> List Constraint
onlyLetters constraints =
    { test =
        \v ->
            String.toList v
                |> List.all Char.isAlpha
    , defaultError =
        \translations ->
            I18Next.t translations (textTranslation "only_letters")
    }
        :: constraints


exactly : Int -> List Constraint -> List Constraint
exactly base constraints =
    { test =
        \v ->
            String.length v == base
    , defaultError =
        \translations ->
            I18Next.tr translations I18Next.Curly (textTranslation "exactly") [ ( "base", String.fromInt base ) ]
    }
        :: constraints


longerThan : Int -> List Constraint -> List Constraint
longerThan base constraints =
    { test =
        \v ->
            String.length v > base
    , defaultError =
        \translations ->
            I18Next.tr translations I18Next.Curly (textTranslation "longer_than") [ ( "base", String.fromInt base ) ]
    }
        :: constraints


shorterThan : Int -> List Constraint -> List Constraint
shorterThan base constraints =
    { test =
        \v ->
            String.length v < base
    , defaultError =
        \translations ->
            I18Next.tr translations I18Next.Curly (textTranslation "shorter_than") [ ( "base", String.fromInt base ) ]
    }
        :: constraints


oneOf : List String -> List Constraint -> List Constraint
oneOf strings constraints =
    { test =
        \v ->
            List.any
                (\string ->
                    v == string
                )
                strings
    , defaultError =
        \translations ->
            I18Next.tr translations I18Next.Curly (enumTranslation "one_of") [ ( "base", String.join ", " strings ) ]
    }
        :: constraints


noneOf : List String -> List Constraint -> List Constraint
noneOf strings constraints =
    { test =
        \v ->
            List.any
                (\string ->
                    v == string
                )
                strings
                |> not
    , defaultError =
        \translations ->
            I18Next.tr translations I18Next.Curly (enumTranslation "none_of") [ ( "base", String.join ", " strings ) ]
    }
        :: constraints



-- Validators


validate : Validator value -> Validator value
validate ({ input, transform, mandatory, constraints } as validator) =
    let
        maybeValue =
            case ( input, transform ) of
                ( Input i, Transform t ) ->
                    t i
    in
    case ( mandatory, maybeValue ) of
        ( Required _, Just value ) ->
            { validator
                | mandatory = Required Valid
                , constraints = validateConstraints value constraints
            }

        ( Required _, Nothing ) ->
            { validator
                | mandatory = Required Invalid
            }

        ( Optional, Just value ) ->
            { validator
                | constraints = validateConstraints value constraints
            }

        ( Optional, Nothing ) ->
            validator


validateConstraints : String -> Constraints Constraint -> Constraints Constraint
validateConstraints value prevConstraints =
    List.foldl
        (testRunner value)
        noConstraints
        (prevConstraints.none ++ prevConstraints.invalid)


testRunner : String -> Constraint -> Constraints Constraint -> Constraints Constraint
testRunner value constraint constraints =
    if constraint.test value then
        { constraints
            | valid = constraint :: constraints.valid
        }

    else
        { constraints
            | invalid = constraint :: constraints.invalid
        }



-- Error handlers


hasErrors : Validator value -> Bool
hasErrors validator =
    hasMandatoryError validator
        || hasConstraintErrors validator


toCustomResult : error -> success -> Validator value -> Result error success
toCustomResult error success validator =
    if hasErrors validator then
        Err error

    else
        Ok success


hasMandatoryError : Validator value -> Bool
hasMandatoryError { mandatory } =
    case mandatory of
        Required Invalid ->
            True

        _ ->
            False


hasConstraintErrors : Validator value -> Bool
hasConstraintErrors { constraints } =
    not (List.isEmpty constraints.invalid)


listErrors : Translations -> Validator value -> List String
listErrors translations { constraints } =
    List.map
        (\constraint ->
            constraint.defaultError translations
        )
        constraints.invalid



-- Helpers


getInput : Validator value -> value
getInput { input } =
    case input of
        Input value ->
            value


noConstraints : Constraints c
noConstraints =
    { none = []
    , valid = []
    , invalid = []
    }


numTranslation : String -> String
numTranslation path =
    "error.validator.number." ++ path


textTranslation : String -> String
textTranslation path =
    "error.validator.text." ++ path


enumTranslation : String -> String
enumTranslation path =
    "error.validator.enum." ++ path
