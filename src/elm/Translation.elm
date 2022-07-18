module Translation exposing
    ( Language(..)
    , Translators
    , allLanguages
    , decimalSeparators
    , defaultLanguage
    , floatStringFromSeparatedString
    , get
    , languageFromLanguageCode
    , languageFromLocale
    , languageToLanguageCode
    , languageToLocale
    )

import Http
import I18Next exposing (Translations)
import Mask
import Url.Builder
import Version exposing (Version)


type Language
    = English
    | Portuguese
    | Spanish
    | Catalan
    | Amharic


type alias Translators =
    { t : String -> String
    , tr : String -> I18Next.Replacements -> String
    }


languageFromLanguageCode : String -> Maybe Language
languageFromLanguageCode locale =
    case String.toLower locale of
        "en-us" ->
            Just English

        "pt-br" ->
            Just Portuguese

        "es" ->
            Just Spanish

        "cat" ->
            Just Catalan

        "amh" ->
            Just Amharic

        _ ->
            Nothing


languageToLanguageCode : Language -> String
languageToLanguageCode language =
    case language of
        English ->
            "en-US"

        Portuguese ->
            "pt-br"

        Spanish ->
            "es"

        Catalan ->
            "cat"

        Amharic ->
            "amh"


languageToLocale : Language -> String
languageToLocale language =
    case language of
        English ->
            "en-us"

        Portuguese ->
            "pt-br"

        Spanish ->
            "es-es"

        Catalan ->
            "ca-es"

        Amharic ->
            "am-et"


languageFromLocale : String -> Maybe Language
languageFromLocale locale =
    case String.toLower locale of
        "en-us" ->
            Just English

        "pt-br" ->
            Just Portuguese

        "es-es" ->
            Just Spanish

        "ca-es" ->
            Just Catalan

        "am-et" ->
            Just Amharic

        _ ->
            Nothing


decimalSeparators : Translators -> { decimalSeparator : String, thousandsSeparator : String }
decimalSeparators translators =
    { decimalSeparator = translators.t "decimal_separator"
    , thousandsSeparator = translators.t "thousands_separator"
    }


{-| Normalize a masked float string into a string that can be parsed into a float
by Elm
-}
floatStringFromSeparatedString : Translators -> String -> String
floatStringFromSeparatedString translators =
    Mask.removeFloat (decimalSeparators translators)


allLanguages : List Language
allLanguages =
    [ English, Portuguese, Spanish, Catalan, Amharic ]


defaultLanguage : Language
defaultLanguage =
    English


get : Version -> Language -> (Result Http.Error Translations -> msg) -> Cmd msg
get version language toMsg =
    let
        translation =
            case language of
                English ->
                    "en-US.json"

                Portuguese ->
                    "pt-BR.json"

                Spanish ->
                    "es-ES.json"

                Catalan ->
                    "cat-CAT.json"

                Amharic ->
                    "amh-ETH.json"
    in
    Http.get
        { url = Url.Builder.absolute [ "translations", translation ] [ Url.Builder.string "version" (Version.toString version) ]
        , expect = Http.expectJson toMsg I18Next.translationsDecoder
        }
