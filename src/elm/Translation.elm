module Translation exposing
    ( Language(..)
    , allLanguages
    , defaultLanguage
    , get
    , languageFromLanguageCode
    , languageFromLocale
    , languageToLanguageCode
    , languageToLocale
    )

import Http
import I18Next exposing (Translations)
import Url.Builder


type Language
    = English
    | Portuguese
    | Spanish
    | Catalan
    | Amharic


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


allLanguages : List Language
allLanguages =
    [ English, Portuguese, Spanish, Catalan, Amharic ]


defaultLanguage : Language
defaultLanguage =
    English


get : Language -> (Result Http.Error Translations -> msg) -> Cmd msg
get language toMsg =
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
        { url = Url.Builder.absolute [ "translations", translation ] []
        , expect = Http.expectJson toMsg I18Next.translationsDecoder
        }
