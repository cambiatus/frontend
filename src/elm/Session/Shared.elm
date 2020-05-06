module Session.Shared exposing
    ( Shared
    , TranslationStatus(..)
    , init
    , langFlag
    , language
    , loadTranslation
    , toLoadingTranslation
    , translationStatus
    , viewFullError
    , viewFullGraphqlError
    , viewFullLoading
    , viewLanguageItems
    )

import Browser.Navigation as Nav
import Eos exposing (Symbol)
import Eos.Account as Eos
import Flags exposing (Endpoints, Environment, Flags, defaultEndpoints)
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (Translations, initialTranslations, t)
import Time exposing (Posix)
import Url exposing (Url)


type alias Shared =
    { navKey : Nav.Key
    , language : String
    , translations : Translations
    , translationsStatus : TranslationStatus
    , environment : Environment
    , maybeAccount : Maybe ( Eos.Name, Bool )
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Posix
    , allowCommunityCreation : Bool
    , url : Url
    }


init : Flags -> Nav.Key -> Url -> Shared
init ({ environment, maybeAccount, endpoints, allowCommunityCreation } as flags) navKey url =
    { navKey = navKey
    , language = flags.language
    , translations = initialTranslations
    , translationsStatus = LoadingTranslation
    , environment = environment
    , maybeAccount = maybeAccount
    , endpoints = endpoints
    , logo = flags.logo
    , logoMobile = flags.logoMobile
    , now = Time.millisToPosix flags.now
    , allowCommunityCreation = allowCommunityCreation
    , url = url
    }


type TranslationStatus
    = LoadingTranslation
    | LoadingTranslationFailed Http.Error
    | LoadedTranslation
    | LoadingAnotherTranslation
    | LoadingAnotherTranslationFailed Http.Error



-- INFO


language : Shared -> String
language shared =
    shared.language


translationStatus : Shared -> TranslationStatus
translationStatus shared =
    shared.translationsStatus



-- TRANSFORM


toLoadingTranslation : Shared -> Shared
toLoadingTranslation shared =
    { shared
        | translationsStatus =
            case shared.translationsStatus of
                LoadedTranslation ->
                    LoadingAnotherTranslation

                LoadingAnotherTranslationFailed _ ->
                    LoadingAnotherTranslation

                _ ->
                    LoadingTranslation
    }


loadTranslation : Result Http.Error ( String, Translations ) -> Shared -> Shared
loadTranslation result shared =
    case result of
        Err err ->
            { shared
                | translationsStatus =
                    case shared.translationsStatus of
                        LoadingTranslation ->
                            LoadingTranslationFailed err

                        LoadingAnotherTranslation ->
                            LoadingAnotherTranslationFailed err

                        _ ->
                            shared.translationsStatus
            }

        Ok ( language_, translations ) ->
            { shared
                | language = language_
                , translations = translations
                , translationsStatus = LoadedTranslation
            }



-- VIEW


viewLanguageItems : Shared -> (String -> msg) -> List (Html msg)
viewLanguageItems shared toMsg =
    [ "en", "pt-br", "es", "cat" ]
        |> List.filter (\l -> not (String.startsWith l shared.language))
        |> List.sort
        |> List.map
            (\lang ->
                button
                    [ class "flex block px-4 py-2 text-gray justify-between items-center text-xs"
                    , onClick (toMsg lang)
                    ]
                    [ langFlag lang
                    , text (String.toUpper lang)
                    ]
            )


langFlag : String -> Html msg
langFlag st =
    let
        iconLink =
            if String.startsWith "cat" st then
                "/icons/cat-lang.svg"

            else if String.startsWith "p" st then
                "/icons/portuguese-lang.svg"

            else if String.startsWith "es" st then
                "/icons/spain-lang.svg"

            else
                "/icons/en-lang.svg"
    in
    img
        [ class "object-cover w-6 h-6"
        , class "lang-flag"
        , src iconLink
        ]
        []


viewFullLoading : Html msg
viewFullLoading =
    div [ class "full-page-loading full-spinner-container" ]
        [ div [ class "spinner" ] [] ]


viewFullError : Shared -> Http.Error -> msg -> String -> Html msg
viewFullError shared err msg msgText =
    div [ class "full-page-loading full-spinner-container" ]
        [ p [] [ text msgText ]
        , button [ onClick msg ] [ text (t shared.translations "menu.try_again") ]
        ]


viewFullGraphqlError : Shared -> Graphql.Http.Error e -> msg -> String -> Html msg
viewFullGraphqlError shared err msg msgText =
    div [ class "full-page-loading full-spinner-container" ]
        [ p [] [ text msgText ]
        , button [ onClick msg ] [ text (t shared.translations "menu.try_again") ]
        ]
