module Session.Shared exposing (ScatterAvailability(..), Shared, TranslationStatus(..), bespiralSymbol, gotScatterAvailability, init, language, loadTranslation, toLoadingTranslation, translationStatus, verifyingScatterAvailability, viewFullError, viewFullGraphqlError, viewFullLoading, viewLanguageItems)

import Account
import Asset.Icon as Icon
import Browser.Navigation as Nav
import Community
import Eos exposing (Symbol)
import Eos.Account as Eos
import Flags exposing (AuthPreference, Endpoints, Environment, Flags, defaultEndpoints)
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (Translations, initialTranslations, t)
import Time exposing (Posix)


type alias Shared =
    { navKey : Nav.Key
    , language : String
    , translations : Translations
    , translationsStatus : TranslationStatus
    , environment : Environment
    , scatter : ScatterAvailability
    , maybeAccount : Maybe ( Eos.Name, Bool )
    , authPreference : Maybe AuthPreference
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Posix
    , allowCommunityCreation : Bool
    }


init : Flags -> Nav.Key -> Shared
init ({ environment, maybeAccount, authPreference, endpoints, allowCommunityCreation } as flags) navKey =
    { navKey = navKey
    , language = flags.language
    , translations = initialTranslations
    , translationsStatus = LoadingTranslation
    , environment = environment
    , scatter = VerifyingScatterAvailability
    , maybeAccount = maybeAccount
    , authPreference = authPreference
    , endpoints = endpoints
    , logo = flags.logo
    , logoMobile = flags.logoMobile
    , now = Time.millisToPosix flags.now
    , allowCommunityCreation = allowCommunityCreation
    }


type TranslationStatus
    = LoadingTranslation
    | LoadingTranslationFailed Http.Error
    | LoadedTranslation
    | LoadingAnotherTranslation
    | LoadingAnotherTranslationFailed Http.Error


type ScatterAvailability
    = VerifyingScatterAvailability
    | ScatterAvailable
    | ScatterUnavailable



-- INFO


language : Shared -> String
language shared =
    shared.language


translationStatus : Shared -> TranslationStatus
translationStatus shared =
    shared.translationsStatus


bespiralSymbol : Shared -> Symbol
bespiralSymbol shared =
    Eos.bespiralSymbol



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


gotScatterAvailability : Bool -> Shared -> Shared
gotScatterAvailability isAvailable shared =
    { shared
        | scatter =
            if isAvailable then
                ScatterAvailable

            else
                ScatterUnavailable
    }


verifyingScatterAvailability : Shared -> Shared
verifyingScatterAvailability shared =
    { shared | scatter = VerifyingScatterAvailability }



-- VIEW


viewLanguageItems : Shared -> (String -> msg) -> List (Html msg)
viewLanguageItems shared toMsg =
    List.map
        (\lang ->
            button
                [ classList
                    [ ( "user-nav__item", True )
                    , ( "user-nav__item--active"
                      , String.startsWith lang shared.language
                      )
                    ]
                , onClick (toMsg lang)
                ]
                [ langFlag lang
                , span [ class "lang__item__text border-t border-gray-500 w-full text-left" ] [ text (String.toUpper lang) ]
                ]
        )
        [ "en", "pt-br", "es", "cat" ]


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
    img [ src iconLink, class "main__header__language" ] []


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
