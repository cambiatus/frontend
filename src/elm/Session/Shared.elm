module Session.Shared exposing
    ( Shared
    , TranslationStatus(..)
    , Translators
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
import Eos
import Eos.Account as Eos
import Flags exposing (Endpoints, Environment, Flags)
import Graphql.Http
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (Translations, initialTranslations, t)
import Time exposing (Posix)
import Url exposing (Url)


type alias Shared =
    { navKey : Nav.Key
    , language : String
    , translations : Translations
    , translators : Translators
    , translationsStatus : TranslationStatus
    , environment : Environment
    , maybeAccount : Maybe ( Eos.Name, Bool )
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Posix
    , allowCommunityCreation : Bool
    , url : Url
    , contracts : { token : String, community : String }
    , graphqlSecret : String
    , canReadClipboard : Bool
    , useSubdomain : Bool
    }


init : Flags -> Nav.Key -> Url -> Shared
init ({ environment, maybeAccount, endpoints, allowCommunityCreation, tokenContract, communityContract } as flags) navKey url =
    { navKey = navKey
    , language = flags.language
    , translations = initialTranslations
    , translators = makeTranslators initialTranslations
    , translationsStatus = LoadingTranslation
    , environment = environment
    , maybeAccount = maybeAccount
    , endpoints = endpoints
    , logo = flags.logo
    , logoMobile = flags.logoMobile
    , now = Time.millisToPosix flags.now
    , allowCommunityCreation = allowCommunityCreation
    , url = url
    , contracts = { token = tokenContract, community = communityContract }
    , graphqlSecret = flags.graphqlSecret
    , canReadClipboard = flags.canReadClipboard
    , useSubdomain = flags.useSubdomain
    }


type TranslationStatus
    = LoadingTranslation
    | LoadingTranslationFailed Http.Error
    | LoadedTranslation
    | LoadingAnotherTranslation
    | LoadingAnotherTranslationFailed Http.Error



-- TRANSLATORS


{-| Contains functions with bounded dictionaries for translating plain strings and strings with placeholders.
-}
type alias Translators =
    { t : String -> String
    , tr : String -> I18Next.Replacements -> String
    }


makeTranslators : Translations -> Translators
makeTranslators translations =
    let
        t : String -> String
        t =
            I18Next.t translations

        tr : String -> I18Next.Replacements -> String
        tr =
            I18Next.tr translations I18Next.Curly
    in
    { t = t
    , tr = tr
    }



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
                , translators = makeTranslators translations
                , translationsStatus = LoadedTranslation
            }



-- VIEW


viewLanguageItems : Shared -> (String -> msg) -> List (Html msg)
viewLanguageItems shared toMsg =
    [ "en", "pt-br", "es", "cat", "amh" ]
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
                "/icons/flag-catalan.svg"

            else if String.startsWith "p" st then
                "/icons/flag-brazil.svg"

            else if String.startsWith "es" st then
                "/icons/flag-spain.svg"

            else if String.startsWith "amh" st then
                "/icons/flag-ethiopia.svg"

            else
                "/icons/flag-usa.svg"
    in
    img
        [ class "object-cover w-6 h-6 lang-flag"
        , src iconLink
        ]
        []


viewFullLoading : Html msg
viewFullLoading =
    div [ class "full-page-loading full-spinner-container" ]
        [ div [ class "spinner" ] [] ]


viewFullError : Shared -> Http.Error -> msg -> String -> Html msg
viewFullError shared _ msg msgText =
    div [ class "full-page-loading full-spinner-container" ]
        [ p [] [ text msgText ]
        , button [ onClick msg ] [ text (t shared.translations "menu.try_again") ]
        ]


viewFullGraphqlError : Shared -> Graphql.Http.Error e -> msg -> String -> Html msg
viewFullGraphqlError shared _ msg msgText =
    div [ class "full-page-loading full-spinner-container" ]
        [ p [] [ text msgText ]
        , button [ onClick msg ] [ text (t shared.translations "menu.try_again") ]
        ]
