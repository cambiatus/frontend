module Session.Shared exposing
    ( Shared
    , TranslationStatus(..)
    , Translators
    , init
    , langFlag
    , loadTranslation
    , toLoadingTranslation
    , translationStatus
    , viewFullError
    , viewFullGraphqlError
    , viewFullLoading
    , viewLanguageItems
    )

import Browser.Navigation as Nav
import Environment exposing (Environment)
import Eos
import Eos.Account as Eos
import Flags exposing (Endpoints, Flags)
import Graphql.Http
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (Translations, initialTranslations)
import Ports
import Time exposing (Posix)
import Translation
import Url exposing (Url)
import Version exposing (Version)


type alias Shared =
    { navKey : Nav.Key
    , language : Translation.Language
    , version : Version
    , translations : Translations
    , translators : Translators
    , translationsStatus : TranslationStatus
    , environment : Environment
    , maybeAccount : Maybe Eos.Name
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Posix
    , timezone : Time.Zone
    , allowCommunityCreation : Bool
    , url : Url
    , contracts : { token : String, community : String }
    , canReadClipboard : Bool
    , canShare : Bool
    , useSubdomain : Bool
    , selectedCommunity : Maybe Eos.Symbol
    , pinVisibility : Bool
    , hasSeenSponsorModal : Bool
    }


init : Flags -> Nav.Key -> Url -> ( Shared, Cmd msg )
init ({ maybeAccount, endpoints, allowCommunityCreation, tokenContract, communityContract } as flags) navKey url =
    let
        environment =
            Environment.fromUrl url
    in
    ( { navKey = navKey
      , language =
            -- We need to try parsing with `fromLanguageCode` first for
            -- backwards-compatiblity. In some time, we should switch to just trying
            -- with `languageFromLocale`
            case flags.language |> Translation.languageFromLanguageCode of
                Just lang ->
                    lang

                Nothing ->
                    flags.language
                        |> Translation.languageFromLocale
                        |> Maybe.withDefault Translation.defaultLanguage
      , version = flags.version
      , translations = initialTranslations
      , translators = makeTranslators initialTranslations
      , translationsStatus = LoadingTranslation
      , environment = environment
      , maybeAccount = maybeAccount
      , endpoints = endpoints
      , logo = flags.logo
      , logoMobile = flags.logoMobile
      , now = Time.millisToPosix flags.now
      , timezone = Time.utc
      , allowCommunityCreation = allowCommunityCreation
      , url = url
      , contracts = { token = tokenContract, community = communityContract }
      , canReadClipboard = flags.canReadClipboard
      , canShare = flags.canShare
      , useSubdomain = flags.useSubdomain
      , selectedCommunity = flags.selectedCommunity
      , pinVisibility = flags.pinVisibility
      , hasSeenSponsorModal = flags.hasSeenSponsorModal
      }
    , case environment of
        Environment.Production ->
            Ports.addPlausibleScript { domain = url.host, src = "https://plausible.io/js/plausible.js" }

        _ ->
            Cmd.none
    )


type TranslationStatus
    = LoadingTranslation
    | LoadingTranslationFailed Http.Error
    | LoadedTranslation
    | LoadingAnotherTranslation
    | LoadingAnotherTranslationFailed



-- TRANSLATORS


{-| Contains functions with bounded dictionaries for translating plain strings and strings with placeholders.
-}
type alias Translators =
    Translation.Translators


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

                LoadingAnotherTranslationFailed ->
                    LoadingAnotherTranslation

                _ ->
                    LoadingTranslation
    }


loadTranslation : Result Http.Error ( Translation.Language, Translations ) -> Shared -> Shared
loadTranslation result shared =
    case result of
        Err err ->
            { shared
                | translationsStatus =
                    case shared.translationsStatus of
                        LoadingTranslation ->
                            LoadingTranslationFailed err

                        LoadingAnotherTranslation ->
                            LoadingAnotherTranslationFailed

                        _ ->
                            shared.translationsStatus
            }

        Ok ( language, translations ) ->
            { shared
                | language = language
                , translations = translations
                , translators = makeTranslators translations
                , translationsStatus = LoadedTranslation
            }



-- VIEW


viewLanguageItems : Shared -> (Translation.Language -> msg) -> List (Html msg)
viewLanguageItems shared toMsg =
    Translation.allLanguages
        |> List.filter (\l -> l /= shared.language)
        |> List.sortBy Translation.languageToLocale
        |> List.map
            (\lang ->
                button
                    [ class "flex px-4 py-2 text-gray justify-between items-center text-sm uppercase focus-ring rounded-sm hover:text-indigo-500 focus-visible:text-indigo-500"
                    , onClick (toMsg lang)
                    ]
                    [ langFlag lang
                    , text (Translation.languageToLanguageCode lang)
                    ]
            )


langFlag : Translation.Language -> Html msg
langFlag language =
    let
        iconLink =
            case language of
                Translation.English ->
                    "/icons/flag-usa.svg"

                Translation.Portuguese ->
                    "/icons/flag-brazil.svg"

                Translation.Spanish ->
                    "/icons/flag-spain.svg"

                Translation.Catalan ->
                    "/icons/flag-catalan.svg"

                Translation.Amharic ->
                    "/icons/flag-ethiopia.svg"
    in
    img
        [ class "object-cover rounded-full w-6 h-6 mr-2"
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
        , button [ onClick msg ] [ text (I18Next.t shared.translations "menu.try_again") ]
        ]


viewFullGraphqlError : Shared -> Graphql.Http.Error e -> msg -> String -> Html msg
viewFullGraphqlError shared _ msg msgText =
    div [ class "full-page-loading full-spinner-container" ]
        [ p [] [ text msgText ]
        , button [ onClick msg ] [ text (I18Next.t shared.translations "menu.try_again") ]
        ]
