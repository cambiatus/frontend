module Session.Shared exposing
    ( Environment(..)
    , Shared
    , TranslationStatus(..)
    , Translators
    , communityDomain
    , environmentFromUrl
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


type alias Shared =
    { navKey : Nav.Key
    , language : Translation.Language
    , translations : Translations
    , translators : Translators
    , translationsStatus : TranslationStatus
    , environment : Environment
    , maybeAccount : Maybe ( Eos.Name, Bool )
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Posix
    , timezone : Time.Zone
    , allowCommunityCreation : Bool
    , url : Url
    , contracts : { token : String, community : String }
    , graphqlSecret : String
    , canReadClipboard : Bool
    , useSubdomain : Bool
    , selectedCommunity : Maybe Eos.Symbol
    }


init : Flags -> Nav.Key -> Url -> ( Shared, Cmd msg )
init ({ maybeAccount, endpoints, allowCommunityCreation, tokenContract, communityContract } as flags) navKey url =
    let
        environment =
            environmentFromUrl url
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
      , graphqlSecret = flags.graphqlSecret
      , canReadClipboard = flags.canReadClipboard
      , useSubdomain = flags.useSubdomain
      , selectedCommunity = flags.selectedCommunity
      }
    , case environment of
        Production ->
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


translationStatus : Shared -> TranslationStatus
translationStatus shared =
    shared.translationsStatus



-- ENVIRONMENT


type Environment
    = Development
    | Staging
    | Demo
    | Production


environmentFromUrl : Url -> Environment
environmentFromUrl url =
    if String.endsWith ".localhost" url.host then
        Development

    else if String.endsWith ".staging.cambiatus.io" url.host then
        Staging

    else if String.endsWith ".demo.cambiatus.io" url.host then
        Demo

    else if String.endsWith ".cambiatus.io" url.host then
        Production

    else
        Staging


{-| Get the community subdomain and the current environment, based on current
url. Example possible outputs:

    [ "cambiatus", "staging" ] -- Cambiatus community in the staging environment

    [ "cambiatus", "demo" ] -- Cambiatus community in the demo environment

    [ "cambiatus" ] -- Cambiatus community in the prod environment

-}
communitySubdomainParts : Url -> Environment -> List String
communitySubdomainParts url environment =
    let
        allParts =
            url.host |> String.split "."

        isStaging =
            case environmentFromUrl url of
                Development ->
                    True

                Staging ->
                    True

                _ ->
                    False

        addStaging parts =
            if isStaging then
                parts ++ [ "staging" ]

            else
                parts
    in
    case allParts of
        [] ->
            addStaging [ "cambiatus" ]

        [ subdomain ] ->
            addStaging [ subdomain ]

        subdomain :: "localhost" :: _ ->
            [ subdomain, "staging" ]

        subdomain :: "cambiatus" :: _ ->
            [ subdomain ]

        subdomain :: env :: _ ->
            [ subdomain, env ]


{-| Returns the full `subdomain` of a community based on the current url

Note: it takes an extensible record just so we can test it. The reason we can't
pass in the entire `Shared` is because we can't create one without a `Nav.Key`
(as stated in an [issue](https://github.com/elm-explorations/test/issues/24) on
the `elm-explorations/test` repo)

-}
communityDomain : { shared | url : Url, environment : Environment } -> String
communityDomain shared =
    String.join "."
        (communitySubdomainParts shared.url shared.environment
            ++ [ "cambiatus", "io" ]
        )



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
                    [ class "flex px-4 py-2 text-gray justify-between items-center text-sm uppercase"
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
        [ class "object-cover rounded-full w-6 h-6 lang-flag mr-2"
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
