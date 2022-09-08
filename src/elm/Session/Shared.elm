module Session.Shared exposing
    ( Bip39Status(..)
    , Shared
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
import Json.Encode
import Ports
import Set exposing (Set)
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

    -- Bip39 is what we use to generate the 12 words to login. We store them in
    -- Shared so that we don't need more HTTP requests than necessary (or to
    -- store the words hardcoded into Elm code, increasing the bundle size).
    -- We use Bip39 in the login page to validate the 12 words
    , bip39 : Bip39Status
    }


init : Flags -> Nav.Key -> Url -> ( Shared, Cmd msg )
init ({ maybeAccount, endpoints, allowCommunityCreation, tokenContract, communityContract } as flags) navKey url =
    let
        environment =
            Environment.fromUrl url

        addMatomoScriptCmd =
            Ports.javascriptOut
                -- We don't need a responseAddress because we don't need a response
                { responseAddress = ()
                , responseData = Json.Encode.null
                , data =
                    Json.Encode.object
                        [ ( "name", Json.Encode.string "addMatomoScript" )
                        ]
                }
                |> Ports.javascriptOutCmd (\_ -> [])
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
      , bip39 = Bip39NotLoaded
      }
    , case environment of
        Environment.Production ->
            addMatomoScriptCmd

        _ ->
            Cmd.none
    )


type Bip39Status
    = Bip39Loaded { english : Set String, portuguese : Set String, spanish : Set String }
    | Bip39NotLoaded


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


viewLanguageItems :
    { containerAttrs : List (Html.Attribute msg)
    , flagIconAttrs : List (Html.Attribute msg)
    }
    -> Shared
    -> (Translation.Language -> msg)
    -> List (Html msg)
viewLanguageItems { containerAttrs, flagIconAttrs } shared toMsg =
    Translation.allLanguages
        |> List.filter (\l -> l /= shared.language)
        |> List.sortBy Translation.languageToLocale
        |> List.map
            (\lang ->
                button (onClick (toMsg lang) :: containerAttrs)
                    [ langFlag flagIconAttrs lang
                    , text (Translation.languageToLanguageCode lang)
                    ]
            )


langFlag : List (Html.Attribute msg) -> Translation.Language -> Html msg
langFlag attrs language =
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
        (class "object-cover rounded-full"
            :: src iconLink
            :: attrs
        )
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
