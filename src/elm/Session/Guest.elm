module Session.Guest exposing (External(..), Model, Msg(..), Page(..), addAfterLoginRedirect, init, initModel, msgToString, update, view)

import Account exposing (Profile)
import Api
import Asset.Icon as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput, onSubmit)
import Http
import I18Next exposing (Delims(..), Translations, t, tr)
import Icons
import Log
import Ports
import Route exposing (Route)
import Session.Shared as Shared exposing (Shared)
import Time
import Translation
import UpdateResult as UR



-- INIT


init : Shared -> ( Model, Cmd Msg )
init shared =
    ( initModel shared
    , Cmd.none
    )


fetchTranslations : String -> Shared -> Cmd Msg
fetchTranslations language shared =
    CompletedLoadTranslation language
        |> Translation.get language



-- MODEL


type alias Model =
    { shared : Shared
    , showLanguageNav : Bool
    , afterLoginRedirect : Maybe Route
    , profile : Maybe Profile
    }


initModel : Shared -> Model
initModel shared =
    { shared = shared
    , showLanguageNav = False
    , afterLoginRedirect = Nothing
    , profile = Nothing
    }



-- VIEW


type Page
    = Other
    | Shop


view : (Msg -> msg) -> Page -> Model -> Html msg -> Html msg
view thisMsg page ({ shared } as model) content =
    case Shared.translationStatus shared of
        Shared.LoadingTranslation ->
            Shared.viewFullLoading

        Shared.LoadingTranslationFailed err ->
            Shared.viewFullError shared
                err
                ClickedTryAgainTranslation
                "An error ocurred while loading translation."
                |> Html.map thisMsg

        _ ->
            let
                onClickCloseLanguageNav =
                    if model.showLanguageNav then
                        onClick (ShowLanguageNav False)

                    else
                        style "" ""

                currentYear : String
                currentYear =
                    Time.toYear Time.utc shared.now
                        |> String.fromInt

                arrowClass : String
                arrowClass =
                    if model.showLanguageNav then
                        "main-header__info-arrow main-header__info-arrow--up"

                    else
                        "main-header__info-arrow"

                langIconPath : String
                langIconPath =
                    if String.startsWith "pt" shared.language then
                        "/icons/portuguese-lang.svg"

                    else if String.startsWith "cat" shared.language then
                        "/icons/cat-lang.svg"

                    else if String.startsWith "es" shared.language then
                        "/icons/spain-lang.svg"

                    else
                        "/icons/en-lang.svg"
            in
            div
                [ class "main-login-grid min-h-screen"
                ]
                [ header
                    [ class "main-header" ]
                    [ a
                        [ class "main-header__logo"
                        , Route.href (Route.Login Nothing)
                        , onClickCloseLanguageNav
                        ]
                        [ img
                            [ class "lg:hidden left-0 absolute ml-4", src shared.logoMobile ]
                            []
                        , img
                            [ class "hidden lg:block lg:visible object-none object-scale-down", src shared.logo ]
                            []
                        ]
                    , div
                        [ class "main-header__space"
                        , onClickCloseLanguageNav
                        ]
                        []
                    , button
                        [ class "btn main-header__language z-40"
                        , onClick (ShowLanguageNav (not model.showLanguageNav))
                        ]
                        [ img [ src langIconPath, class "main__header__language" ] []
                        , span [ class "main-header__info-name" ]
                            [ text shared.language ]
                        , img [ src "/icons/arrow.svg", class arrowClass ] []
                        , Icons.arrowDown arrowClass
                        ]
                    ]
                    |> Html.map thisMsg
                , main_
                    [ id "main-content"
                    , class "min-h-screen main-content__guest flex-wrap flex items-center justify-center outline-none bg-local"
                    , if Time.toHour Time.utc shared.now >= 6 && Time.toHour Time.utc shared.now <= 18 then
                        style "background-image" "url('/images/login-bg-day.png')"

                      else
                        style "background-image" "url('/images/login-bg-night.png')"
                    , tabindex -1
                    ]
                    [ content
                    , footer [ class "main-footer w-full", id "guest-footer" ]
                        [ p [ class "main-footer__text main-footer__text--login" ]
                            [ text ("Copyrights © " ++ currentYear ++ " • Cambiatus") ]
                        ]
                    ]
                , div
                    [ classList
                        [ ( "content-screen", True )
                        , ( "content-screen--dark", model.showLanguageNav )
                        ]
                    , onClick (ShowLanguageNav False)
                    ]
                    []
                    |> Html.map thisMsg
                , nav
                    [ classList
                        [ ( "user-nav", True )
                        , ( "guest-nav--show shadow-lg rounded", model.showLanguageNav )
                        ]
                    ]
                    (Shared.viewLanguageItems shared ClickedLanguage)
                    |> Html.map thisMsg
                ]



-- UPDATE


type External
    = UpdatedGuest Model


type alias UpdateResult =
    UR.UpdateResult Model Msg ()


type Msg
    = CompletedLoadTranslation String (Result Http.Error Translations)
    | ClickedTryAgainTranslation
    | ShowLanguageNav Bool
    | ClickedLanguage String


update : Msg -> Model -> UpdateResult
update msg ({ shared } as model) =
    case msg of
        CompletedLoadTranslation lang (Ok transl) ->
            { model | shared = Shared.loadTranslation (Ok ( lang, transl )) shared }
                |> UR.init
                |> UR.addCmd (Ports.storeLanguage lang)

        CompletedLoadTranslation lang (Err err) ->
            { model | shared = Shared.loadTranslation (Err err) shared }
                |> UR.init
                |> UR.logHttpError msg err

        ClickedTryAgainTranslation ->
            { model | shared = Shared.toLoadingTranslation shared }
                |> UR.init
                |> UR.addCmd (fetchTranslations (Shared.language shared) shared)

        ShowLanguageNav b ->
            UR.init { model | showLanguageNav = b }

        ClickedLanguage lang ->
            { model
                | shared = Shared.toLoadingTranslation shared
                , showLanguageNav = False
            }
                |> UR.init
                |> UR.addCmd (fetchTranslations lang shared)



-- TRANSFORM


addAfterLoginRedirect : Route -> Model -> Model
addAfterLoginRedirect route model =
    { model | afterLoginRedirect = Just route }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadTranslation _ r ->
            [ "CompletedLoadTranslation", UR.resultToString r ]

        ClickedTryAgainTranslation ->
            [ "ClickedTryAgainTranslation" ]

        ShowLanguageNav _ ->
            [ "ShowLanguageNav" ]

        ClickedLanguage _ ->
            [ "ClickedLanguage" ]
