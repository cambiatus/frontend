module Session.Guest exposing (External(..), Model, Msg(..), Page(..), addAfterLoginRedirect, init, initModel, msgToString, subscriptions, update, view)

import Api
import Asset.Icon as Icon
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput, onMouseEnter, onSubmit)
import Http
import I18Next exposing (Delims(..), Translations, t, tr)
import Icons
import Json.Decode as Decode
import Log
import Ports
import Profile exposing (Profile)
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeyDown (Browser.Events.onKeyDown (Decode.field "key" Decode.string))



-- VIEW


{-| Page types the Guest can access.
-}
type Page
    = Register
    | Login
    | Shop
    | Other


view : (Msg -> msg) -> Page -> Model -> Html msg -> Html msg
view thisMsg page ({ shared } as model) content =
    case Shared.translationStatus shared of
        Shared.LoadingTranslation ->
            Shared.viewFullLoading

        Shared.LoadingTranslationFailed err ->
            Shared.viewFullError shared
                err
                ClickedTryAgainTranslation
                "An error occurred while loading translation."
                |> Html.map thisMsg

        _ ->
            let
                contentBackgroundClass =
                    case page of
                        Login ->
                            "bg-purple-500"

                        _ ->
                            "bg-white"
            in
            div
                [ class "md:pl-1/3 lg:pl-1/2 md:bg-cover md:bg-center"
                , style "background-image" "url(images/login-bg-day.png)"
                ]
                [ viewQuote
                    { photoSrc = "images/register-dog.png"
                    , name = "Victoria Lane"
                    , occupation = "Founder"
                    , quote = "We are designing for a global user base, we can get feedback not just from drivers we see in San Francisco"
                    }
                , div
                    [ class "min-h-screen"
                    , class contentBackgroundClass
                    ]
                    [ div
                        [ class "md:max-w-lg md:pt-20 md:m-auto" ]
                        [ viewPageHeader model shared
                            |> Html.map thisMsg
                        , main_ []
                            [ div [] [ content ]
                            ]
                        ]
                    ]
                ]


viewPageHeader : Model -> Shared -> Html Msg
viewPageHeader model shared =
    header
        [ class "flex items-center justify-between px-4 py-3 bg-white" ]
        [ div []
            [ img
                [ class "lg:block h-6"
                , src shared.logo
                , alt "Cambiatus"
                ]
                []
            ]
        , div [ class "relative z-10" ]
            [ button
                [ type_ "button"
                , tabindex -1
                , class "flex block relative z-10 w-32 items-center px-4 py-2 bg-white text-xs focus:outline-none"
                , classList
                    [ ( "rounded-tr-lg rounded-tl-lg justify-between"
                      , model.showLanguageNav
                      )
                    ]
                , onClick (ShowLanguageNav (not model.showLanguageNav))
                , onMouseEnter (ShowLanguageNav True)
                ]
                [ Shared.langFlag shared.language
                , if model.showLanguageNav then
                    div [ class "flex-grow" ]
                        [ text (String.toUpper model.shared.language) ]

                  else
                    text ""
                , Icons.arrowDown "flex-none"
                ]
            , if model.showLanguageNav then
                button
                    [ class "fixed h-full w-full inset-0 bg-black opacity-50 cursor-default"
                    , onClick (ShowLanguageNav False)
                    , onMouseEnter (ShowLanguageNav False)
                    ]
                    []

              else
                text ""
            , div
                [ class "absolute right-0 w-32 py-2 bg-white border-t rounded-br-lg rounded-bl-lg shadow-lg"
                , classList
                    [ ( "hidden", not model.showLanguageNav )
                    ]
                ]
                (Shared.viewLanguageItems shared ClickedLanguage)
            ]
        ]


type alias Quote =
    { photoSrc : String
    , name : String
    , occupation : String
    , quote : String
    }


viewQuote : Quote -> Html msg
viewQuote { photoSrc, name, occupation, quote } =
    div
        [ class "lg:block lg:visible hidden md:absolute md:left-0 md:w-1/3 lg:w-1/2 md:p-10"
        ]
        [ div [ class "flex items-center mb-3" ]
            [ div [ class "rounded-full border-white border-2 bg-grey w-16 h-16" ]
                [ img
                    [ class "max-w-full max-h-full"
                    , src photoSrc
                    ]
                    []
                ]
            , p [ class "ml-3" ]
                [ span [ class "block text-2xl text-black" ] [ text name ]
                , span [ class "text-purple-500 text-body uppercase" ] [ text occupation ]
                ]
            ]
        , p [ class "text-gray-900 max-w-xl" ]
            [ text <| "“" ++ quote ++ "”" ]
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
    | KeyDown String


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

        KeyDown key ->
            if key == "Esc" || key == "Escape" then
                { model | showLanguageNav = False }
                    |> UR.init

            else
                model
                    |> UR.init



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

        KeyDown _ ->
            [ "KeyDown" ]
