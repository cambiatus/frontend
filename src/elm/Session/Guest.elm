module Session.Guest exposing (External(..), Model, Msg(..), Page(..), addAfterLoginRedirect, init, initModel, msgToString, subscriptions, update, view)

import Browser.Events
import Html exposing (Html, button, div, header, img, text)
import Html.Attributes exposing (alt, class, classList, src, style, tabindex, type_)
import Html.Events exposing (onClick, onMouseEnter)
import Http
import I18Next exposing (Delims(..), Translations)
import Icons
import Json.Decode as Decode
import Ports
import Profile exposing (Profile)
import Route exposing (Route)
import Session.Shared as Shared exposing (Shared)
import Translation
import UpdateResult as UR



-- INIT


init : Shared -> ( Model, Cmd Msg )
init shared =
    ( initModel shared
    , Cmd.none
    )


fetchTranslations : String -> Cmd Msg
fetchTranslations language =
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
subscriptions _ =
    Sub.map KeyDown (Browser.Events.onKeyDown (Decode.field "key" Decode.string))



-- VIEW


{-| Page types the Guest can access.
-}
type Page
    = Register
    | Login
    | Shop
    | PaymentHistory
    | Other


view : (Msg -> msg) -> Page -> Model -> Html msg -> Html msg
view thisMsg page ({ shared } as model) content =
    let
        isLeftArtAvailable =
            -- Some guest pages have the half-width block with the picture and the user quotes
            case page of
                Login ->
                    True

                Register ->
                    True

                _ ->
                    False

        leftColWidth =
            if isLeftArtAvailable then
                "md:w-2/5"

            else
                ""

        rightColWidth =
            if isLeftArtAvailable then
                "md:w-3/5"

            else
                "md:w-full"
    in
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
            div
                [ class "md:flex" ]
                [ if isLeftArtAvailable then
                    viewLeftCol leftColWidth

                  else
                    text ""
                , div
                    [ class "min-h-stretch flex flex-col"
                    , class rightColWidth
                    ]
                    [ viewPageHeader model shared
                        |> Html.map thisMsg
                    , content
                    ]
                ]


viewLeftCol : String -> Html msg
viewLeftCol mdWidth =
    div
        -- Left part with background and quote (desktop only)
        [ class "hidden md:block md:visible min-h-screen bg-bottom bg-no-repeat"
        , class mdWidth
        , style "background-color" "#EFF9FB"
        , style "background-size" "auto 80%"
        , style "background-image" "url(images/auth_bg_full.png)"
        ]
        []


viewPageHeader : Model -> Shared -> Html Msg
viewPageHeader model shared =
    header
        [ class "flex items-center justify-between pl-4 md:pl-6 py-3 bg-white" ]
        [ div []
            [ img
                [ class "h-5"
                , src shared.logo
                , alt "Cambiatus"
                ]
                []
            ]
        , div [ class "relative z-10" ]
            [ button
                [ type_ "button"
                , tabindex -1
                , class "flex block relative z-20 items-center px-4 py-2 bg-white text-xs focus:outline-none"
                , classList
                    [ ( "rounded-tr-lg rounded-tl-lg justify-between lang-menu-open"
                      , model.showLanguageNav
                      )
                    ]
                , onClick (ShowLanguageNav (not model.showLanguageNav))
                , onMouseEnter (ShowLanguageNav True)
                ]
                [ Shared.langFlag shared.language
                , if model.showLanguageNav then
                    div [ class "flex-grow whitespace-no-wrap" ]
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
                [ class "absolute right-0 w-full py-2 bg-white border-t rounded-br-lg rounded-bl-lg shadow-lg"
                , class "lang-menu-open"
                , classList
                    [ ( "hidden", not model.showLanguageNav )
                    ]
                ]
                (Shared.viewLanguageItems shared ClickedLanguage)
            ]
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

        CompletedLoadTranslation _ (Err err) ->
            { model | shared = Shared.loadTranslation (Err err) shared }
                |> UR.init
                |> UR.logHttpError msg err

        ClickedTryAgainTranslation ->
            { model | shared = Shared.toLoadingTranslation shared }
                |> UR.init
                |> UR.addCmd (fetchTranslations (Shared.language shared))

        ShowLanguageNav b ->
            UR.init { model | showLanguageNav = b }

        ClickedLanguage lang ->
            { model
                | shared = Shared.toLoadingTranslation shared
                , showLanguageNav = False
            }
                |> UR.init
                |> UR.addCmd (fetchTranslations lang)

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
