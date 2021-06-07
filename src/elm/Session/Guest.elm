module Session.Guest exposing
    ( BroadcastMsg(..)
    , External(..)
    , Model
    , Msg(..)
    , Page(..)
    , addAfterLoginRedirect
    , init
    , initLoggingIn
    , maybeInitWith
    , msgToString
    , subscriptions
    , update
    , view
    )

import Api.Graphql
import Auth
import Browser.Events
import Browser.Navigation
import Community
import Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, button, div, header, img, text)
import Html.Attributes exposing (class, classList, src, style, tabindex, type_)
import Html.Events exposing (onClick, onMouseEnter)
import Http
import I18Next exposing (Delims(..), Translations)
import Icons
import Json.Decode as Decode
import Ports
import Profile exposing (Model)
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session.Shared as Shared exposing (Shared)
import Task
import Translation
import UpdateResult as UR
import Url
import View.Feedback as Feedback



-- INIT


init : Shared -> ( Model, Cmd Msg )
init shared =
    ( initModel shared, fetchCommunity shared )


initLoggingIn : Shared -> Eos.Name -> (RemoteData (Graphql.Http.Error (Maybe Auth.SignInResponse)) (Maybe Auth.SignInResponse) -> msg) -> ( Model, Cmd Msg, Cmd msg )
initLoggingIn shared accountName signInMessage =
    let
        ( model, cmd ) =
            init shared
    in
    ( { model | isLoggingIn = True }
    , cmd
    , Api.Graphql.mutation shared
        Nothing
        (Auth.signIn accountName shared Nothing)
        signInMessage
    )


fetchCommunity : Shared -> Cmd Msg
fetchCommunity shared =
    if shared.useSubdomain then
        Api.Graphql.query shared
            Nothing
            (Community.communityPreviewQuery (Shared.communityDomain shared))
            CompletedLoadCommunityPreview

    else
        let
            symbol =
                shared.selectedCommunity
                    |> Maybe.withDefault Eos.cambiatusSymbol
        in
        Api.Graphql.query shared
            Nothing
            (Community.communityPreviewSymbolQuery symbol)
            CompletedLoadCommunityPreview


fetchTranslations : String -> Cmd Msg
fetchTranslations language =
    CompletedLoadTranslation language
        |> Translation.get language



-- MODEL


type alias Model =
    { shared : Shared
    , showLanguageNav : Bool
    , afterLoginRedirect : Maybe Route
    , community : RemoteData (Graphql.Http.Error (Maybe Community.CommunityPreview)) Community.CommunityPreview
    , isLoggingIn : Bool
    , feedback : Feedback.Model
    }


initModel : Shared -> Model
initModel shared =
    { shared = shared
    , showLanguageNav = False
    , afterLoginRedirect = Nothing
    , community = RemoteData.Loading
    , isLoggingIn = False
    , feedback = Feedback.Hidden
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
    | Invite
    | Join
    | Other


view : (Msg -> msg) -> Page -> Model -> Html msg -> Html msg
view thisMsg page ({ shared } as model) content =
    let
        ( maybeLeftArt, rightColWidth ) =
            -- Some guest pages have the half-width block with the picture and the user quotes
            case page of
                Login ->
                    ( Just loginLeftCol, "md:w-3/5" )

                Register ->
                    ( Just loginLeftCol, "md:w-3/5" )

                Invite ->
                    ( Nothing, "md:w-full" )

                Join ->
                    model.community
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (\community ->
                                ( Just (Community.communityPreviewImage True model.shared community), "md:w-1/2" )
                            )
                        |> Maybe.withDefault ( Nothing, "md:w-full" )

                Other ->
                    ( Nothing, "md:w-full" )
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
                [ case maybeLeftArt of
                    Just leftArt ->
                        leftArt

                    Nothing ->
                        text ""
                , div
                    [ class "min-h-screen flex flex-col"
                    , class rightColWidth
                    ]
                    [ viewPageHeader model shared
                        |> Html.map thisMsg
                    , Feedback.view model.feedback
                        |> Html.map (GotFeedbackMsg >> thisMsg)
                    , content
                    ]
                ]


loginLeftCol : Html msg
loginLeftCol =
    div
        -- Left part with background and quote (desktop only)
        [ class "hidden md:block md:visible md:w-2/5 h-screen sticky top-0 bg-bottom bg-no-repeat"
        , style "background-color" "#EFF9FB"
        , style "background-size" "auto 80%"
        , style "background-image" "url(/images/auth_bg_full.png)"
        ]
        []


viewPageHeader : Model -> Shared -> Html Msg
viewPageHeader model shared =
    let
        imageElement url =
            img
                [ class "h-6"
                , src url
                ]
                []

        loadingSpinner =
            div [ class "full-spinner-container h-full" ]
                [ div [ class "spinner spinner--delay" ] [] ]

        logo =
            case model.community of
                RemoteData.Loading ->
                    ""

                RemoteData.NotAsked ->
                    ""

                RemoteData.Success comm ->
                    comm.logo

                RemoteData.Failure _ ->
                    shared.logo
    in
    header
        [ class "flex items-center justify-between pl-4 md:pl-6 py-3 bg-white" ]
        [ a [ Route.href (Route.Login Nothing) ]
            [ case model.community of
                RemoteData.Loading ->
                    loadingSpinner

                _ ->
                    imageElement logo
            ]
        , div [ class "relative z-50" ]
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
                    div [ class "flex-grow whitespace-nowrap" ]
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
    = LoggedIn Eos.PrivateKey Auth.SignInResponse
    | SetFeedback Feedback.Model


type BroadcastMsg
    = CommunityLoaded Community.CommunityPreview


type alias UpdateResult =
    UR.UpdateResult Model Msg BroadcastMsg


type Msg
    = CompletedLoadTranslation String (Result Http.Error Translations)
    | ClickedTryAgainTranslation
    | ShowLanguageNav Bool
    | ClickedLanguage String
    | KeyDown String
    | CompletedLoadCommunityPreview (RemoteData (Graphql.Http.Error (Maybe Community.CommunityPreview)) (Maybe Community.CommunityPreview))
    | GotFeedbackMsg Feedback.Msg


update : Msg -> Model -> UpdateResult
update msg ({ shared } as model) =
    let
        currentUrl =
            shared.url

        invalidCommunityRedirectUrl =
            if String.contains "localhost:" (Url.toString currentUrl) && shared.useSubdomain then
                { currentUrl | host = "cambiatus.staging.localhost" }
                    |> Url.toString

            else
                "https://cambiatus.com"
    in
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

        CompletedLoadCommunityPreview (RemoteData.Success (Just communityPreview)) ->
            { model | community = RemoteData.Success communityPreview }
                |> UR.init
                |> UR.addExt (CommunityLoaded communityPreview)

        CompletedLoadCommunityPreview (RemoteData.Success Nothing) ->
            UR.init model
                |> UR.addCmd
                    (if shared.useSubdomain then
                        Browser.Navigation.load invalidCommunityRedirectUrl

                     else
                        Cmd.none
                    )

        CompletedLoadCommunityPreview (RemoteData.Failure err) ->
            UR.init { model | community = RemoteData.Failure err }
                |> UR.logGraphqlError msg err
                |> UR.addCmd
                    (if shared.useSubdomain then
                        Browser.Navigation.load invalidCommunityRedirectUrl

                     else
                        Cmd.none
                    )

        CompletedLoadCommunityPreview RemoteData.NotAsked ->
            UR.init { model | community = RemoteData.NotAsked }

        CompletedLoadCommunityPreview RemoteData.Loading ->
            UR.init { model | community = RemoteData.Loading }

        GotFeedbackMsg subMsg ->
            { model | feedback = Feedback.update subMsg model.feedback }
                |> UR.init



-- TRANSFORM


maybeInitWith : (a -> msg) -> (Model -> RemoteData e a) -> Model -> Cmd msg
maybeInitWith toMsg attribute model =
    case attribute model of
        RemoteData.Success value ->
            Task.succeed value
                |> Task.perform toMsg

        _ ->
            Cmd.none


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

        CompletedLoadCommunityPreview r ->
            [ "CompletedLoadCommunityPreview", UR.remoteDataToString r ]

        GotFeedbackMsg _ ->
            [ "GotFeedbackMsg" ]
