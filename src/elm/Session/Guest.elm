module Session.Guest exposing
    ( BroadcastMsg(..)
    , External(..)
    , Model
    , Msg(..)
    , Page(..)
    , addAfterLoginRedirect
    , init
    , invalidCommunityRedirectUrl
    , maybeInitWith
    , msgToString
    , subscriptions
    , update
    , view
    )

import Api.Graphql
import Browser.Navigation
import Community
import Dict
import Environment
import Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, button, div, header, img, text)
import Html.Attributes exposing (class, classList, src, style, tabindex, type_)
import Html.Events exposing (onClick, onMouseEnter)
import Http
import I18Next exposing (Translations)
import Icons
import Json.Encode as Encode
import Log
import Ports
import Profile exposing (Model)
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session.Shared as Shared exposing (Shared)
import Task
import Translation
import UpdateResult as UR
import Url
import Utils
import Version exposing (Version)
import View.Feedback as Feedback



-- INIT


init : Shared -> ( Model, Cmd Msg )
init shared =
    ( initModel shared, fetchCommunity shared )


fetchCommunity : Shared -> Cmd Msg
fetchCommunity shared =
    if shared.useSubdomain then
        Api.Graphql.query shared
            Nothing
            (Community.communityPreviewQuery (Environment.communityDomain shared.url))
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


fetchTranslations : Version -> Translation.Language -> Cmd Msg
fetchTranslations version language =
    CompletedLoadTranslation language
        |> Translation.get version language



-- MODEL


type alias Model =
    { shared : Shared
    , showLanguageNav : Bool
    , afterLoginRedirect : Maybe Route
    , maybeInvitation : Maybe String
    , community : RemoteData (Graphql.Http.Error (Maybe Community.CommunityPreview)) Community.CommunityPreview
    , feedback : Feedback.Model
    }


initModel : Shared -> Model
initModel shared =
    { shared = shared
    , showLanguageNav = False
    , afterLoginRedirect = Nothing
    , maybeInvitation = Nothing
    , community = RemoteData.Loading
    , feedback = Feedback.Hidden
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Utils.escSubscription PressedEsc



-- VIEW


{-| Page types the Guest can access.
-}
type Page
    = Redirect
    | NotFound
    | ComingSoon
    | Register
    | Login
    | Invite
    | Join
    | ShopViewer


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

                ShopViewer ->
                    ( Nothing, "md:w-full" )

                Redirect ->
                    ( Nothing, "md:w-full" )

                NotFound ->
                    ( Nothing, "md:w-full" )

                ComingSoon ->
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
        [ class "flex items-center justify-between pl-4 py-4 md:pl-6 bg-white" ]
        [ a [ Route.href (Route.Login model.maybeInvitation model.afterLoginRedirect) ]
            [ if String.isEmpty logo then
                text ""

              else
                img [ class "h-10", src logo ] []
            ]
        , div [ class "relative z-50" ]
            [ button
                [ type_ "button"
                , tabindex -1
                , class "flex block relative z-20 items-center px-4 py-2 bg-white text-sm focus:outline-none"
                , classList
                    [ ( "rounded-tr-lg rounded-tl-lg justify-between lang-menu-open"
                      , model.showLanguageNav
                      )
                    ]
                , onClick (ShowLanguageNav (not model.showLanguageNav))
                , onMouseEnter (ShowLanguageNav True)
                ]
                [ Shared.langFlag [ class "w-6 h-6 mr-2" ] shared.language
                , if model.showLanguageNav then
                    div [ class "flex-grow whitespace-nowrap uppercase" ]
                        [ text (Translation.languageToLanguageCode model.shared.language) ]

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
                (Shared.viewLanguageItems
                    { containerAttrs = [ class "flex w-full px-4 py-2 text-gray items-center text-sm uppercase focus-ring rounded-sm hover:text-indigo-500 focus-visible:text-indigo-500" ]
                    , flagIconAttrs = [ class "w-6 h-6 mr-2" ]
                    }
                    shared
                    ClickedLanguage
                )
            ]
        ]



-- UPDATE


type External
    = LoggedIn
        { pin : String
        , privateKey : Eos.PrivateKey
        , signInResponse : Api.Graphql.SignInResponse
        }
    | SetFeedback Feedback.Model
    | UpdatedShared Shared


type BroadcastMsg
    = CommunityLoaded Community.CommunityPreview


type alias UpdateResult =
    UR.UpdateResult Model Msg BroadcastMsg


type Msg
    = CompletedLoadTranslation Translation.Language (Result Http.Error Translations)
    | ClickedTryAgainTranslation
    | ShowLanguageNav Bool
    | ClickedLanguage Translation.Language
    | PressedEsc
    | CompletedLoadCommunityPreview (RemoteData (Graphql.Http.Error (Maybe Community.CommunityPreview)) (Maybe Community.CommunityPreview))
    | GotFeedbackMsg Feedback.Msg


update : Msg -> Model -> UpdateResult
update msg ({ shared } as model) =
    let
        currentUrl =
            shared.url
    in
    case msg of
        CompletedLoadTranslation lang (Ok transl) ->
            { model | shared = Shared.loadTranslation (Ok ( lang, transl )) shared }
                |> UR.init
                |> UR.addCmd (Ports.storeLanguage (Translation.languageToLocale lang))

        CompletedLoadTranslation _ (Err err) ->
            { model | shared = Shared.loadTranslation (Err err) shared }
                |> UR.init
                |> UR.logHttpError msg
                    Nothing
                    "Got an error when loading translation as a guest"
                    { moduleName = "Session.Guest", function = "update" }
                    []
                    err

        ClickedTryAgainTranslation ->
            { model | shared = Shared.toLoadingTranslation shared }
                |> UR.init
                |> UR.addCmd (fetchTranslations shared.version shared.language)
                |> UR.addBreadcrumb
                    { type_ = Log.ErrorBreadcrumb
                    , category = msg
                    , message = "Clicked to fetch translation again"
                    , data = Dict.empty
                    , level = Log.Warning
                    }

        ShowLanguageNav b ->
            UR.init { model | showLanguageNav = b }

        ClickedLanguage lang ->
            { model
                | shared = Shared.toLoadingTranslation shared
                , showLanguageNav = False
            }
                |> UR.init
                |> UR.addCmd (fetchTranslations shared.version lang)

        PressedEsc ->
            { model | showLanguageNav = False }
                |> UR.init

        CompletedLoadCommunityPreview (RemoteData.Success (Just communityPreview)) ->
            { model | community = RemoteData.Success communityPreview }
                |> UR.init
                |> UR.addExt (CommunityLoaded communityPreview)
                |> UR.addBreadcrumb
                    { type_ = Log.DefaultBreadcrumb
                    , category = msg
                    , message = "Community preview loaded successfully"
                    , data =
                        Dict.fromList
                            [ ( "symbol", Eos.encodeSymbol communityPreview.symbol )
                            , ( "name", Encode.string communityPreview.name )
                            ]
                    , level = Log.Info
                    }
                |> UR.addPort
                    { responseAddress = msg
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "setFavicon" )
                            , ( "favicon", Encode.string communityPreview.logo )
                            ]
                    }

        CompletedLoadCommunityPreview (RemoteData.Success Nothing) ->
            UR.init model
                |> UR.addCmd
                    (case invalidCommunityRedirectUrl currentUrl shared.useSubdomain of
                        Nothing ->
                            Cmd.none

                        Just redirectUrl ->
                            Browser.Navigation.load redirectUrl
                    )

        CompletedLoadCommunityPreview (RemoteData.Failure err) ->
            UR.init { model | community = RemoteData.Failure err }
                |> UR.logGraphqlError msg
                    Nothing
                    "Got an error when loading community preview as a guest"
                    { moduleName = "Session.Guest", function = "update" }
                    []
                    err
                |> UR.addCmd
                    (case invalidCommunityRedirectUrl currentUrl shared.useSubdomain of
                        Nothing ->
                            Cmd.none

                        Just redirectUrl ->
                            Browser.Navigation.load redirectUrl
                    )

        CompletedLoadCommunityPreview RemoteData.NotAsked ->
            UR.init { model | community = RemoteData.NotAsked }

        CompletedLoadCommunityPreview RemoteData.Loading ->
            UR.init { model | community = RemoteData.Loading }

        GotFeedbackMsg subMsg ->
            { model | feedback = Feedback.update subMsg model.feedback }
                |> UR.init


invalidCommunityRedirectUrl : Url.Url -> Bool -> Maybe String
invalidCommunityRedirectUrl currentUrl useSubdomain =
    if useSubdomain then
        if String.contains "localhost:" (Url.toString currentUrl) then
            { currentUrl | host = "cambiatus.staging.localhost" }
                |> Url.toString
                |> Just

        else if String.endsWith "staging.cambiatus.io" currentUrl.host then
            { currentUrl | host = "cambiatus.staging.cambiatus.io" }
                |> Url.toString
                |> Just

        else if String.endsWith "demo.cambiatus.io" currentUrl.host then
            Just "https://www.cambiatus.com/welcome-demo"

        else
            Just "https://www.cambiatus.com/welcome"

    else
        Nothing



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

        PressedEsc ->
            [ "PressedEsc" ]

        CompletedLoadCommunityPreview r ->
            [ "CompletedLoadCommunityPreview", UR.remoteDataToString r ]

        GotFeedbackMsg _ ->
            [ "GotFeedbackMsg" ]
