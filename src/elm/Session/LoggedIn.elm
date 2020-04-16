module Session.LoggedIn exposing
    ( External(..)
    , ExternalMsg(..)
    , FeedbackStatus(..)
    , Model
    , Msg(..)
    , Page(..)
    , ProfileStatus(..)
    , addNotification
    , askedAuthentication
    , init
    , initLogin
    , isAccount
    , isActive
    , isAuth
    , jsAddressToMsg
    , mapExternal
    , maybePrivateKey
    , msgToString
    , profile
    , readAllNotifications
    , subscriptions
    , update
    , view
    , viewFooter
    )

import Api.Graphql
import Auth
import Avatar
import Browser.Dom as Dom
import Browser.Events
import Cambiatus.Object
import Cambiatus.Object.UnreadNotifications
import Cambiatus.Subscription as Subscription
import Eos exposing (Symbol)
import Eos.Account as Eos
import Flags exposing (Flags)
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootSubscription)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, button, div, footer, img, input, nav, p, span, text)
import Html.Attributes exposing (class, classList, placeholder, required, src, style, type_, value)
import Html.Events exposing (onClick, onFocus, onInput, onMouseEnter, onSubmit, stopPropagationOn)
import Http
import I18Next exposing (Delims(..), Translations, t)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Notification exposing (Notification)
import Ports
import Profile exposing (Profile, query)
import Route exposing (Route)
import Session.Shared as Shared exposing (Shared)
import Shop
import Task
import Translation
import UpdateResult as UR



-- INIT


init : Shared -> Eos.Name -> Flags -> ( Model, Cmd Msg )
init shared accountName flags =
    let
        authModel =
            Auth.init shared
    in
    ( initModel shared authModel accountName flags.selectedCommunity
    , Cmd.batch
        [ Api.Graphql.query shared (Profile.query accountName) CompletedLoadProfile
        ]
    )


fetchTranslations : String -> Shared -> Cmd Msg
fetchTranslations language _ =
    CompletedLoadTranslation language
        |> Translation.get language


initLogin : Shared -> Auth.Model -> Profile -> ( Model, Cmd Msg )
initLogin shared authModel profile_ =
    let
        model =
            initModel shared authModel profile_.account Eos.bespiralSymbol
    in
    ( { model
        | profile = Loaded profile_
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotAuthMsg (Auth.subscriptions model.auth)
        , Sub.map KeyDown (Browser.Events.onKeyDown (Decode.field "key" Decode.string))
        ]



-- MODEL


type alias Model =
    { shared : Shared
    , accountName : Eos.Name
    , profile : ProfileStatus
    , selectedCommunity : Symbol
    , showUserNav : Bool
    , showLanguageItems : Bool
    , searchText : String
    , showNotificationModal : Bool
    , showMainNav : Bool
    , notification : Notification.Model
    , unreadCount : Int
    , showAuthModal : Bool
    , auth : Auth.Model
    , feedback : FeedbackStatus
    , showCommunitySelector : Bool
    }


initModel : Shared -> Auth.Model -> Eos.Name -> Symbol -> Model
initModel shared authModel accountName selectedCommunity =
    { shared = shared
    , accountName = accountName
    , profile = Loading accountName
    , selectedCommunity = selectedCommunity
    , showUserNav = False
    , showLanguageItems = False
    , searchText = ""
    , showNotificationModal = False
    , showMainNav = False
    , notification = Notification.init
    , unreadCount = 0
    , showAuthModal = False
    , auth = authModel
    , feedback = Hidden
    , showCommunitySelector = False
    }


type alias Feedback =
    { message : String
    , success : Bool
    }


type FeedbackStatus
    = Show Feedback
    | Hidden


type ProfileStatus
    = Loading Eos.Name
    | LoadingFailed Eos.Name (Graphql.Http.Error (Maybe Profile))
    | Loaded Profile


isAuth : Model -> Bool
isAuth model =
    Auth.isAuth model.auth


maybePrivateKey : Model -> Maybe String
maybePrivateKey model =
    Auth.maybePrivateKey model.auth



-- VIEW


type Page
    = Other
    | Dashboard
    | Communities
    | News
    | Learn
    | Shop
    | FAQ
    | Profile


view : (Msg -> msg) -> Page -> Model -> Html msg -> Html msg
view thisMsg page ({ shared } as model) content =
    case ( Shared.translationStatus shared, model.profile ) of
        ( Shared.LoadingTranslation, _ ) ->
            Shared.viewFullLoading

        ( Shared.LoadingTranslationFailed err, _ ) ->
            Shared.viewFullError shared
                err
                ClickedTryAgainTranslation
                "An error ocurred while loading translation."
                |> Html.map thisMsg

        ( _, Loading _ ) ->
            Shared.viewFullLoading

        ( _, LoadingFailed accountName err ) ->
            Shared.viewFullGraphqlError shared
                err
                (ClickedTryAgainProfile accountName)
                "An error ocurred while loading profile."
                |> Html.map thisMsg

        ( _, Loaded profile_ ) ->
            viewHelper thisMsg page profile_ model content


viewFeedback : Feedback -> Html Msg
viewFeedback feedback =
    div
        [ class "sticky top-0 w-full flex justify-center items-center"
        , classList [ ( "bg-green", feedback.success ), ( "bg-red", not feedback.success ) ]
        ]
        [ span [ class "ml-auto invisible" ] []
        , span [ class "flex items-center text-sm h-10 leading-snug text-white font-bold" ]
            [ text feedback.message ]
        , span
            [ class "ml-auto mr-5 cursor-pointer"
            , onClick HideFeedback
            ]
            [ Icons.close "fill-current text-white"
            ]
        ]


viewHelper : (Msg -> msg) -> Page -> Profile -> Model -> Html msg -> Html msg
viewHelper thisMsg page profile_ ({ shared } as model) content =
    let
        onClickCloseAny =
            if model.showUserNav then
                onClick (ShowUserNav False)

            else if model.showNotificationModal then
                onClick (ShowNotificationModal False)

            else if model.showMainNav then
                onClick (ShowMainNav False)

            else if model.showAuthModal then
                onClick ClosedAuthModal

            else
                style "" ""
    in
    div
        [ class "min-h-screen flex flex-col" ]
        [ div [ class "bg-white" ]
            [ div [ class "container mx-auto" ]
                [ viewHeader model profile_ |> Html.map thisMsg
                , viewMainMenu page model |> Html.map thisMsg
                ]
            ]
        , case model.feedback of
            Show feedback ->
                viewFeedback feedback |> Html.map thisMsg

            Hidden ->
                text ""
        , div [ class "flex-grow" ]
            [ content
            ]
        , viewFooter shared
        , div [ onClickCloseAny ] [] |> Html.map thisMsg
        , if model.showAuthModal then
            div
                [ classList
                    [ ( "modal-old", True )
                    , ( "fade-in", True )
                    ]
                , onClickCloseAny
                ]
                [ div
                    [ class "card card--register card--modal"
                    , stopPropagationOn "click"
                        (Decode.succeed ( Ignored, True ))
                    ]
                    (Auth.view True shared model.auth
                        |> List.map (Html.map GotAuthMsg)
                    )
                ]
                |> Html.map thisMsg

          else
            text ""
        , communitySelectorModal model
            |> Html.map thisMsg
        ]


viewHeader : Model -> Profile -> Html Msg
viewHeader ({ shared } as model) profile_ =
    let
        text_ str =
            text (t shared.translations str)

        tr str values =
            I18Next.tr shared.translations I18Next.Curly str values
    in
    div [ class "flex flex-wrap items-center justify-between px-4 pt-6 pb-4" ]
        [ viewCommunitySelector model
        , div [ class "hidden lg:block lg:visible lg:w-1/3" ] [ searchBar model ]
        , div [ class "flex items-center float-right" ]
            [ a
                [ class "outline-none relative mx-6"
                , Route.href Route.Notification
                ]
                [ Icons.notification ""
                , if model.unreadCount > 0 then
                    div [ class "absolute top-0 right-0 -mr-4 px-2 py-1 bg-orange-500 text-white font-medium text-xs rounded-full" ]
                        [ text (String.fromInt model.unreadCount) ]

                  else
                    text ""
                ]
            , div [ class "relative z-10" ]
                [ button
                    [ class "h-12 z-10 bg-gray-200 py-2 px-3 relative hidden lg:visible lg:flex"
                    , classList [ ( "rounded-tr-lg rounded-tl-lg", model.showUserNav ) ]
                    , classList [ ( "rounded-lg", not model.showUserNav ) ]
                    , type_ "button"
                    , onClick (ShowUserNav (not model.showUserNav))
                    , onMouseEnter (ShowUserNav True)
                    ]
                    [ Avatar.view shared.endpoints.ipfs profile_.avatar "h-8 w-8"
                    , div [ class "flex flex-wrap text-left pl-2" ]
                        [ p [ class "w-full font-sans uppercase text-gray-900 text-xs overflow-x-hidden" ]
                            [ text (tr "menu.welcome_message" [ ( "user_name", Eos.nameToString profile_.account ) ]) ]
                        , p [ class "w-full font-sans text-indigo-500 text-sm" ]
                            [ text (t shared.translations "menu.my_account") ]
                        ]
                    , Icons.arrowDown "float-right"
                    ]
                , button
                    [ class "h-12 z-10 py-2 px-3 flex relative lg:hidden"
                    , classList [ ( "rounded-tr-lg rounded-tl-lg", model.showUserNav ) ]
                    , classList [ ( "rounded-lg", not model.showUserNav ) ]
                    , type_ "button"
                    , onClick (ShowUserNav (not model.showUserNav))
                    , onMouseEnter (ShowUserNav True)
                    ]
                    [ Avatar.view shared.endpoints.ipfs profile_.avatar "h-8 w-8"
                    ]

                -- Invisible button to hide menu when clicking outside
                , if model.showUserNav then
                    button
                        [ class "fixed h-full w-full inset-0 bg-black opacity-50 cursor-default"
                        , onClick (ShowUserNav False)
                        , onMouseEnter (ShowUserNav False)
                        ]
                        []

                  else
                    text ""
                , nav
                    [ class "absolute right-0 lg:w-full py-2 px-4 shadow-lg bg-white rounded-t-lg rounded-b-lg lg:rounded-t-none"
                    , classList
                        [ ( "hidden", not model.showUserNav )
                        ]
                    ]
                    [ a
                        [ class "flex block w-full px-4 py-4 justify-start items-center text-sm"
                        , Route.href Route.Profile
                        , onClick (ShowUserNav False)
                        ]
                        [ Icons.profile "mr-4"
                        , text_ "menu.profile"
                        ]
                    , button
                        [ class "flex block w-full px-4 py-4 justify-start items-center text-sm border-t"
                        , onClick ToggleLanguageItems
                        ]
                        [ Icons.languages "mr-4"
                        , text_ "menu.languages"
                        ]
                    , if model.showLanguageItems then
                        div [ class "ml-10 mb-2" ]
                            (button
                                [ class "flex block px-4 py-2 text-gray justify-between items-center text-indigo-500 font-bold text-xs"
                                ]
                                [ Shared.langFlag shared.language, text (String.toUpper shared.language) ]
                                :: Shared.viewLanguageItems shared ClickedLanguage
                            )

                      else
                        text ""
                    , button
                        [ class "flex block w-full px-4 py-4 justify-start items-center text-sm border-t"
                        , onClick ClickedLogout
                        ]
                        [ Icons.close "fill-current text-red mr-4"
                        , text_ "menu.logout"
                        ]
                    ]
                ]
            ]
        , div [ class "w-full mt-2 lg:hidden" ] [ searchBar model ]
        ]


viewCommunitySelector : Model -> Html Msg
viewCommunitySelector ({ shared } as model) =
    let
        findCommunity : Symbol -> Maybe Profile.CommunityInfo
        findCommunity symbol =
            case model.profile of
                Loaded p ->
                    p.communities
                        |> List.find (\c -> c.id == symbol)

                _ ->
                    Nothing

        url hash =
            shared.endpoints.ipfs ++ "/" ++ hash
    in
    case findCommunity model.selectedCommunity of
        Just community ->
            button [ class "flex items-center", onClick OpenCommunitySelector ]
                [ a [ Route.href Route.Dashboard ]
                    [ img [ class "h-10", src <| url community.logo ] [] ]
                , Icons.arrowDown ""
                ]

        Nothing ->
            a [ Route.href Route.Dashboard ]
                [ img [ class "lg:hidden h-8", src shared.logoMobile ] []
                , img
                    [ class "hidden lg:block lg:visible h-6"
                    , src shared.logo
                    ]
                    []
                ]


communitySelectorModal : Model -> Html Msg
communitySelectorModal model =
    let
        t s =
            I18Next.t model.shared.translations s

        text_ s =
            text (t s)

        logoUrl hash =
            model.shared.endpoints.ipfs ++ "/" ++ hash

        viewCommunityItem : Profile.CommunityInfo -> Html Msg
        viewCommunityItem c =
            div
                [ class "flex items-center h-16 border-b text-body hover:pointer"
                , onClick <| SelectCommunity c.id
                ]
                [ img [ src (logoUrl c.logo), class "h-12 mr-5" ] []
                , text c.name
                ]
    in
    if model.showCommunitySelector then
        case model.profile of
            Loaded pro ->
                if List.isEmpty pro.communities then
                    text ""

                else
                    div [ class "modal container" ]
                        [ div [ class "modal-bg", onClick CloseCommunitySelector ] []
                        , div [ class "modal-content overflow-auto", style "height" "65%" ]
                            [ div [ class "w-full" ]
                                [ p [ class "w-full font-bold text-heading text-2xl" ]
                                    [ text_ "menu.community_selector.title" ]
                                , button
                                    [ onClick CloseCommunitySelector ]
                                    [ Icons.close "absolute fill-current text-gray-400 top-0 right-0 mx-8 my-4"
                                    ]
                                , p [ class "text-body w-full font-sans mb-2" ]
                                    [ text_ "menu.community_selector.body"
                                    ]
                                , div [ class "w-full overflow-scroll" ]
                                    (List.map viewCommunityItem pro.communities)
                                ]
                            ]
                        ]

            _ ->
                text ""

    else
        text ""


searchBar : Model -> Html Msg
searchBar ({ shared } as model) =
    Html.form
        [ class "h-12 bg-gray-200 rounded-full flex items-center p-4"
        , onSubmit SubmitedSearch
        ]
        [ Icons.search ""
        , input
            [ class "bg-gray-200 w-full outline-none pl-3 text-sm"
            , placeholder (t shared.translations "menu.search")
            , type_ "text"
            , value model.searchText
            , onFocus FocusedSearchInput
            , onInput EnteredSearch
            , required True
            ]
            []
        ]


viewMainMenu : Page -> Model -> Html Msg
viewMainMenu page model =
    let
        menuItemClass =
            "mx-4 w-48 font-sans uppercase flex items-center justify-center leading-tight text-xs text-gray-700 hover:text-indigo-500"

        activeClass =
            "border-orange-100 border-b-2 text-indigo-500 font-medium"

        iconClass =
            "w-6 h-6 fill-current hover:text-indigo-500 mr-5"
    in
    nav [ class "h-16 w-full flex overflow-x-auto" ]
        [ a
            [ classList
                [ ( menuItemClass, True )
                , ( activeClass, isActive page Route.Dashboard )
                ]
            , Route.href Route.Dashboard
            ]
            [ Icons.dashboard iconClass
            , text (t model.shared.translations "menu.dashboard")
            ]
        , a
            [ classList
                [ ( menuItemClass, True )
                , ( activeClass, isActive page (Route.Shop Shop.All) )
                ]
            , Route.href (Route.Shop Shop.All)
            ]
            [ Icons.shop iconClass
            , text (t model.shared.translations "menu.shop")
            ]
        , a
            [ classList
                [ ( menuItemClass, True )
                , ( activeClass, isActive page Route.Communities )
                ]
            , Route.href Route.Communities
            ]
            [ Icons.communities iconClass
            , text (t model.shared.translations "menu.communities")
            ]
        ]


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Dashboard, Route.Dashboard ) ->
            True

        ( Communities, Route.Communities ) ->
            True

        ( Shop, Route.Shop _ ) ->
            True

        _ ->
            False


viewFooter : Shared -> Html msg
viewFooter _ =
    footer [ class "bg-white w-full flex flex-wrap mx-auto border-t border-grey p-4 pt-6 h-40 bottom-0" ]
        [ p [ class "text-sm flex w-full justify-center items-center" ]
            [ text "Created with"
            , Icons.heart
            , text "by Satisfied Vagabonds"
            ]
        , img
            [ class "h-24 w-full"
            , src "/images/satisfied-vagabonds.svg"
            ]
            []
        ]



-- UPDATE


type External msg
    = UpdatedLoggedIn Model
    | RequiredAuthentication (Maybe msg)
    | ShowFeedback Feedback


mapExternal : (msg -> msg2) -> External msg -> External msg2
mapExternal transform ext =
    case ext of
        UpdatedLoggedIn m ->
            UpdatedLoggedIn m

        RequiredAuthentication maybeM ->
            RequiredAuthentication (Maybe.map transform maybeM)

        ShowFeedback f ->
            ShowFeedback f


type alias UpdateResult =
    UR.UpdateResult Model Msg ExternalMsg


type ExternalMsg
    = AuthenticationSucceed
    | AuthenticationFailed


type Msg
    = Ignored
    | CompletedLoadTranslation String (Result Http.Error Translations)
    | ClickedTryAgainTranslation
    | CompletedLoadProfile (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))
    | ClickedTryAgainProfile Eos.Name
    | ClickedLogout
    | EnteredSearch String
    | SubmitedSearch
    | ShowNotificationModal Bool
    | ShowUserNav Bool
    | ShowMainNav Bool
    | FocusedSearchInput
    | ToggleLanguageItems
    | ClickedLanguage String
    | ClosedAuthModal
    | GotAuthMsg Auth.Msg
    | CompletedLoadUnread Value
    | KeyDown String
    | HideFeedback
    | OpenCommunitySelector
    | CloseCommunitySelector
    | SelectCommunity Symbol


update : Msg -> Model -> UpdateResult
update msg model =
    let
        shared =
            model.shared

        focusMainContent b alternative =
            if b then
                Dom.focus "main-content"
                    |> Task.attempt (\_ -> Ignored)

            else
                Dom.focus alternative
                    |> Task.attempt (\_ -> Ignored)

        closeAllModals =
            { model
                | showNotificationModal = False
                , showUserNav = False
                , showMainNav = False
                , showAuthModal = False
            }
    in
    case msg of
        Ignored ->
            UR.init model

        CompletedLoadTranslation lang (Ok transl) ->
            case model.profile of
                Loaded _ ->
                    UR.init { model | shared = Shared.loadTranslation (Ok ( lang, transl )) shared }
                        |> UR.addCmd (Ports.storeLanguage lang)

                _ ->
                    UR.init model

        CompletedLoadTranslation _ (Err err) ->
            UR.init { model | shared = Shared.loadTranslation (Err err) shared }
                |> UR.logHttpError msg err

        ClickedTryAgainTranslation ->
            UR.init { model | shared = Shared.toLoadingTranslation shared }
                |> UR.addCmd (fetchTranslations (Shared.language shared) shared)

        CompletedLoadProfile (Ok profile_) ->
            let
                subscriptionDoc =
                    unreadCountSubscription model.accountName
                        |> Graphql.Document.serializeSubscription
            in
            case profile_ of
                Just p ->
                    { model | profile = Loaded p }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = CompletedLoadUnread (Encode.string "")
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "subscribeToUnreadCount" )
                                    , ( "subscription", Encode.string subscriptionDoc )
                                    ]
                            }

                Nothing ->
                    UR.init model
                        |> UR.addCmd (Route.replaceUrl shared.navKey Route.Logout)

        CompletedLoadProfile (Err err) ->
            UR.init
                { model
                    | profile =
                        case model.profile of
                            Loading accountName ->
                                LoadingFailed accountName err

                            _ ->
                                model.profile
                }
                |> UR.logGraphqlError msg err

        ClickedTryAgainProfile accountName ->
            UR.init { model | profile = Loading accountName }
                |> UR.addCmd (Api.Graphql.query shared (Profile.query accountName) CompletedLoadProfile)

        ClickedLogout ->
            UR.init model
                |> UR.addCmd (Route.replaceUrl shared.navKey Route.Logout)
                |> UR.addPort
                    { responseAddress = ClickedLogout
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "logout" )
                            ]
                    }

        EnteredSearch s ->
            UR.init { model | searchText = s }

        SubmitedSearch ->
            UR.init model

        ShowNotificationModal b ->
            UR.init
                { closeAllModals
                    | showNotificationModal = b
                    , notification =
                        if b then
                            model.notification

                        else
                            Notification.readAll model.notification
                }
                |> UR.addCmd (focusMainContent (not b) "notifications-modal")

        ShowUserNav b ->
            UR.init { closeAllModals | showUserNav = b }
                |> UR.addCmd (focusMainContent (not b) "user-nav")

        ShowMainNav b ->
            UR.init { closeAllModals | showMainNav = b }
                |> UR.addCmd (focusMainContent (not b) "mobile-main-nav")

        FocusedSearchInput ->
            UR.init model
                |> UR.addCmd (Route.pushUrl shared.navKey Route.Communities)

        ToggleLanguageItems ->
            UR.init { model | showLanguageItems = not model.showLanguageItems }

        ClickedLanguage lang ->
            UR.init
                { model
                    | shared = Shared.toLoadingTranslation shared
                    , showUserNav = False
                }
                |> UR.addCmd (fetchTranslations lang shared)

        ClosedAuthModal ->
            UR.init closeAllModals

        GotAuthMsg authMsg ->
            Auth.update authMsg shared model.auth model.showAuthModal
                |> UR.map
                    (\a -> { model | auth = a })
                    GotAuthMsg
                    (\extMsg uResult ->
                        case extMsg of
                            Auth.ClickedCancel ->
                                closeModal uResult
                                    |> UR.addExt AuthenticationFailed

                            Auth.CompletedAuth _ ->
                                closeModal uResult
                                    |> UR.addExt AuthenticationSucceed

                            Auth.UpdatedShared newShared ->
                                UR.mapModel
                                    (\m -> { m | shared = newShared })
                                    uResult
                    )

        CompletedLoadUnread payload ->
            case Decode.decodeValue (unreadCountSubscription model.accountName |> Graphql.Document.decoder) payload of
                Ok res ->
                    { model | unreadCount = res.unreads }
                        |> UR.init

                Err _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        KeyDown key ->
            if key == "Esc" || key == "Escape" then
                UR.init { closeAllModals | showUserNav = False }

            else
                model
                    |> UR.init

        HideFeedback ->
            { model | feedback = Hidden }
                |> UR.init

        OpenCommunitySelector ->
            { model | showCommunitySelector = True }
                |> UR.init

        CloseCommunitySelector ->
            { model | showCommunitySelector = False }
                |> UR.init

        SelectCommunity communityId ->
            { model | selectedCommunity = communityId, showCommunitySelector = False }
                |> UR.init
                |> UR.addCmd (Route.replaceUrl model.shared.navKey Route.Dashboard)


closeModal : UpdateResult -> UpdateResult
closeModal ({ model } as uResult) =
    { uResult
        | model =
            { model
                | showNotificationModal = False
                , showUserNav = False
                , showMainNav = False
                , showAuthModal = False
            }
    }


askedAuthentication : Model -> Model
askedAuthentication model =
    { model
        | showNotificationModal = False
        , showUserNav = False
        , showMainNav = False
        , showAuthModal = True
    }



-- TRANSFORM


addNotification : Notification -> Model -> Model
addNotification notification model =
    { model
        | notification = Notification.addNotification notification model.notification
    }


readAllNotifications : Model -> Model
readAllNotifications model =
    { model | notification = Notification.readAll model.notification }



-- INFO


profile : Model -> Maybe Profile
profile model =
    case model.profile of
        Loaded profile_ ->
            Just profile_

        _ ->
            Nothing


isAccount : Eos.Name -> Model -> Bool
isAccount accountName model =
    Maybe.map .account (profile model) == Just accountName



-- UNREAD NOTIFICATIONS


type alias UnreadMeta =
    { unreads : Int }


unreadSelection : SelectionSet UnreadMeta Cambiatus.Object.UnreadNotifications
unreadSelection =
    SelectionSet.succeed UnreadMeta
        |> with Cambiatus.Object.UnreadNotifications.unreads


unreadCountSubscription : Eos.Name -> SelectionSet UnreadMeta RootSubscription
unreadCountSubscription name =
    let
        stringName =
            name
                |> Eos.nameToString

        args =
            { input = { account = stringName } }
    in
    Subscription.unreads args unreadSelection


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "GotAuthMsg" :: remainAddress ->
            Auth.jsAddressToMsg remainAddress val
                |> Maybe.map GotAuthMsg

        "CompletedLoadUnread" :: [] ->
            Decode.decodeValue (Decode.field "meta" Decode.value) val
                |> Result.map CompletedLoadUnread
                |> Result.toMaybe

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        CompletedLoadTranslation _ r ->
            [ "CompletedLoadTranslation", UR.resultToString r ]

        ClickedTryAgainTranslation ->
            [ "ClickedTryAgainTranslation" ]

        CompletedLoadProfile r ->
            [ "CompletedLoadProfile", UR.resultToString r ]

        ClickedTryAgainProfile _ ->
            [ "ClickedTryAgainProfile" ]

        ClickedLogout ->
            [ "ClickedLogout" ]

        EnteredSearch _ ->
            [ "EnteredSearch" ]

        SubmitedSearch ->
            [ "SubmitedSearch" ]

        ShowNotificationModal _ ->
            [ "ShowNotificationModal" ]

        ShowUserNav _ ->
            [ "ShowUserNav" ]

        ShowMainNav _ ->
            [ "ShowMainNav" ]

        FocusedSearchInput ->
            [ "FocusedSearchInput" ]

        ToggleLanguageItems ->
            [ "ToggleLanguageItems" ]

        ClickedLanguage _ ->
            [ "ClickedLanguage" ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        GotAuthMsg subMsg ->
            "GotAuthMsg" :: Auth.msgToString subMsg

        CompletedLoadUnread _ ->
            [ "CompletedLoadUnread" ]

        KeyDown _ ->
            [ "KeyDown" ]

        HideFeedback ->
            [ "HideFeedback" ]

        OpenCommunitySelector ->
            [ "OpenCommunitySelector" ]

        CloseCommunitySelector ->
            [ "CloseCommunitySelector" ]

        SelectCommunity _ ->
            [ "SelectCommunity" ]
