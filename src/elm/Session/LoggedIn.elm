module Session.LoggedIn exposing (External(..), ExternalMsg(..), Model, Msg(..), Page(..), ProfileStatus(..), addNotification, askedAuthentication, init, initLogin, isAccount, isActive, isAuth, jsAddressToMsg, mapExternal, maybePrivateKey, msgToString, profile, readAllNotifications, subscriptions, update, view)

import Account exposing (Profile, profileQuery)
import Api
import Api.Chat as Chat exposing (ChatPreferences)
import Api.Graphql
import Asset.Icon as Icon
import Auth
import Avatar
import Bespiral.Object
import Bespiral.Object.UnreadNotifications
import Bespiral.Query
import Bespiral.Subscription as Subscription
import Browser.Dom as Dom
import Browser.Events
import Community exposing (Balance)
import Eos
import Eos.Account as Eos
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery, RootSubscription)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onFocus, onInput, onMouseEnter, onMouseOver, onSubmit, stopPropagationOn)
import Http
import I18Next exposing (Delims(..), Translations, t, tr)
import Icons
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode exposing (Value)
import Log
import Notification exposing (Notification)
import Ports
import Route exposing (Route)
import Session.Shared as Shared exposing (Shared)
import Shop
import Task exposing (Task)
import Time
import Translation
import UpdateResult as UR



-- INIT


init : Shared -> Eos.Name -> ( Model, Cmd Msg )
init shared accountName =
    let
        authModel =
            Auth.init shared
    in
    ( initModel shared authModel accountName
    , Cmd.batch
        [ Api.Graphql.query shared (profileQuery accountName) CompletedLoadProfile
        , Api.getBalances shared accountName CompletedLoadBalances
        ]
    )


fetchTranslations : String -> Shared -> Cmd Msg
fetchTranslations language shared =
    CompletedLoadTranslation language
        |> Translation.get language


initLogin : Shared -> Auth.Model -> Profile -> ( Model, Cmd Msg )
initLogin shared authModel profile_ =
    let
        model =
            initModel shared authModel profile_.accountName
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
    , isAuthenticated : Bool
    , accountName : Eos.Name
    , profile : ProfileStatus
    , showUserNav : Bool
    , showLanguageItems : Bool
    , searchText : String
    , showNotificationModal : Bool
    , showMainNav : Bool
    , showToggleMainNav : Bool
    , notification : Notification.Model
    , unreadCount : Int
    , showAuthModal : Bool
    , auth : Auth.Model
    , balances : List Balance
    }


initModel : Shared -> Auth.Model -> Eos.Name -> Model
initModel shared authModel accountName =
    { shared = shared
    , isAuthenticated = False
    , accountName = accountName
    , profile = Loading accountName
    , showUserNav = False
    , showLanguageItems = False
    , searchText = ""
    , showNotificationModal = False
    , showMainNav = False
    , showToggleMainNav = True
    , notification = Notification.init
    , unreadCount = 0
    , showAuthModal = False
    , auth = authModel
    , balances = []
    }


type ProfileStatus
    = Loading Eos.Name
    | LoadingFailed Eos.Name (Graphql.Http.Error (Maybe Profile))
    | Loaded Profile


type Authentication
    = WithPrivateKey
    | WithScatter


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


onScroll : msg -> Attribute msg
onScroll message =
    on "scroll" (Decode.succeed message)


viewHelper : (Msg -> msg) -> Page -> Profile -> Model -> Html msg -> Html msg
viewHelper thisMsg page profile_ ({ shared } as model) content =
    let
        ipfsUrl =
            shared.endpoints.ipfs

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
        [ viewHeader model page profile_ |> Html.map thisMsg
        , viewMainMenu page profile_ model |> Html.map thisMsg
        , div [ class "flex-grow" ] [ content ]
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
        ]


viewHeader : Model -> Page -> Profile -> Html Msg
viewHeader ({ shared } as model) page profile_ =
    let
        text_ str =
            text (t shared.translations str)

        tr str values =
            I18Next.tr shared.translations I18Next.Curly str values
    in
    div [ class "flex flex-wrap items-center justify-between bg-white px-4 pt-6 pb-4" ]
        [ a [ Route.href Route.Dashboard ]
            [ img [ class "lg:hidden h-8", src shared.logoMobile ] []
            , img
                [ class "hidden lg:block lg:visible h-6"
                , src shared.logo
                ]
                []
            ]
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
                            [ text (tr "menu.welcome_message" [ ( "user_name", Eos.nameToString profile_.accountName ) ]) ]
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
                            ([ button
                                [ class "flex block px-4 py-2 text-gray justify-between items-center text-indigo-500 font-bold text-xs"
                                ]
                                [ Shared.langFlag shared.language, text (String.toUpper shared.language) ]
                             ]
                                ++ Shared.viewLanguageItems shared ClickedLanguage
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


viewMainMenu : Page -> Profile -> Model -> Html Msg
viewMainMenu page profile_ model =
    let
        ipfsUrl =
            model.shared.endpoints.ipfs

        menuItemClass =
            "mx-4 w-auto md:w-48 font-sans uppercase flex md:items-center md:justify-center leading-tight text-xs text-gray-700 hover:text-indigo-500 sm:justify-start sm:pl-3 "

        activeClass =
            "border-orange-100 border-b-2 text-indigo-500 font-medium"

        iconClass =
            "w-6 h-6 fill-current hover:text-indigo-500 mr-5"
    in
    div [ class "flex align-center justify-center" ]
        [ nav
            [ classList [ ( "invisible", not model.showMainNav ) ]
            , class "bg-white w-64 h-56 md:h-12 md:w-1/2 md:h-16 md:w-full flex flex-col justify-around md:justify-start md:flex-row md:overflow-x-auto md:visible rounded-lg md:rounded-none fixed bottom-0 mb-24 md:static md:mb-0 shadow-md"
            ]
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
                    , ( activeClass, isActive page Route.Communities )
                    ]
                , Route.href Route.Communities
                ]
                [ Icons.communities iconClass
                , text (t model.shared.translations "menu.communities")
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
            ]
        , toggleNav model model.showMainNav
        , button
            [ class "fixed h-full w-full inset-0 bg-black opacity-0 cursor-default"
            , onMouseEnter MobileMenuPresent
            ]
            []
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
viewFooter shared =
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



-- Button to show/hide navbar on small screens


toggleNav : Model -> Bool -> Html Msg
toggleNav model clicked =
    {- if hide flag in model is false animate menu onto screen
       if the hide flag is true animate menu out of screen
    -}
    let
        animation =
            if model.showToggleMainNav == True then
                "animated fadeInRight"

            else
                "invisible"

        buttonStyles =
            "flex align-center justify-center md:invisible fixed right-0 bottom-0 mb-4 mr-4 h-16 w-16 bg-purple-500 text-white rounded-full shadow p-4 align-bottom text-left "

        content =
            if clicked then
                button [ onClick (ShowMainNav False) ] [ Icons.close "fill-current text-white" ]

            else
                button [ onClick (ShowMainNav True) ] [ text "Menu" ]
    in
    div [ class (String.append buttonStyles animation) ]
        [ content ]



-- UPDATE


type External msg
    = UpdatedLoggedIn Model
    | RequiredAuthentication (Maybe msg)
    | UpdateBalances


mapExternal : (msg -> msg2) -> External msg -> External msg2
mapExternal transform ext =
    case ext of
        UpdatedLoggedIn m ->
            UpdatedLoggedIn m

        RequiredAuthentication maybeM ->
            RequiredAuthentication (Maybe.map transform maybeM)

        UpdateBalances ->
            UpdateBalances


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
    | HideToggleNav Bool
    | FocusedSearchInput
    | ToggleLanguageItems
    | ClickedLanguage String
    | CompletedChatTranslation (Result (Graphql.Http.Error (Maybe ChatPreferences)) (Maybe ChatPreferences))
    | ClosedAuthModal
    | GotAuthMsg Auth.Msg
    | ReceivedNotification String
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | CompletedLoadUnread Value
    | KeyDown String
    | MobileMenuPresent
    | ToggleMobileMenu


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
                Loaded profile_ ->
                    UR.init { model | shared = Shared.loadTranslation (Ok ( lang, transl )) shared }
                        |> UR.addCmd (Chat.updateChatLanguage shared profile_ lang CompletedChatTranslation)
                        |> UR.addCmd (Ports.storeLanguage lang)

                _ ->
                    UR.init model

        CompletedLoadTranslation lang (Err err) ->
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
                    UR.init { model | profile = Loaded p }
                        |> UR.addCmd (Chat.updateChatLanguage shared p shared.language CompletedChatTranslation)
                        |> UR.addPort
                            { responseAddress = CompletedLoadProfile (Ok profile_)
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "chatCredentials" )
                                    , ( "container", Encode.string "chat-manager" )
                                    , ( "credentials", Account.encodeProfileChat p )
                                    , ( "notificationAddress"
                                      , Encode.list Encode.string [ "GotPageMsg", "GotLoggedInMsg", "ReceivedNotification" ]
                                      )
                                    ]
                            }
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
                |> UR.addCmd (Api.Graphql.query shared (profileQuery accountName) CompletedLoadProfile)

        ClickedLogout ->
            UR.init model
                |> UR.addCmd (Route.replaceUrl shared.navKey Route.Logout)
                |> UR.addPort
                    { responseAddress = ClickedLogout
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "logout" )
                            , ( "container", Encode.string "chat-manager" )
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
                |> UR.addCmd (focusMainContent b "mobile-main-nav")

        HideToggleNav b ->
            UR.init { closeAllModals | showToggleMainNav = b }

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

        CompletedChatTranslation _ ->
            UR.init model

        ClosedAuthModal ->
            UR.init closeAllModals

        GotAuthMsg authMsg ->
            Auth.update authMsg shared model.auth
                |> UR.map
                    (\a -> { model | auth = a })
                    GotAuthMsg
                    (\extMsg uResult ->
                        case extMsg of
                            Auth.ClickedCancel ->
                                closeModal uResult
                                    |> UR.addExt AuthenticationFailed

                            Auth.CompletedAuth profile_ ->
                                closeModal uResult
                                    |> UR.mapModel
                                        (\m ->
                                            { m | isAuthenticated = True }
                                        )
                                    |> UR.addExt AuthenticationSucceed

                            Auth.UpdatedShared newShared ->
                                UR.mapModel
                                    (\m -> { m | shared = newShared })
                                    uResult
                    )

        ReceivedNotification from ->
            addNotification
                (chatNotification model from)
                model
                |> UR.init

        CompletedLoadBalances res ->
            case res of
                Ok bals ->
                    { model | balances = bals }
                        |> UR.init

                Err err ->
                    model
                        |> UR.init

        CompletedLoadUnread payload ->
            case Decode.decodeValue (unreadCountSubscription model.accountName |> Graphql.Document.decoder) payload of
                Ok res ->
                    { model | unreadCount = res.unreads }
                        |> UR.init

                Err e ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        KeyDown key ->
            if key == "Esc" || key == "Escape" then
                UR.init { closeAllModals | showUserNav = False }

            else
                model
                    |> UR.init

        MobileMenuPresent ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = ToggleMobileMenu
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "scrollListener" ) ]
                    }

        ToggleMobileMenu ->
            {- { model | mobileMenuShow = !model.mobileMenuShown} -}
            { model | showToggleMainNav = not model.showToggleMainNav }
                |> UR.init



{- Add logic to show and hide mobile mebu
   Consider adding a flag to the model to hold the menu visibilty
   Use that flag when rendering the menu to animate it out of screen
   Or to animate it onto the screen
-}


chatNotification : Model -> String -> Notification
chatNotification model from =
    { title = "menu.chat_message_notification"
    , description = from
    , class = "chat-notification"
    , unread = True
    , link = Just (model.shared.endpoints.chat ++ "/direct/" ++ from)
    }


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
    Maybe.map .accountName (profile model) == Just accountName



-- UNREAD NOTIFICATIONS


type alias UnreadMeta =
    { unreads : Int }


unreadSelection : SelectionSet UnreadMeta Bespiral.Object.UnreadNotifications
unreadSelection =
    SelectionSet.succeed UnreadMeta
        |> with Bespiral.Object.UnreadNotifications.unreads


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

        "ReceivedNotification" :: [] ->
            Decode.decodeValue
                (Decode.field "username" Decode.string)
                val
                |> Result.map ReceivedNotification
                |> Result.toMaybe

        "CompletedLoadUnread" :: [] ->
            Decode.decodeValue (Decode.field "meta" Decode.value) val
                |> Result.map CompletedLoadUnread
                |> Result.toMaybe

        "ToggleMobileMenu" :: [] ->
            Just ToggleMobileMenu

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

        HideToggleNav _ ->
            [ "HideToggleNav" ]

        FocusedSearchInput ->
            [ "FocusedSearchInput" ]

        ToggleLanguageItems ->
            [ "ToggleLanguageItems" ]

        ClickedLanguage _ ->
            [ "ClickedLanguage" ]

        CompletedChatTranslation _ ->
            [ "CompletedChatTranslation" ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        GotAuthMsg subMsg ->
            "GotAuthMsg" :: Auth.msgToString subMsg

        ReceivedNotification _ ->
            [ "ReceivedNotification" ]

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]

        CompletedLoadUnread _ ->
            [ "CompletedLoadUnread" ]

        KeyDown _ ->
            [ "KeyDown" ]

        MobileMenuPresent ->
            [ "MobileMenuPresent" ]

        ToggleMobileMenu ->
            [ "ToggleMobileMenu" ]
