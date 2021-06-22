module Session.LoggedIn exposing
    ( BroadcastMsg(..)
    , External(..)
    , ExternalMsg(..)
    , Model
    , Msg(..)
    , Page(..)
    , Resource(..)
    , init
    , initLogin
    , isAccount
    , jsAddressToMsg
    , maybeInitWith
    , maybePrivateKey
    , msgToString
    , profile
    , subscriptions
    , update
    , updateExternal
    , view
    , withAuthentication
    )

import Action
import Api
import Api.Graphql
import Auth
import Avatar
import Browser.Dom as Dom
import Browser.Events
import Cambiatus.Object
import Cambiatus.Object.UnreadNotifications
import Cambiatus.Subscription as Subscription
import Community
import Eos
import Eos.Account as Eos
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootSubscription)
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, footer, img, nav, p, text)
import Html.Attributes exposing (class, classList, src, type_)
import Html.Events exposing (onClick, onMouseEnter)
import Http
import I18Next exposing (Delims(..), Translations)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Maybe.Extra
import Notification exposing (Notification)
import Ports
import Profile
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Search exposing (State(..))
import Session.Shared as Shared exposing (Shared)
import Shop
import Task
import Time
import Translation
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback
import View.Modal as Modal



-- INIT


{-| Initialize already logged in user when the page is [re]loaded.
-}
init : Shared -> Eos.Name -> String -> ( Model, Cmd Msg )
init shared accountName authToken =
    ( initModel shared Nothing accountName authToken
    , Cmd.batch
        [ Api.Graphql.query shared (Just authToken) (Profile.query accountName) CompletedLoadProfile
        , fetchCommunity shared authToken Nothing
        , Ports.getRecentSearches () -- run on the page refresh, duplicated in `initLogin`
        , Task.perform GotTimeInternal Time.now
        ]
    )


fetchCommunity : Shared -> String -> Maybe Eos.Symbol -> Cmd Msg
fetchCommunity shared authToken maybeToken =
    if shared.useSubdomain then
        Api.Graphql.query shared
            (Just authToken)
            (Community.subdomainQuery (Shared.communityDomain shared))
            CompletedLoadCommunity

    else
        let
            symbol =
                Maybe.Extra.or maybeToken shared.selectedCommunity
                    |> Maybe.withDefault Eos.cambiatusSymbol
        in
        Api.Graphql.query shared (Just authToken) (Community.symbolQuery symbol) CompletedLoadCommunity


fetchTranslations : String -> Shared -> Cmd Msg
fetchTranslations language _ =
    CompletedLoadTranslation language
        |> Translation.get language


{-| Initialize logged in user after signing-in.
-}
initLogin : Shared -> Maybe Eos.PrivateKey -> Profile.Model -> String -> ( Model, Cmd Msg )
initLogin shared maybePrivateKey_ profile_ authToken =
    let
        loadedProfile =
            Just profile_
                |> RemoteData.Success
                |> Task.succeed
                |> Task.perform CompletedLoadProfile
    in
    ( initModel shared maybePrivateKey_ profile_.account authToken
    , Cmd.batch
        [ Ports.getRecentSearches () -- run on the passphrase login, duplicated in `init`
        , loadedProfile
        , fetchCommunity shared authToken Nothing
        , Task.perform GotTimeInternal Time.now
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyDown (Browser.Events.onKeyDown (Decode.field "key" Decode.string))
        , Sub.map GotSearchMsg Search.subscriptions
        , Sub.map GotActionMsg (Action.subscriptions model.claimingAction)
        ]



-- MODEL


type alias Model =
    { shared : Shared
    , routeHistory : List Route
    , accountName : Eos.Name
    , profile : RemoteData (Graphql.Http.Error (Maybe Profile.Model)) Profile.Model
    , selectedCommunity : RemoteData (Graphql.Http.Error (Maybe Community.Model)) Community.Model
    , showUserNav : Bool
    , showLanguageItems : Bool
    , showNotificationModal : Bool
    , showMainNav : Bool
    , notification : Notification.Model
    , unreadCount : Int
    , showAuthModal : Bool
    , auth : Auth.Model
    , showCommunitySelector : Bool
    , feedback : Feedback.Model
    , searchModel : Search.Model
    , claimingAction : Action.Model
    , authToken : String
    }


initModel : Shared -> Maybe Eos.PrivateKey -> Eos.Name -> String -> Model
initModel shared maybePrivateKey_ accountName authToken =
    { shared = shared
    , routeHistory = []
    , accountName = accountName
    , profile = RemoteData.Loading
    , selectedCommunity = RemoteData.Loading
    , showUserNav = False
    , showLanguageItems = False
    , showNotificationModal = False
    , showMainNav = False
    , notification = Notification.init
    , unreadCount = 0
    , showAuthModal = False
    , auth = Auth.init maybePrivateKey_
    , feedback = Feedback.Hidden
    , showCommunitySelector = False
    , searchModel = Search.init
    , claimingAction = { status = Action.NotAsked, feedback = Nothing, needsPinConfirmation = False }
    , authToken = authToken
    }


hasPrivateKey : Model -> Bool
hasPrivateKey model =
    Auth.hasPrivateKey model.auth


maybePrivateKey : Model -> Maybe Eos.PrivateKey
maybePrivateKey model =
    Auth.maybePrivateKey model.auth



-- VIEW


type Page
    = Other
    | Invite
    | Join
    | Dashboard
    | Community
    | CommunitySettings
    | CommunitySettingsFeatures
    | CommunitySettingsInfo
    | CommunitySettingsCurrency
    | CommunityEditor
    | CommunitySelector
    | Objectives
    | ObjectiveEditor
    | ActionEditor
    | Claim
    | Notification
    | Shop
    | ShopEditor
    | ShopViewer
    | Profile
    | ProfilePublic
    | ProfileEditor
    | ProfileAddKyc
    | ProfileClaims
    | ProfileAddContact
    | PaymentHistory
    | Transfer
    | ViewTransfer
    | Analysis


view : (Msg -> msg) -> Page -> Model -> Html msg -> Html msg
view thisMsg page ({ shared } as model) content =
    case ( Shared.translationStatus shared, model.profile ) of
        ( Shared.LoadingTranslation, _ ) ->
            Shared.viewFullLoading

        ( Shared.LoadingTranslationFailed err, _ ) ->
            Shared.viewFullError shared
                err
                ClickedTryAgainTranslation
                "An error occurred while loading translation."
                |> Html.map thisMsg

        ( _, RemoteData.Loading ) ->
            View.Components.loadingLogoAnimated shared.translators ""

        ( _, RemoteData.NotAsked ) ->
            View.Components.loadingLogoAnimated shared.translators ""

        ( _, RemoteData.Failure err ) ->
            Shared.viewFullGraphqlError shared
                err
                (ClickedTryAgainProfile model.accountName)
                "An error occurred while loading profile."
                |> Html.map thisMsg

        ( _, RemoteData.Success profile_ ) ->
            viewHelper thisMsg page profile_ model content


hideCommunityAndSearch : Page -> Model -> Bool
hideCommunityAndSearch currentPage model =
    let
        hiddenPages =
            [ CommunitySelector ]
    in
    List.member currentPage hiddenPages || not (isCommunityMember model)


viewHelper : (Msg -> pageMsg) -> Page -> Profile.Model -> Model -> Html pageMsg -> Html pageMsg
viewHelper pageMsg page profile_ ({ shared } as model) content =
    let
        viewClaimWithProofs action proof isLoading =
            [ Action.viewClaimWithProofs proof shared.translators isLoading action
                |> Html.map (GotActionMsg >> pageMsg)
            ]

        mainView =
            case ( Search.isActive model.searchModel, model.claimingAction.status ) of
                ( True, _ ) ->
                    case model.selectedCommunity of
                        RemoteData.Success community ->
                            [ Search.viewSearchBody
                                shared.translators
                                community.symbol
                                shared.now
                                (GotSearchMsg >> pageMsg)
                                (GotActionMsg >> pageMsg)
                                model.searchModel
                            ]

                        _ ->
                            []

                ( False, Action.PhotoUploaderShowed action p ) ->
                    viewClaimWithProofs action p False

                ( False, Action.ClaimInProgress action (Just p) ) ->
                    viewClaimWithProofs action p True

                _ ->
                    viewPageBody model profile_ page content
    in
    div
        [ class "min-h-screen flex flex-col" ]
        (div [ class "bg-white" ]
            [ div [ class "container mx-auto" ]
                [ viewHeader page model profile_
                    |> Html.map pageMsg
                , if hideCommunityAndSearch page model || Search.isActive model.searchModel then
                    text ""

                  else
                    viewMainMenu page model |> Html.map pageMsg
                ]
            ]
            :: (Feedback.view model.feedback |> Html.map (GotFeedbackMsg >> pageMsg))
            :: mainView
            ++ [ viewFooter shared
               , Action.viewClaimConfirmation shared.translators model.claimingAction
                    |> Html.map (GotActionMsg >> pageMsg)
               , Modal.initWith
                    { closeMsg = ClosedAuthModal
                    , isVisible = model.showAuthModal
                    }
                    |> Modal.withBody
                        (Auth.view shared model.auth
                            |> List.map (Html.map GotAuthMsg)
                        )
                    |> Modal.toHtml
                    |> Html.map pageMsg
               , communitySelectorModal model
                    |> Html.map pageMsg
               ]
        )


viewPageBody : Model -> Profile.Model -> Page -> Html pageMsg -> List (Html pageMsg)
viewPageBody ({ shared } as model) profile_ page content =
    let
        { t } =
            shared.translators

        hasUserKycFilled =
            case profile_.kyc of
                Just _ ->
                    True

                Nothing ->
                    False

        availableWithoutKyc : List Page
        availableWithoutKyc =
            [ Other
            , Invite
            , CommunitySelector
            , Profile
            , Notification
            , ProfilePublic
            , ProfileEditor
            , ProfileAddKyc
            , PaymentHistory
            , ViewTransfer
            ]

        viewKycRestriction =
            div [ class "mx-auto container max-w-sm" ]
                [ div [ class "my-6 mx-4 text-center" ]
                    [ p [ class "text-2xl font-bold" ]
                        [ text (t "community.kyc.restriction.title") ]
                    , p [ class "mt-2 mb-6" ]
                        [ text (t "community.kyc.restriction.description") ]
                    , a
                        [ class "button button-primary m-auto w-full sm:w-56"
                        , Route.href Route.ProfileAddKyc
                        ]
                        [ text (t "community.kyc.restriction.link") ]
                    ]
                , img
                    [ class "w-full mx-auto md:w-64 mt-6 mb-8"
                    , src "/images/not_found.svg"
                    ]
                    []
                ]
    in
    [ div [ class "flex-grow flex flex-col" ]
        [ case model.selectedCommunity of
            RemoteData.Loading ->
                div [ class "full-spinner-container h-full" ]
                    [ div [ class "spinner spinner--delay mt-8" ] [] ]

            RemoteData.NotAsked ->
                div [ class "full-spinner-container h-full" ]
                    [ div [ class "spinner spinner--delay mt-8" ] [] ]

            RemoteData.Success { hasKyc } ->
                let
                    isContentAllowed =
                        List.member page availableWithoutKyc
                            || not hasKyc
                            || (hasKyc && hasUserKycFilled)
                in
                if isContentAllowed then
                    content

                else
                    viewKycRestriction

            RemoteData.Failure _ ->
                let
                    isContentAllowed =
                        List.member page availableWithoutKyc
                in
                if isContentAllowed then
                    content

                else
                    text ""
        ]
    ]


viewHeader : Page -> Model -> Profile.Model -> Html Msg
viewHeader page ({ shared } as model) profile_ =
    let
        text_ str =
            text (shared.translators.t str)

        tr str values =
            shared.translators.tr str values

        hideCommunitySelectorPages =
            [ CommunitySelector ]

        hideCommunitySelector =
            List.member page hideCommunitySelectorPages
    in
    div [ class "flex flex-wrap items-center justify-between px-4 pt-6 pb-4" ]
        [ if hideCommunitySelector then
            div []
                [ img [ class "hidden sm:block h-5", src shared.logo ] []
                , img [ class "sm:hidden h-5", src shared.logoMobile ] []
                ]

          else
            viewCommunitySelector model
        , if hideCommunityAndSearch page model then
            div [] []

          else
            div [ class "order-last w-full md:order-none mt-2 md:ml-2 md:flex-grow md:w-auto" ]
                [ Search.viewForm shared.translators model.searchModel
                    |> Html.map GotSearchMsg
                ]
        , div [ class "flex items-center float-right" ]
            [ a
                [ class "outline-none relative mx-6"
                , Route.href Route.Notification
                ]
                [ Icons.notification "fill-current text-black"
                , if model.unreadCount > 0 then
                    div [ class "absolute top-0 right-0 -mr-4 px-2 py-1 bg-orange-500 text-white font-medium text-xs rounded-full" ]
                        [ text (String.fromInt model.unreadCount) ]

                  else
                    text ""
                ]
            , div [ class "relative z-50" ]
                [ button
                    [ class "h-12 z-10 bg-gray-200 py-2 px-3 relative hidden lg:visible lg:flex"
                    , classList [ ( "rounded-tr-lg rounded-tl-lg", model.showUserNav ) ]
                    , classList [ ( "rounded-lg", not model.showUserNav ) ]
                    , type_ "button"
                    , onClick (ShowUserNav (not model.showUserNav))
                    , onMouseEnter (ShowUserNav True)
                    ]
                    [ Avatar.view profile_.avatar "h-8 w-8"
                    , div [ class "flex flex-wrap text-left pl-2" ]
                        [ p [ class "w-full font-sans uppercase text-gray-900 text-xs overflow-x-hidden" ]
                            [ text (tr "menu.welcome_message" [ ( "user_name", Eos.nameToString profile_.account ) ]) ]
                        , p [ class "w-full font-sans text-indigo-500 text-sm" ]
                            [ text (shared.translators.t "menu.my_account") ]
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
                    [ Avatar.view profile_.avatar "h-8 w-8"
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
                    [ class "absolute right-0 lg:w-full py-2 px-4 shadow-lg bg-white rounded-t-lg rounded-b-lg lg:rounded-t-none z-50"
                    , classList
                        [ ( "hidden", not model.showUserNav )
                        ]
                    ]
                    [ a
                        [ class "flex block w-full px-4 py-4 justify-start items-center text-sm"
                        , Route.href Route.Profile
                        , onClick (ShowUserNav False)
                        , onClick SearchClosed
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
                                [ class "flex px-4 py-2 text-gray items-center text-indigo-500 font-bold text-xs"
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
        ]


viewCommunitySelector : Model -> Html Msg
viewCommunitySelector model =
    let
        hasMultipleCommunities : Bool
        hasMultipleCommunities =
            case model.profile of
                RemoteData.Success p ->
                    List.length p.communities > 1 || not (isCommunityMember model)

                _ ->
                    False
    in
    case model.selectedCommunity of
        RemoteData.Success community ->
            button [ class "flex items-center", onClick OpenCommunitySelector ]
                [ img [ class "h-10", src community.logo ] []
                , if hasMultipleCommunities then
                    Icons.arrowDown ""

                  else
                    text ""
                ]

        _ ->
            text ""


communitySelectorModal : Model -> Html Msg
communitySelectorModal model =
    let
        t s =
            model.shared.translators.t s

        text_ s =
            text (t s)

        viewCommunityItem : Profile.CommunityInfo -> Html Msg
        viewCommunityItem c =
            div
                [ class "flex items-center p-4 text-body cursor-pointer hover:text-black hover:bg-gray-100"
                , onClick <| SelectedCommunity c
                ]
                [ img [ src c.logo, class "h-16 w-16 mr-5 object-scale-down" ] []
                , text c.name
                ]
    in
    if model.showCommunitySelector then
        case model.profile of
            RemoteData.Success pro ->
                if List.isEmpty pro.communities then
                    text ""

                else
                    Modal.initWith
                        { closeMsg = CloseCommunitySelector
                        , isVisible = True
                        }
                        |> Modal.withHeader (t "menu.community_selector.title")
                        |> Modal.withBody
                            [ p []
                                [ text_ "menu.community_selector.body"
                                ]
                            , div [ class "w-full overflow-y-auto divide-y divide-gray-300" ]
                                (List.map viewCommunityItem pro.communities)
                            ]
                        |> Modal.toHtml

            _ ->
                text ""

    else
        text ""


viewMainMenu : Page -> Model -> Html Msg
viewMainMenu page model =
    let
        menuItemClass =
            "mx-4 w-48 font-sans uppercase flex items-center justify-center leading-tight text-xs text-gray-700 hover:text-indigo-500"

        activeClass =
            "border-orange-100 border-b-2 text-indigo-500 font-medium"

        iconClass =
            "w-6 h-6 fill-current hover:text-indigo-500 mr-5"

        closeClaimWithPhoto =
            GotActionMsg Action.ClaimConfirmationClosed
    in
    nav [ class "h-16 w-full flex overflow-x-auto" ]
        [ a
            [ classList
                [ ( menuItemClass, True )
                , ( activeClass, isActive page Route.Dashboard )
                ]
            , Route.href Route.Dashboard
            , onClick closeClaimWithPhoto
            ]
            [ Icons.dashboard iconClass
            , text (model.shared.translators.t "menu.dashboard")
            ]
        , case model.selectedCommunity of
            RemoteData.Success { hasShop } ->
                if hasShop then
                    a
                        [ classList
                            [ ( menuItemClass, True )
                            , ( activeClass, isActive page (Route.Shop Shop.All) )
                            ]
                        , Route.href (Route.Shop Shop.All)
                        , onClick closeClaimWithPhoto
                        ]
                        [ Icons.shop iconClass
                        , text (model.shared.translators.t "menu.shop")
                        ]

                else
                    text ""

            _ ->
                text ""
        ]


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Dashboard, Route.Dashboard ) ->
            True

        ( Shop, Route.Shop _ ) ->
            True

        _ ->
            False


viewFooter : Shared -> Html msg
viewFooter _ =
    footer [ class "bg-white w-full flex flex-wrap mx-auto border-t border-grey-500 p-4 pt-6 h-40 bottom-0" ]
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


{-| Messages that pages can fire and LoggedIn will react to
-}
type External msg
    = UpdatedLoggedIn Model
    | AddedCommunity Profile.CommunityInfo
    | CreatedCommunity Eos.Symbol String
    | ExternalBroadcast BroadcastMsg
    | ReloadResource Resource
    | RequiredAuthentication { successMsg : msg, errorMsg : msg }
    | ShowFeedback Feedback.Status String
    | HideFeedback


type Resource
    = CommunityResource
    | ProfileResource
    | TimeResource


updateExternal :
    External msg
    -> Model
    ->
        { model : Model
        , cmd : Cmd Msg
        , externalCmd : Cmd msg
        , broadcastMsg : Maybe BroadcastMsg
        , afterAuthMsg : Maybe { successMsg : msg, errorMsg : msg }
        }
updateExternal externalMsg ({ shared } as model) =
    let
        defaultResult =
            { model = model
            , cmd = Cmd.none
            , externalCmd = Cmd.none
            , broadcastMsg = Nothing
            , afterAuthMsg = Nothing
            }
    in
    case externalMsg of
        UpdatedLoggedIn newModel ->
            { defaultResult | model = newModel }

        AddedCommunity communityInfo ->
            let
                ( newModel, cmd ) =
                    signUpForCommunity model communityInfo

                profileWithCommunity =
                    case profile newModel of
                        Nothing ->
                            newModel.profile

                        Just profile_ ->
                            RemoteData.Success
                                { profile_ | communities = communityInfo :: profile_.communities }
            in
            { defaultResult
                | model = { newModel | profile = profileWithCommunity }
                , cmd = cmd
            }

        CreatedCommunity symbol subdomain ->
            let
                ( newModel, cmd ) =
                    selectCommunity model { symbol = symbol, subdomain = subdomain } Route.Dashboard
            in
            { defaultResult | model = newModel, cmd = cmd }

        ExternalBroadcast broadcastMsg ->
            case broadcastMsg of
                CommunityLoaded community ->
                    let
                        ( newModel, cmd ) =
                            setCommunity community model
                    in
                    { defaultResult | model = newModel, cmd = cmd, broadcastMsg = Just broadcastMsg }

                ProfileLoaded profile_ ->
                    { defaultResult
                        | model = { model | profile = RemoteData.Success profile_ }
                        , broadcastMsg = Just broadcastMsg
                    }

                GotTime time ->
                    { defaultResult
                        | model = { model | shared = { shared | now = time } }
                        , broadcastMsg = Just broadcastMsg
                    }

        ReloadResource CommunityResource ->
            let
                ( _, cmd ) =
                    model.selectedCommunity
                        |> RemoteData.map .symbol
                        |> RemoteData.toMaybe
                        |> loadCommunity model
            in
            { defaultResult | cmd = cmd }

        ReloadResource ProfileResource ->
            { defaultResult
                | cmd =
                    Api.Graphql.query model.shared
                        (Just model.authToken)
                        (Profile.query model.accountName)
                        CompletedLoadProfile
            }

        ReloadResource TimeResource ->
            { defaultResult | cmd = Task.perform GotTimeInternal Time.now }

        RequiredAuthentication afterAuthMsg ->
            { defaultResult
                | model = askedAuthentication model
                , afterAuthMsg = Just afterAuthMsg
            }

        ShowFeedback status message ->
            { defaultResult | model = { model | feedback = Feedback.Visible status message } }

        HideFeedback ->
            { defaultResult | model = { model | feedback = Feedback.Hidden } }


type alias UpdateResult =
    UR.UpdateResult Model Msg ExternalMsg


{-| Messages that LoggedIn can fire, and pages/Main will react to
-}
type ExternalMsg
    = AuthenticationSucceed
    | AuthenticationFailed
    | Broadcast BroadcastMsg


type BroadcastMsg
    = CommunityLoaded Community.Model
    | ProfileLoaded Profile.Model
    | GotTime Time.Posix


type Msg
    = NoOp
    | CompletedLoadTranslation String (Result Http.Error Translations)
    | ClickedTryAgainTranslation
    | CompletedLoadProfile (RemoteData (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))
    | CompletedLoadCommunity (RemoteData (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | ClickedTryAgainProfile Eos.Name
    | ClickedLogout
    | ShowUserNav Bool
    | ToggleLanguageItems
    | ClickedLanguage String
    | ClosedAuthModal
    | GotAuthMsg Auth.Msg
    | CompletedLoadUnread Value
    | KeyDown String
    | OpenCommunitySelector
    | CloseCommunitySelector
    | SelectedCommunity Profile.CommunityInfo
    | GotFeedbackMsg Feedback.Msg
    | GotSearchMsg Search.Msg
    | GotActionMsg Action.Msg
    | SearchClosed
    | GotTimeInternal Time.Posix


update : Msg -> Model -> UpdateResult
update msg model =
    let
        shared =
            model.shared

        focusMainContent b alternative =
            if b then
                Dom.focus "main-content"
                    |> Task.attempt (\_ -> NoOp)

            else
                Dom.focus alternative
                    |> Task.attempt (\_ -> NoOp)

        closeAllModals =
            { model
                | showNotificationModal = False
                , showUserNav = False
                , showMainNav = False
                , showAuthModal = False
            }
    in
    case msg of
        NoOp ->
            UR.init model

        GotTimeInternal time ->
            UR.init { model | shared = { shared | now = time } }
                |> UR.addExt (GotTime time |> Broadcast)

        GotActionMsg actionMsg ->
            handleActionMsg model actionMsg

        SearchClosed ->
            { model | searchModel = Search.closeSearch model.searchModel }
                |> UR.init

        GotSearchMsg searchMsg ->
            case model.selectedCommunity of
                RemoteData.Success community ->
                    let
                        ( searchModel, searchCmd ) =
                            Search.update shared model.authToken community.symbol model.searchModel searchMsg
                    in
                    { model | searchModel = searchModel }
                        |> UR.init
                        |> UR.addCmd (Cmd.map GotSearchMsg searchCmd)

                _ ->
                    UR.init model

        CompletedLoadTranslation lang (Ok transl) ->
            case model.profile of
                RemoteData.Success _ ->
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

        CompletedLoadProfile (RemoteData.Success profile_) ->
            let
                subscriptionDoc =
                    unreadCountSubscription model.accountName
                        |> Graphql.Document.serializeSubscription
            in
            case profile_ of
                Just p ->
                    { model | profile = RemoteData.Success p }
                        |> UR.init
                        |> UR.addExt (ProfileLoaded p |> Broadcast)
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

        CompletedLoadProfile (RemoteData.Failure err) ->
            UR.init
                { model
                    | profile =
                        case model.profile of
                            RemoteData.Loading ->
                                RemoteData.Failure err

                            _ ->
                                model.profile
                }
                |> UR.logGraphqlError msg err

        CompletedLoadProfile RemoteData.NotAsked ->
            UR.init model

        CompletedLoadProfile RemoteData.Loading ->
            UR.init model

        CompletedLoadCommunity (RemoteData.Success (Just community)) ->
            let
                ( newModel, cmd ) =
                    setCommunity community model
            in
            UR.init newModel
                |> UR.addCmd cmd
                |> UR.addExt (CommunityLoaded community |> Broadcast)

        CompletedLoadCommunity (RemoteData.Success Nothing) ->
            UR.init model
                |> UR.addCmd (Route.pushUrl shared.navKey (Route.CommunitySelector (List.head model.routeHistory)))

        CompletedLoadCommunity (RemoteData.Failure e) ->
            let
                communityExists =
                    not (Community.isNonExistingCommunityError e)
            in
            UR.init { model | selectedCommunity = RemoteData.Failure e }
                |> UR.logGraphqlError msg e
                |> (if communityExists then
                        identity

                    else
                        UR.addCmd (Route.pushUrl shared.navKey (Route.CommunitySelector (List.head model.routeHistory)))
                   )

        CompletedLoadCommunity RemoteData.NotAsked ->
            UR.init { model | selectedCommunity = RemoteData.NotAsked }

        CompletedLoadCommunity RemoteData.Loading ->
            UR.init { model | selectedCommunity = RemoteData.Loading }

        ClickedTryAgainProfile accountName ->
            UR.init { model | profile = RemoteData.Loading }
                |> UR.addCmd
                    (Api.Graphql.query shared
                        (Just model.authToken)
                        (Profile.query accountName)
                        CompletedLoadProfile
                    )

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

        ShowUserNav b ->
            UR.init { closeAllModals | showUserNav = b }
                |> UR.addCmd (focusMainContent (not b) "user-nav")

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
                |> UR.addExt AuthenticationFailed

        GotAuthMsg authMsg ->
            Auth.update authMsg shared model.auth
                |> UR.map
                    (\a -> { model | auth = a })
                    GotAuthMsg
                    (\extMsg uResult ->
                        case extMsg of
                            Auth.CompletedAuth { user, token } auth ->
                                let
                                    cmd =
                                        case model.claimingAction.status of
                                            Action.ClaimInProgress action maybeProof ->
                                                -- If action claim is in progress,
                                                -- send a message to finish the claiming process
                                                -- when the user confirms the PIN.
                                                Task.succeed (GotActionMsg (Action.ActionClaimed action maybeProof))
                                                    |> Task.perform identity

                                            _ ->
                                                Cmd.none
                                in
                                closeModal uResult
                                    |> UR.mapModel
                                        (\m ->
                                            { m
                                                | profile = RemoteData.Success user
                                                , authToken = token
                                                , auth = auth
                                            }
                                        )
                                    |> UR.addExt AuthenticationSucceed
                                    |> UR.addCmd cmd
                    )

        CompletedLoadUnread payload ->
            case Decode.decodeValue (unreadCountSubscription model.accountName |> Graphql.Document.decoder) payload of
                Ok res ->
                    { model | unreadCount = res }
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

        GotFeedbackMsg subMsg ->
            { model | feedback = Feedback.update subMsg model.feedback }
                |> UR.init

        OpenCommunitySelector ->
            { model | showCommunitySelector = True }
                |> UR.init

        CloseCommunitySelector ->
            { model | showCommunitySelector = False }
                |> UR.init

        SelectedCommunity ({ symbol } as newCommunity) ->
            let
                ( loadCommunityModel, loadCommunityCmd ) =
                    loadCommunity model (Just symbol)
            in
            case model.selectedCommunity of
                RemoteData.Success selectedCommunity ->
                    if symbol == selectedCommunity.symbol then
                        UR.init { model | showCommunitySelector = False }

                    else
                        let
                            ( newModel, cmd ) =
                                selectCommunity model
                                    newCommunity
                                    (List.head model.routeHistory
                                        |> Maybe.withDefault Route.Dashboard
                                    )
                        in
                        UR.init { newModel | showCommunitySelector = False }
                            |> UR.addCmd cmd

                RemoteData.NotAsked ->
                    UR.init loadCommunityModel
                        |> UR.addCmd loadCommunityCmd

                RemoteData.Failure _ ->
                    UR.init loadCommunityModel
                        |> UR.addCmd loadCommunityCmd

                RemoteData.Loading ->
                    UR.init model


handleActionMsg : Model -> Action.Msg -> UpdateResult
handleActionMsg ({ shared } as model) actionMsg =
    case model.selectedCommunity of
        RemoteData.Success community ->
            let
                actionModelToLoggedIn : Action.Model -> Model
                actionModelToLoggedIn a =
                    { model
                        | claimingAction = a
                        , feedback =
                            case ( a.feedback, actionMsg ) of
                                ( _, Action.Tick _ ) ->
                                    -- Don't change feedback each second
                                    model.feedback

                                ( Just (Action.Failure s), _ ) ->
                                    Feedback.Visible Feedback.Failure s

                                ( Just (Action.Success s), _ ) ->
                                    Feedback.Visible Feedback.Success s

                                ( Nothing, _ ) ->
                                    model.feedback
                    }
                        |> (if a.needsPinConfirmation then
                                askedAuthentication

                            else
                                identity
                           )
            in
            Action.update (hasPrivateKey model)
                shared
                (Api.uploadImage shared)
                community.symbol
                model.accountName
                actionMsg
                model.claimingAction
                |> UR.map
                    actionModelToLoggedIn
                    GotActionMsg
                    (\extMsg uR -> UR.addExt extMsg uR)
                |> UR.addCmd
                    (case actionMsg of
                        Action.AgreedToClaimWithProof _ ->
                            Task.perform identity (Task.succeed SearchClosed)

                        _ ->
                            Cmd.none
                    )

        _ ->
            UR.init model


{-| Checks if we already have the user's private key loaded. If it does, returns
`successfulUR`. If it doesn't, requires authentication and fires the `subMsg`
again
-}
withAuthentication :
    Model
    -> subModel
    -> { successMsg : subMsg, errorMsg : subMsg }
    -> UR.UpdateResult subModel subMsg (External subMsg)
    -> UR.UpdateResult subModel subMsg (External subMsg)
withAuthentication loggedIn subModel subMsg successfulUR =
    if hasPrivateKey loggedIn then
        successfulUR

    else
        UR.init subModel
            |> UR.addExt (RequiredAuthentication subMsg)


isCommunityMember : Model -> Bool
isCommunityMember model =
    case ( profile model, model.selectedCommunity ) of
        ( Just profile_, RemoteData.Success community ) ->
            List.any (.symbol >> (==) community.symbol) profile_.communities
                || List.any (.account >> (==) profile_.account) community.members

        ( Nothing, RemoteData.Success community ) ->
            List.any (.account >> (==) model.accountName) community.members

        _ ->
            False


loadCommunity : Model -> Maybe Eos.Symbol -> ( Model, Cmd Msg )
loadCommunity ({ shared } as model) maybeSymbol =
    ( { model
        | showCommunitySelector = False
        , selectedCommunity = RemoteData.Loading
      }
    , fetchCommunity shared model.authToken maybeSymbol
    )


{-| Given a `Community.Model`, check if the user is part of it (or if it has
auto invites), and set it as default or redirect the user
-}
setCommunity : Community.Model -> Model -> ( Model, Cmd Msg )
setCommunity community ({ shared } as model) =
    let
        isMember =
            case profile model of
                Just profile_ ->
                    List.any (.symbol >> (==) community.symbol) profile_.communities

                Nothing ->
                    List.any (.account >> (==) model.accountName) community.members

        storeCommunityCmd =
            Eos.symbolToString community.symbol
                |> Ports.storeSelectedCommunitySymbol

        sharedWithCommunity =
            { shared | selectedCommunity = Just community.symbol }
    in
    if isMember then
        let
            newProfile =
                case profile model of
                    Nothing ->
                        model.profile

                    Just profile_ ->
                        RemoteData.Success
                            { profile_
                                | communities =
                                    List.updateIf (.symbol >> (==) community.symbol)
                                        (\c ->
                                            { c
                                                | name = community.name
                                                , logo = community.logo
                                                , hasKyc = community.hasKyc
                                            }
                                        )
                                        profile_.communities
                            }
        in
        ( { model
            | selectedCommunity = RemoteData.Success community
            , profile = newProfile
            , shared = sharedWithCommunity
          }
        , storeCommunityCmd
        )

    else if community.hasAutoInvite then
        ( { model | selectedCommunity = RemoteData.Success community, shared = sharedWithCommunity }
        , Cmd.batch [ Route.pushUrl shared.navKey (Route.Join (List.head model.routeHistory)), storeCommunityCmd ]
        )

    else
        ( { model | selectedCommunity = RemoteData.Success community, shared = sharedWithCommunity }
        , Cmd.batch [ Route.pushUrl shared.navKey (Route.CommunitySelector (List.head model.routeHistory)), storeCommunityCmd ]
        )


signUpForCommunity : Model -> Profile.CommunityInfo -> ( Model, Cmd Msg )
signUpForCommunity ({ shared, authToken } as model) communityInfo =
    ( { model | selectedCommunity = RemoteData.Loading }
    , Api.Graphql.query shared
        (Just authToken)
        (Community.symbolQuery communityInfo.symbol)
        CompletedLoadCommunity
    )


{-| Given minimal information, selects a community. This means querying for the
entire `Community.Model`, and then setting it in the `Model`
-}
selectCommunity : Model -> { community | symbol : Eos.Symbol, subdomain : String } -> Route -> ( Model, Cmd Msg )
selectCommunity ({ shared, authToken } as model) community route =
    if shared.useSubdomain then
        ( model
        , Route.loadExternalCommunity shared community route
        )

    else
        ( { model | selectedCommunity = RemoteData.Loading }
        , fetchCommunity shared authToken (Just community.symbol)
        )


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



-- INFO


profile : Model -> Maybe Profile.Model
profile model =
    RemoteData.toMaybe model.profile


isAccount : Eos.Name -> Model -> Bool
isAccount accountName model =
    Maybe.map .account (profile model) == Just accountName



-- UNREAD NOTIFICATIONS


type alias UnreadMeta =
    Int


unreadSelection : SelectionSet UnreadMeta Cambiatus.Object.UnreadNotifications
unreadSelection =
    Cambiatus.Object.UnreadNotifications.unreads


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



-- BROADCAST


maybeInitWith : (a -> msg) -> (Model -> RemoteData e a) -> Model -> Cmd msg
maybeInitWith toMsg attribute model =
    case attribute model of
        RemoteData.Success value ->
            Task.succeed value
                |> Task.perform toMsg

        _ ->
            Cmd.none


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

        "GotActionMsg" :: remainAddress ->
            Action.jsAddressToMsg remainAddress val
                |> Maybe.map GotActionMsg

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        GotTimeInternal _ ->
            [ "GotTimeInternal" ]

        SearchClosed ->
            [ "SearchClosed" ]

        GotSearchMsg _ ->
            [ "GotSearchMsg" ]

        GotActionMsg actionMsg ->
            "GotActionMsg" :: Action.msgToString actionMsg

        CompletedLoadTranslation _ r ->
            [ "CompletedLoadTranslation", UR.resultToString r ]

        ClickedTryAgainTranslation ->
            [ "ClickedTryAgainTranslation" ]

        CompletedLoadProfile r ->
            [ "CompletedLoadProfile", UR.remoteDataToString r ]

        CompletedLoadCommunity r ->
            [ "CompletedLoadCommunity", UR.remoteDataToString r ]

        ClickedTryAgainProfile _ ->
            [ "ClickedTryAgainProfile" ]

        ClickedLogout ->
            [ "ClickedLogout" ]

        ShowUserNav _ ->
            [ "ShowUserNav" ]

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

        OpenCommunitySelector ->
            [ "OpenCommunitySelector" ]

        CloseCommunitySelector ->
            [ "CloseCommunitySelector" ]

        SelectedCommunity _ ->
            [ "SelectedCommunity" ]

        GotFeedbackMsg _ ->
            [ "GotFeedbackMsg" ]
