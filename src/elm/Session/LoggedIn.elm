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
    , mapExternal
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
import Cambiatus.Object
import Cambiatus.Object.UnreadNotifications
import Cambiatus.Subscription as Subscription
import Community
import Community.News
import Dict
import Eos
import Eos.Account as Eos
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootSubscription)
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, footer, h1, img, li, nav, p, text, ul)
import Html.Attributes exposing (class, classList, src, type_)
import Html.Events exposing (onClick, onMouseEnter)
import Http
import I18Next exposing (Delims(..), Translations)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Log
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
import Utils
import View.Components
import View.Feedback as Feedback
import View.MarkdownEditor
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


fetchTranslations : Translation.Language -> Cmd Msg
fetchTranslations language =
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
        [ loadedProfile
        , fetchCommunity shared authToken Nothing
        , Task.perform GotTimeInternal Time.now
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotSearchMsg Search.subscriptions
        , Sub.map GotActionMsg (Action.subscriptions model.claimingAction)
        , Time.every (60 * 1000) GotTimeInternal
        , if model.showUserNav then
            Utils.escSubscription (ShowUserNav False)

          else
            Sub.none
        , case model.searchModel.state of
            Inactive ->
                Sub.none

            _ ->
                Utils.escSubscription (GotSearchMsg Search.closeMsg)
        ]



-- MODEL


type alias Model =
    { shared : Shared
    , routeHistory : List Route
    , accountName : Eos.Name
    , profile : RemoteData (Graphql.Http.Error (Maybe Profile.Model)) Profile.Model
    , selectedCommunity : RemoteData (Graphql.Http.Error (Maybe Community.Model)) Community.Model
    , contributionCount : RemoteData (Graphql.Http.Error (Maybe Int)) Int
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
    , hasSeenDashboard : Bool
    , queuedCommunityFields : List Community.Field
    , maybeHighlightedNews : Maybe Community.News.Model
    }


initModel : Shared -> Maybe Eos.PrivateKey -> Eos.Name -> String -> Model
initModel shared maybePrivateKey_ accountName authToken =
    { shared = shared
    , routeHistory = []
    , accountName = accountName
    , profile = RemoteData.Loading
    , selectedCommunity = RemoteData.Loading
    , contributionCount = RemoteData.NotAsked
    , showUserNav = False
    , showLanguageItems = False
    , showNotificationModal = False
    , showMainNav = False
    , showCommunitySelector = False
    , showAuthModal = False
    , auth = Auth.init shared.pinVisibility maybePrivateKey_
    , notification = Notification.init
    , unreadCount = 0
    , feedback = Feedback.Hidden
    , searchModel = Search.init
    , claimingAction = { status = Action.NotAsked, feedback = Nothing, needsPinConfirmation = False }
    , authToken = authToken
    , hasSeenDashboard = False
    , queuedCommunityFields = []
    , maybeHighlightedNews = Nothing
    }


hasPrivateKey : Model -> Bool
hasPrivateKey model =
    Auth.hasPrivateKey model.auth


maybePrivateKey : Model -> Maybe Eos.PrivateKey
maybePrivateKey model =
    Auth.maybePrivateKey model.auth



-- VIEW


type Page
    = Redirect
    | NotFound
    | ComingSoon
    | Invite
    | Dashboard
    | News
    | Community
    | CommunitySettings
    | CommunitySettingsInfo
    | CommunitySettingsNews
    | CommunitySettingsNewsEditor
    | CommunitySettingsCurrency
    | CommunitySettingsFeatures
    | CommunitySettingsSponsorship
    | CommunitySettingsSponsorshipFiat
    | CommunitySettingsSponsorshipThankYouMessage
    | CommunityEditor
    | CommunitySelector
    | CommunityThankYou
    | CommunitySponsor
    | CommunitySupporters
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
    | ProfileContributions
    | ProfileEditor
    | ProfileAddKyc
    | ProfileClaims
    | ProfileAddContact
    | PaymentHistory
    | Transfer
    | ViewTransfer
    | Analysis
    | Join


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
            :: (case model.maybeHighlightedNews of
                    Just news ->
                        -- case news.receipt of
                        --     Just _ ->
                        --         viewHighlightedNews pageMsg news
                        --     Nothing ->
                        --         text ""
                        -- TODO - Conditionally show the highlighted news
                        viewHighlightedNews pageMsg news

                    Nothing ->
                        text ""
               )
            :: mainView
            ++ [ viewFooter shared
               , Action.viewClaimConfirmation shared.translators model.claimingAction
                    |> Html.map (GotActionMsg >> pageMsg)
               , Modal.initWith
                    { closeMsg = ClosedAuthModal
                    , isVisible = model.showAuthModal
                    }
                    |> Modal.withHeader (shared.translators.t "auth.login.modalFormTitle")
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


viewHighlightedNews : (Msg -> pageMsg) -> Community.News.Model -> Html pageMsg
viewHighlightedNews toPageMsg news =
    div [ class "bg-purple-500 p-4" ]
        [ div [ class "container mx-auto px-4 text-white flex items-center" ]
            [ Icons.speechBubble "stroke-current flex-shrink-0"
            , div [ class "truncate ml-4 mr-8" ]
                [ h1 [ class "font-bold truncate" ] [ text news.title ]
                , p [ class "truncate" ] [ text <| View.MarkdownEditor.removeFormatting news.description ]
                ]
            , a
                [ class "button button-primary w-auto px-4 ml-auto mr-6"
                , Route.href (Route.News (Just news.id))
                , onClick (toPageMsg ClickedReadHighlightedNews)
                ]
                -- TODO - I18N
                [ text "Read" ]
            , button
                [ class "hover:text-red focus:text-red focus:outline-none"
                , onClick (toPageMsg ClosedHighlightedNews)
                ]
                [ Icons.close "fill-current" ]
            ]
        ]


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
            [ Redirect
            , NotFound
            , ComingSoon
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

        isCommunityCreator =
            case model.selectedCommunity of
                RemoteData.Success community ->
                    community.creator == model.accountName

                _ ->
                    False

        isSearchOpen =
            case model.searchModel.state of
                Search.Inactive ->
                    False

                _ ->
                    True
    in
    div [ class "flex flex-wrap items-center justify-between p-4 md:flex-nowrap" ]
        [ div
            [ class "flex-shrink-0"
            , classList
                [ ( "md:flex-shrink md:w-full lg:w-2/3 xl:w-full", not isSearchOpen )
                , ( "lg:w-full", not isCommunityCreator && not isSearchOpen )
                ]
            ]
            (if hideCommunitySelector then
                [ img [ class "hidden sm:block h-5", src shared.logo ] []
                , img [ class "sm:hidden h-5", src shared.logoMobile ] []
                ]

             else
                [ viewCommunitySelector model ]
            )
        , if hideCommunityAndSearch page model then
            div [] []

          else
            Search.viewForm
                [ class "order-last w-full md:order-none mt-4 md:mt-0 md:mx-4"
                , classList
                    [ ( "md:w-96 md:flex-shrink-0", not isSearchOpen )
                    , ( "w-full", isSearchOpen )
                    ]
                ]
                shared.translators
                model.searchModel
                |> Html.map GotSearchMsg
        , div
            [ class "flex items-center justify-end space-x-8 my-auto flex-shrink-0"
            , classList [ ( "md:flex-shrink md:w-full", not isSearchOpen ) ]
            ]
            [ a
                [ class "relative rounded-sm group focus-ring focus-visible:ring-orange-300 focus-visible:ring-opacity-50"
                , Route.href Route.Notification
                , classList [ ( "mr-4", model.unreadCount > 0 ) ]
                ]
                [ Icons.notification "fill-current text-gray-900 h-6 md:h-7 group-hover:text-orange-300"
                , if model.unreadCount > 0 then
                    div [ class "absolute top-0 right-0 -mr-2 px-1 py-0.5 bg-orange-500 text-white font-semibold text-xs rounded-full md:-mr-4 md:px-2 md:py-1 md:text-sm" ]
                        [ text (String.fromInt model.unreadCount) ]

                  else
                    text ""
                ]
            , if isCommunityCreator then
                a
                    [ class "rounded-sm group focus-ring focus:ring-orange-300 focus:ring-opacity-50 focus:ring-offset-4"
                    , Route.href Route.CommunitySettings
                    ]
                    [ Icons.settings "fill-current h-6 text-gray-900 md:h-7 group-hover:text-orange-300" ]

              else
                text ""
            , div [ class "relative z-50" ]
                [ button
                    [ class "h-12 z-10 py-2 px-3 relative hidden lg:visible lg:flex lg:items-center lg:bg-white lg:focus-ring lg:focus-visible:ring-orange-300 lg:focus-visible:ring-opacity-50"
                    , classList
                        [ ( "rounded-tr-lg rounded-tl-lg", model.showUserNav )
                        , ( "rounded-lg", not model.showUserNav )
                        ]
                    , type_ "button"
                    , onClick (ShowUserNav (not model.showUserNav))
                    , onMouseEnter (ShowUserNav True)
                    ]
                    [ Avatar.view profile_.avatar "h-8 w-8"
                    , div [ class "flex flex-col items-center text-left pl-2" ]
                        [ p [ class "w-full text-gray-333 overflow-x-hidden" ]
                            [ text (tr "menu.welcome_message" [ ( "user_name", Eos.nameToString profile_.account ) ]) ]
                        , p [ class "w-full text-orange-300" ]
                            [ text (shared.translators.t "menu.my_account") ]
                        ]
                    ]
                , button
                    [ class "z-10 flex relative focus-ring focus-visible:ring-orange-300 focus-visible:ring-opacity-50 focus-visible:ring-offset-4 lg:hidden"
                    , classList [ ( "rounded-tr-lg rounded-tl-lg", model.showUserNav ) ]
                    , classList [ ( "rounded-lg", not model.showUserNav ) ]
                    , type_ "button"
                    , onClick (ShowUserNav (not model.showUserNav))
                    , onMouseEnter (ShowUserNav True)
                    ]
                    [ Avatar.view profile_.avatar "h-6 w-6 md:h-7 md:w-7"
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
                , if model.showUserNav then
                    View.Components.focusTrap { firstFocusContainer = Nothing }
                        []
                        [ nav
                            [ class "absolute right-0 lg:w-full py-2 px-4 shadow-lg bg-white rounded-t-lg rounded-b-lg lg:rounded-t-none z-50" ]
                            [ a
                                [ class "flex block w-full px-4 py-4 justify-start items-center text-sm focus-ring rounded-sm hover:text-orange-300 focus-visible:text-orange-300"
                                , Route.href (Route.Profile model.accountName)
                                , onClick ClickedProfileIcon
                                ]
                                [ Icons.profile "mr-4 fill-current"
                                , text_ "menu.profile"
                                ]
                            , button
                                [ class "flex block w-full px-4 py-4 justify-start items-center text-sm border-t focus-ring rounded-sm hover:text-orange-300 focus-visible:text-orange-300"
                                , onClick ToggleLanguageItems
                                ]
                                [ Icons.languages "mr-4 fill-current"
                                , text_ "menu.languages"
                                ]
                            , if model.showLanguageItems then
                                div [ class "ml-6 mb-2" ]
                                    (button
                                        [ class "flex px-4 py-2 text-gray items-center text-indigo-500 font-bold text-xs uppercase focus-ring rounded-sm"
                                        ]
                                        [ Shared.langFlag shared.language
                                        , text (Translation.languageToLanguageCode shared.language)
                                        ]
                                        :: Shared.viewLanguageItems shared ClickedLanguage
                                    )

                              else
                                text ""
                            , button
                                [ class "flex block w-full px-4 py-4 justify-start items-center text-sm border-t focus-ring rounded-sm hover:text-red focus-visible:text-red"
                                , onClick ClickedLogout
                                ]
                                [ Icons.close "fill-current m-1 mr-5"
                                , text_ "menu.logout"
                                ]
                            ]
                        ]

                  else
                    text ""
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
            button
                [ class "flex items-center rounded-sm focus-ring focus:ring-offset-4"
                , onClick OpenCommunitySelector
                ]
                [ img [ class "h-8 w-8 object-scale-down", src community.logo ] []
                , if hasMultipleCommunities then
                    Icons.arrowDown "fill-current text-gray-900"

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
            li [ class "flex" ]
                [ button
                    [ class "w-full flex items-center p-3 m-1 text-body rounded-sm hover:text-black hover:bg-gray-100 focus:outline-none focus:ring"
                    , onClick <| SelectedCommunity c
                    ]
                    [ img [ src c.logo, class "h-16 w-16 mr-5 object-scale-down" ] []
                    , text c.name
                    ]
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
                            , ul [ class "w-full flex flex-col overflow-y-auto divide-y divide-gray-300" ]
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
        closeClaimWithPhoto =
            GotActionMsg Action.ClaimConfirmationClosed

        menuItem title route =
            a
                [ class "text-center text-gray-900 uppercase py-2 hover:text-orange-300 focus-ring focus-visible:ring-orange-300 focus-visible:ring-opacity-50 rounded-sm"
                , classList [ ( "text-orange-300 font-bold", isActive page route ) ]
                , Route.href route
                , onClick closeClaimWithPhoto
                ]
                [ text (model.shared.translators.t title) ]

        hasShop =
            model.selectedCommunity
                |> RemoteData.map .hasShop
                |> RemoteData.withDefault False

        isInDashboard =
            isActive page Route.Dashboard

        isInShop =
            isActive page (Route.Shop Shop.All)
    in
    nav
        [ class "grid relative md:mx-4"
        , classList
            [ ( "grid-cols-2 md:w-96", hasShop )
            , ( "md:w-48", not hasShop )
            ]
        ]
        [ menuItem "menu.dashboard" Route.Dashboard
        , if hasShop then
            menuItem "menu.shop" (Route.Shop Shop.All)

          else
            text ""
        , div
            [ class "absolute bottom-0 h-3px"
            , classList
                [ ( "w-1/2 transform transition-transform motion-reduce:transition-none", hasShop )
                , ( "w-full", not hasShop )
                , ( "translate-x-0", isInDashboard )
                , ( "translate-x-full", isInShop )
                , ( "bg-orange-300", isInDashboard || isInShop )
                ]
            ]
            []
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
            , Icons.heartSolid
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
    | RequestedReloadCommunityField Community.Field
    | RequestedCommunityField Community.Field
    | RequiredAuthentication { successMsg : msg, errorMsg : msg }
    | ShowFeedback Feedback.Status String
    | HideFeedback


mapExternal : (msg -> otherMsg) -> External msg -> External otherMsg
mapExternal mapFn msg =
    case msg of
        UpdatedLoggedIn model ->
            UpdatedLoggedIn model

        AddedCommunity communityInfo ->
            AddedCommunity communityInfo

        CreatedCommunity symbol name ->
            CreatedCommunity symbol name

        ExternalBroadcast broadcastMsg ->
            ExternalBroadcast broadcastMsg

        ReloadResource resource ->
            ReloadResource resource

        RequestedCommunityField field ->
            RequestedCommunityField field

        RequestedReloadCommunityField field ->
            RequestedReloadCommunityField field

        RequiredAuthentication { successMsg, errorMsg } ->
            RequiredAuthentication { successMsg = mapFn successMsg, errorMsg = mapFn errorMsg }

        ShowFeedback status message ->
            ShowFeedback status message

        HideFeedback ->
            HideFeedback


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
        , broadcastMsg : Maybe BroadcastMsg
        , afterAuthMsg : Maybe { successMsg : msg, errorMsg : msg }
        }
updateExternal externalMsg ({ shared } as model) =
    let
        defaultResult =
            { model = model
            , cmd = Cmd.none
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

                CommunityFieldLoaded community field ->
                    { defaultResult
                        | model =
                            { model
                                | selectedCommunity =
                                    Community.setFieldValue field community
                                        |> RemoteData.Success
                            }
                        , broadcastMsg = Just broadcastMsg
                    }

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

                TranslationsLoaded ->
                    { defaultResult | broadcastMsg = Just broadcastMsg }

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

        RequestedCommunityField field ->
            case model.selectedCommunity of
                RemoteData.Success community ->
                    if Community.isFieldLoading field community then
                        defaultResult

                    else
                        case Community.maybeFieldValue field community of
                            Nothing ->
                                { defaultResult
                                    | cmd =
                                        Community.queryForField community.symbol
                                            shared
                                            model.authToken
                                            field
                                            (CompletedLoadCommunityField community)
                                    , model =
                                        { model
                                            | selectedCommunity =
                                                Community.setFieldAsLoading field community
                                                    |> RemoteData.Success
                                        }
                                }

                            Just fieldValue ->
                                { defaultResult
                                    | broadcastMsg =
                                        Just
                                            (CommunityFieldLoaded community fieldValue)
                                }

                _ ->
                    { defaultResult
                        | model =
                            { model
                                | queuedCommunityFields =
                                    field :: model.queuedCommunityFields
                            }
                    }

        RequestedReloadCommunityField field ->
            case model.selectedCommunity of
                RemoteData.Success community ->
                    { defaultResult
                        | cmd =
                            Community.queryForField community.symbol
                                shared
                                model.authToken
                                field
                                (CompletedLoadCommunityField community)
                    }

                _ ->
                    { defaultResult
                        | model =
                            { model
                                | queuedCommunityFields =
                                    field :: model.queuedCommunityFields
                            }
                    }

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
    | CommunityFieldLoaded Community.Model Community.FieldValue
    | ProfileLoaded Profile.Model
    | GotTime Time.Posix
    | TranslationsLoaded


type Msg
    = CompletedLoadTranslation Translation.Language (Result Http.Error Translations)
    | ClickedTryAgainTranslation
    | CompletedLoadProfile (RemoteData (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))
    | CompletedLoadCommunity (RemoteData (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | CompletedLoadCommunityField Community.Model (RemoteData (Graphql.Http.Error (Maybe Community.FieldValue)) (Maybe Community.FieldValue))
    | CompletedLoadCommunityFields Community.Model (RemoteData (Graphql.Http.Error (List Community.FieldValue)) (List Community.FieldValue))
    | ClickedTryAgainProfile Eos.Name
    | ClickedLogout
    | ShowUserNav Bool
    | ToggleLanguageItems
    | ClickedLanguage Translation.Language
    | ClosedAuthModal
    | GotAuthMsg Auth.Msg
    | CompletedLoadUnread Value
    | OpenCommunitySelector
    | CloseCommunitySelector
    | SelectedCommunity Profile.CommunityInfo
    | GotFeedbackMsg Feedback.Msg
    | GotSearchMsg Search.Msg
    | GotActionMsg Action.Msg
    | SearchClosed
    | ClickedProfileIcon
    | GotTimeInternal Time.Posix
    | CompletedLoadContributionCount (RemoteData (Graphql.Http.Error (Maybe Int)) (Maybe Int))
    | ClickedReadHighlightedNews
    | ClosedHighlightedNews
    | ReceivedNewHighlightedNews Encode.Value


update : Msg -> Model -> UpdateResult
update msg model =
    let
        shared =
            model.shared

        closeAllModals =
            { model
                | showNotificationModal = False
                , showUserNav = False
                , showMainNav = False
                , showAuthModal = False
            }
    in
    case msg of
        GotTimeInternal time ->
            UR.init { model | shared = { shared | now = time } }
                |> UR.addExt (GotTime time |> Broadcast)

        GotActionMsg actionMsg ->
            handleActionMsg model actionMsg

        SearchClosed ->
            { model | searchModel = Search.closeSearch model.searchModel }
                |> UR.init

        ClickedProfileIcon ->
            { closeAllModals | searchModel = Search.closeSearch model.searchModel }
                |> UR.init

        GotSearchMsg searchMsg ->
            case model.selectedCommunity of
                RemoteData.Success community ->
                    let
                        ( searchModel, searchCmd ) =
                            Search.update shared model.authToken community.symbol model.searchModel searchMsg
                    in
                    { model
                        | searchModel = searchModel
                        , hasSeenDashboard = model.hasSeenDashboard || Search.isOpenMsg searchMsg
                    }
                        |> UR.init
                        |> UR.addCmd (Cmd.map GotSearchMsg searchCmd)

                _ ->
                    UR.init model

        CompletedLoadTranslation lang (Ok transl) ->
            case model.profile of
                RemoteData.Success _ ->
                    UR.init { model | shared = Shared.loadTranslation (Ok ( lang, transl )) shared }
                        |> UR.addCmd (Ports.storeLanguage (Translation.languageToLocale lang))
                        |> UR.addExt (Broadcast TranslationsLoaded)

                _ ->
                    UR.init model

        CompletedLoadTranslation _ (Err err) ->
            UR.init { model | shared = Shared.loadTranslation (Err err) shared }
                |> UR.logHttpError msg
                    (Just model.accountName)
                    "Got an error when loading translation as a logged in user"
                    { moduleName = "Session.LoggedIn", function = "update" }
                    []
                    err

        ClickedTryAgainTranslation ->
            UR.init { model | shared = Shared.toLoadingTranslation shared }
                |> UR.addCmd (fetchTranslations shared.language)
                |> UR.addBreadcrumb
                    { type_ = Log.ErrorBreadcrumb
                    , category = msg
                    , message = "Clicked to fetch translation again"
                    , data = Dict.empty
                    , level = Log.Warning
                    }

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
                        |> UR.addBreadcrumb
                            { type_ = Log.DefaultBreadcrumb
                            , category = msg
                            , message = "User profile successfully loaded"
                            , data = Dict.fromList [ ( "username", Eos.encodeName p.account ) ]
                            , level = Log.Info
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
                |> UR.logGraphqlError msg
                    (Just model.accountName)
                    "Got an error when trying to load profile"
                    { moduleName = "Session.LoggedIn", function = "update" }
                    []
                    err

        CompletedLoadProfile RemoteData.NotAsked ->
            UR.init model

        CompletedLoadProfile RemoteData.Loading ->
            UR.init model

        CompletedLoadCommunity (RemoteData.Success (Just community)) ->
            let
                ( newModel, cmd ) =
                    setCommunity
                        (Community.mergeFields model.selectedCommunity community)
                        model

                newCommunity =
                    Community.mergeFields newModel.selectedCommunity community
                        |> (\comm ->
                                List.foldl Community.setFieldAsLoading
                                    comm
                                    newModel.queuedCommunityFields
                           )

                queryForContributionCount =
                    Api.Graphql.query newModel.shared
                        (Just newModel.authToken)
                        (Profile.contributionCountQuery community.symbol model.accountName)
                        CompletedLoadContributionCount
            in
            { newModel
                | selectedCommunity = RemoteData.Success newCommunity

                -- TODO - Check if user has already read news
                , maybeHighlightedNews = community.highlightedNews
            }
                |> UR.init
                |> UR.addCmd cmd
                |> UR.addCmd (Ports.getRecentSearches ())
                |> UR.addPort
                    { responseAddress = ReceivedNewHighlightedNews Encode.null
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "subscribeToHighlightedNewsChanged" )
                            , ( "subscription"
                              , highlightedNewsSubscription newCommunity.symbol
                                    |> Graphql.Document.serializeSubscription
                                    |> Encode.string
                              )
                            ]
                    }
                |> UR.addExt (CommunityLoaded newCommunity |> Broadcast)
                |> UR.addCmd
                    (Community.queryForFields community.symbol
                        newModel.shared
                        newModel.authToken
                        newModel.queuedCommunityFields
                        (CompletedLoadCommunityFields newCommunity)
                    )
                |> UR.addCmd queryForContributionCount
                |> UR.addBreadcrumb
                    { type_ = Log.DefaultBreadcrumb
                    , category = msg
                    , message = "Community successfully loaded"
                    , data =
                        Dict.fromList
                            [ ( "symbol", Eos.encodeSymbol community.symbol )
                            , ( "name", Encode.string community.name )
                            ]
                    , level = Log.Info
                    }

        CompletedLoadCommunity (RemoteData.Success Nothing) ->
            UR.init model
                |> UR.addCmd (Route.pushUrl shared.navKey (Route.CommunitySelector (List.head model.routeHistory)))

        CompletedLoadCommunity (RemoteData.Failure e) ->
            let
                communityExists =
                    not (Community.isNonExistingCommunityError e)
            in
            UR.init { model | selectedCommunity = RemoteData.Failure e }
                |> UR.logGraphqlError msg
                    (Just model.accountName)
                    "Got an error when loading community as logged in"
                    { moduleName = "Session.LoggedIn", function = "update" }
                    []
                    e
                |> (if communityExists then
                        identity

                    else
                        UR.addCmd (Route.pushUrl shared.navKey (Route.CommunitySelector (List.head model.routeHistory)))
                   )

        CompletedLoadCommunity RemoteData.NotAsked ->
            UR.init { model | selectedCommunity = RemoteData.NotAsked }

        CompletedLoadCommunity RemoteData.Loading ->
            UR.init { model | selectedCommunity = RemoteData.Loading }

        CompletedLoadCommunityField community (RemoteData.Success (Just fieldValue)) ->
            let
                newCommunity =
                    model.selectedCommunity
                        |> RemoteData.withDefault community
                        |> Community.setFieldValue fieldValue
            in
            { model | selectedCommunity = RemoteData.Success newCommunity }
                |> UR.init
                |> UR.addExt
                    (CommunityFieldLoaded newCommunity fieldValue
                        |> Broadcast
                    )

        CompletedLoadCommunityField community (RemoteData.Success Nothing) ->
            model
                |> UR.init
                |> UR.logImpossible msg
                    "Tried loading community field, but got Nothing in return"
                    (Just model.accountName)
                    { moduleName = "Session.LoggedIn", function = "update" }
                    [ Log.contextFromCommunity (RemoteData.Success community) ]

        CompletedLoadCommunityField _ (RemoteData.Failure err) ->
            model
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just model.accountName)
                    "Got an error when loading community field"
                    { moduleName = "Session.LoggedIn", function = "update" }
                    []
                    err

        CompletedLoadCommunityField _ RemoteData.NotAsked ->
            UR.init model

        CompletedLoadCommunityField _ RemoteData.Loading ->
            UR.init model

        CompletedLoadCommunityFields community (RemoteData.Success fieldValues) ->
            let
                newCommunity =
                    List.foldl Community.setFieldValue
                        (RemoteData.withDefault community model.selectedCommunity)
                        fieldValues

                addBroadcasts uResult =
                    List.foldl
                        (\field ->
                            UR.addExt
                                (CommunityFieldLoaded community field
                                    |> Broadcast
                                )
                        )
                        uResult
                        fieldValues
            in
            { model | selectedCommunity = RemoteData.Success newCommunity }
                |> UR.init
                |> addBroadcasts

        CompletedLoadCommunityFields _ (RemoteData.Failure err) ->
            UR.init model
                |> UR.logGraphqlError msg
                    (Just model.accountName)
                    "Got an error when loading multiple community fields"
                    { moduleName = "Session.LoggedIn", function = "update" }
                    []
                    err

        CompletedLoadCommunityFields _ RemoteData.NotAsked ->
            UR.init model

        CompletedLoadCommunityFields _ RemoteData.Loading ->
            UR.init model

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

        ToggleLanguageItems ->
            UR.init { model | showLanguageItems = not model.showLanguageItems }

        ClickedLanguage lang ->
            UR.init
                { model
                    | shared = Shared.toLoadingTranslation shared
                    , showUserNav = False
                }
                |> UR.addCmd (fetchTranslations lang)

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
                                    |> UR.addBreadcrumb
                                        { type_ = Log.DefaultBreadcrumb
                                        , category = msg
                                        , message = "Successfully authenticated user through PIN"
                                        , data = Dict.fromList [ ( "username", Eos.encodeName user.account ) ]
                                        , level = Log.Info
                                        }

                            Auth.UpdatedShared newShared ->
                                uResult
                                    |> UR.mapModel (\m -> { m | shared = newShared })
                    )

        CompletedLoadUnread payload ->
            case Decode.decodeValue (unreadCountSubscription model.accountName |> Graphql.Document.decoder) payload of
                Ok res ->
                    { model | unreadCount = res }
                        |> UR.init

                Err err ->
                    model
                        |> UR.init
                        |> UR.logDecodingError msg
                            (Just model.accountName)
                            "Got an error when loading unread notifications"
                            { moduleName = "Session.LoggedIn", function = "update" }
                            []
                            err

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

        CompletedLoadContributionCount (RemoteData.Success (Just contributionCount)) ->
            { model | contributionCount = RemoteData.Success contributionCount }
                |> UR.init

        CompletedLoadContributionCount (RemoteData.Success Nothing) ->
            { model | contributionCount = RemoteData.Success 0 }
                |> UR.init
                |> UR.logImpossible msg
                    "Got contribution count successfully, but it returned `Nothing`"
                    (Just model.accountName)
                    { moduleName = "Session.LoggedIn"
                    , function = "update"
                    }
                    [ Log.contextFromCommunity model.selectedCommunity ]

        CompletedLoadContributionCount (RemoteData.Failure err) ->
            { model | contributionCount = RemoteData.Failure err }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just model.accountName)
                    "Got an error when loading contribution count"
                    { moduleName = "Session.LoggedIn"
                    , function = "update"
                    }
                    [ Log.contextFromCommunity model.selectedCommunity ]
                    err

        CompletedLoadContributionCount _ ->
            model
                |> UR.init

        ClosedHighlightedNews ->
            { model | maybeHighlightedNews = Nothing }
                |> UR.init

        ClickedReadHighlightedNews ->
            { model | maybeHighlightedNews = Nothing }
                |> UR.init

        ReceivedNewHighlightedNews payload ->
            case model.selectedCommunity of
                RemoteData.Success community ->
                    case
                        Decode.decodeValue
                            (highlightedNewsSubscription community.symbol
                                |> Graphql.Document.decoder
                            )
                            payload
                    of
                        Ok highlightedNews ->
                            { model
                                | selectedCommunity =
                                    RemoteData.Success
                                        { community | highlightedNews = highlightedNews }
                                , maybeHighlightedNews = highlightedNews
                            }
                                |> UR.init

                        Err err ->
                            model
                                |> UR.init
                                |> UR.logDecodingError msg
                                    (Just model.accountName)
                                    "Got an error when loading highlighted news"
                                    { moduleName = "Session.LoggedIn", function = "update" }
                                    []
                                    err

                _ ->
                    -- TODO - Log impossible
                    UR.init model
                        |> UR.logImpossible msg
                            "Received new highlighted news, but community wasn't loaded"
                            (Just model.accountName)
                            { moduleName = "Session.LoggedIn", function = "update" }
                            [ Log.contextFromCommunity model.selectedCommunity ]


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


getInvitation : Model -> Maybe String
getInvitation model =
    case List.head model.routeHistory of
        Just (Route.Invite invitation) ->
            Just invitation

        Just (Route.Login maybeInvitation _) ->
            maybeInvitation

        Just (Route.Register maybeInvitation _) ->
            maybeInvitation

        _ ->
            Nothing


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
        case getInvitation model of
            Just invitation ->
                ( { model | selectedCommunity = RemoteData.Success community, shared = sharedWithCommunity }
                , Route.pushUrl shared.navKey (Route.Invite invitation)
                )

            Nothing ->
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


highlightedNewsSubscription : Eos.Symbol -> SelectionSet (Maybe Community.News.Model) RootSubscription
highlightedNewsSubscription symbol =
    Subscription.highlightedNewsChange { communityId = Eos.symbolToString symbol }
        Community.News.selectionSet



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

        "ReceivedNewHighlightedNews" :: _ ->
            Decode.decodeValue (Decode.field "meta" Decode.value) val
                |> Result.map ReceivedNewHighlightedNews
                |> Result.toMaybe

        "GotActionMsg" :: remainAddress ->
            Action.jsAddressToMsg remainAddress val
                |> Maybe.map GotActionMsg

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        GotTimeInternal _ ->
            [ "GotTimeInternal" ]

        SearchClosed ->
            [ "SearchClosed" ]

        ClickedProfileIcon ->
            [ "ClickedProfileIcon" ]

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

        CompletedLoadCommunityField _ _ ->
            [ "CompletedLoadCommunityField" ]

        CompletedLoadCommunityFields _ _ ->
            [ "CompletedLoadCommunityFields" ]

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

        OpenCommunitySelector ->
            [ "OpenCommunitySelector" ]

        CloseCommunitySelector ->
            [ "CloseCommunitySelector" ]

        SelectedCommunity _ ->
            [ "SelectedCommunity" ]

        GotFeedbackMsg _ ->
            [ "GotFeedbackMsg" ]

        CompletedLoadContributionCount r ->
            [ "CompletedLoadContributionCount", UR.remoteDataToString r ]

        ClosedHighlightedNews ->
            [ "ClosedHighlightedNews" ]

        ClickedReadHighlightedNews ->
            [ "ClickedReadHighlightedNews" ]

        ReceivedNewHighlightedNews _ ->
            [ "ReceivedNewHighlightedNews" ]
