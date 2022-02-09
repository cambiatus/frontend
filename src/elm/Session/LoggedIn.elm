module Session.LoggedIn exposing
    ( BroadcastMsg(..)
    , External(..)
    , ExternalMsg(..)
    , Model
    , Msg(..)
    , Page(..)
    , Resource(..)
    , addFeedback
    , executeFeedback
    , init
    , initLogin
    , initRequestingAuthToken
    , isAccount
    , jsAddressToMsg
    , mapExternal
    , mapMsg
    , maybeInitWith
    , maybePrivateKey
    , msgToString
    , profile
    , query
    , subscriptions
    , update
    , updateExternal
    , view
    , withAuthToken
    , withPrivateKey
    )

import Action
import Api.Graphql
import Auth
import Avatar
import Cambiatus.Object
import Cambiatus.Object.UnreadNotifications
import Cambiatus.Subscription as Subscription
import Community
import Community.News
import Dict
import Environment
import Eos
import Eos.Account as Eos
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, footer, h2, img, li, nav, p, span, text, ul)
import Html.Attributes exposing (alt, class, classList, src, type_)
import Html.Attributes.Aria exposing (ariaLabel, ariaLive)
import Html.Events exposing (onClick, onMouseEnter)
import Http
import I18Next exposing (Delims(..), Translations)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Log
import Markdown
import Maybe.Extra
import Notification exposing (Notification)
import Ports
import Profile
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Search exposing (State(..))
import Session.Guest exposing (Msg(..))
import Session.Shared as Shared exposing (Shared, Translators)
import Shop
import Task
import Time
import Translation
import UpdateResult as UR
import Utils
import View.Components
import View.Feedback as Feedback
import View.Modal as Modal



-- INIT


{-| Initialize already logged in user when the page is [re]loaded.
-}
init : Shared -> Eos.Name -> Api.Graphql.Token -> ( Model, Cmd (Msg externalMsg) )
init shared accountName authToken =
    let
        ( model, cmd ) =
            initModel shared Nothing accountName (Just authToken)
    in
    ( model
      -- TODO - We need to check if auth token is valid
    , Cmd.batch
        [ Api.Graphql.query shared (Just authToken) (Profile.query accountName) CompletedLoadProfile
        , fetchCommunity shared authToken Nothing
        , Task.perform GotTimeInternal Time.now
        , cmd
        ]
    )


{-| Initialize already logged in user when the page is [re]loaded, but their auth
token is no longer valid (needed after PR #680)
-}
initRequestingAuthToken : Shared -> Eos.Name -> ( Model, Cmd (Msg externalMsg) )
initRequestingAuthToken shared accountName =
    let
        ( model, cmd ) =
            initModel shared Nothing accountName Nothing
    in
    ( model
    , Cmd.batch
        [ Task.perform GotTimeInternal Time.now
        , cmd
        , generateAuthToken model
            { callback =
                \authToken ->
                    Cmd.batch
                        [ fetchCommunity model.shared authToken Nothing
                        , Api.Graphql.query model.shared
                            (Just authToken)
                            (Profile.query model.accountName)
                            CompletedLoadProfile
                        ]
            }

        -- , generateAuthToken model
        --     { callback =
        --         \authToken ->
        --             CmdCallback
        --                 (Cmd.batch
        --                     [ fetchCommunity model.shared authToken Nothing
        --                     -- TODO - use new `query` function on logged in
        --                     , Api.Graphql.query model.shared
        --                         (Just authToken)
        --                         (Profile.query model.accountName)
        --                         CompletedLoadProfile
        --                     ]
        --                 )
        --     }
        ]
    )


fetchCommunity : Shared -> Api.Graphql.Token -> Maybe Eos.Symbol -> Cmd (Msg externalMsg)
fetchCommunity shared authToken maybeToken =
    if shared.useSubdomain then
        Api.Graphql.query shared
            (Just authToken)
            (Community.subdomainQuery (Environment.communityDomain shared.url))
            CompletedLoadCommunity

    else
        let
            symbol =
                Maybe.Extra.or maybeToken shared.selectedCommunity
                    |> Maybe.withDefault Eos.cambiatusSymbol
        in
        Api.Graphql.query shared (Just authToken) (Community.symbolQuery symbol) CompletedLoadCommunity


fetchTranslations : Translation.Language -> Cmd (Msg externalMsg)
fetchTranslations language =
    CompletedLoadTranslation language
        |> Translation.get language


{-| Initialize logged in user after signing-in.
-}
initLogin : Shared -> Maybe Eos.PrivateKey -> Profile.Model -> Api.Graphql.Token -> ( Model, Cmd (Msg externalMsg) )
initLogin shared maybePrivateKey_ profile_ authToken =
    let
        loadedProfile =
            Just profile_
                |> RemoteData.Success
                |> Task.succeed
                |> Task.perform CompletedLoadProfile

        ( model, cmd ) =
            initModel shared maybePrivateKey_ profile_.account (Just authToken)
    in
    ( model
    , Cmd.batch
        [ loadedProfile
        , fetchCommunity shared authToken Nothing
        , Task.perform GotTimeInternal Time.now
        , cmd
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub (Msg externalMsg)
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
    , authToken : Maybe Api.Graphql.Token
    , hasSeenDashboard : Bool
    , queuedCommunityFields : List Community.Field
    , maybeHighlightedNews : Maybe Community.News.Model
    }


initModel : Shared -> Maybe Eos.PrivateKey -> Eos.Name -> Maybe Api.Graphql.Token -> ( Model, Cmd (Msg externalMsg) )
initModel shared maybePrivateKey_ accountName authToken =
    let
        ( authModel, authCmd ) =
            Auth.init shared.pinVisibility maybePrivateKey_
    in
    ( { shared = shared
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
      , auth = authModel
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
    , Cmd.map GotAuthMsg authCmd
    )


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
    | News (Maybe Int)
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


view : (Msg msg -> msg) -> Page -> Model -> Html msg -> Html msg
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
            div []
                [ View.Components.loadingLogoAnimated shared.translators ""
                , viewAuthModal thisMsg model
                ]

        ( _, RemoteData.NotAsked ) ->
            div []
                [ View.Components.loadingLogoAnimated shared.translators ""
                , viewAuthModal thisMsg model
                ]

        ( _, RemoteData.Failure err ) ->
            div []
                [ Shared.viewFullGraphqlError shared
                    err
                    (ClickedTryAgainProfile model.accountName)
                    "An error occurred while loading profile."
                    |> Html.map thisMsg
                , viewAuthModal thisMsg model
                ]

        ( _, RemoteData.Success profile_ ) ->
            viewHelper thisMsg page profile_ model content


hideCommunityAndSearch : Page -> Model -> Bool
hideCommunityAndSearch currentPage model =
    let
        hiddenPages =
            [ CommunitySelector ]
    in
    List.member currentPage hiddenPages || not (isCommunityMember model)


viewHelper : (Msg pageMsg -> pageMsg) -> Page -> Profile.Model -> Model -> Html pageMsg -> Html pageMsg
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
                    viewClaimWithProofs action p.proof True

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
                        let
                            isInNewsPage =
                                case page of
                                    News maybeNewsId ->
                                        maybeNewsId == Just news.id

                                    _ ->
                                        False

                            showHighlightedNews =
                                not (Maybe.Extra.isJust news.receipt)
                                    && not (isAdminPage page)
                                    && not isInNewsPage
                        in
                        if showHighlightedNews then
                            viewHighlightedNews shared.translators pageMsg news

                        else
                            text ""

                    Nothing ->
                        text ""
               )
            :: mainView
            ++ [ viewFooter shared
               , Action.viewClaimConfirmation shared.translators model.claimingAction
                    |> Html.map (GotActionMsg >> pageMsg)
               , viewAuthModal pageMsg model
               , communitySelectorModal model
                    |> Html.map pageMsg
               ]
        )


viewAuthModal : (Msg pageMsg -> pageMsg) -> Model -> Html pageMsg
viewAuthModal pageMsg ({ shared } as model) =
    Modal.initWith
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


viewHighlightedNews : Translators -> (Msg pageMsg -> pageMsg) -> Community.News.Model -> Html pageMsg
viewHighlightedNews { t } toPageMsg news =
    div
        [ class "bg-purple-500 py-4 sticky top-0 z-10"
        ]
        [ div [ class "container mx-auto px-4 text-white flex items-center" ]
            [ Icons.speechBubble
                [ alt "" ]
                "stroke-current flex-shrink-0"
            , div [ class "truncate ml-4 mr-8" ]
                [ h2
                    [ class "font-bold truncate"
                    , ariaLive "polite"
                    ]
                    [ span [ class "sr-only" ] [ text <| t "news.got_community_news" ]
                    , text news.title
                    ]
                , p [ class "truncate" ]
                    [ text <| Markdown.toUnformattedString news.description ]
                ]
            , a
                [ class "button button-primary w-auto px-4 ml-auto mr-6"
                , Route.href (Route.News { selectedNews = Just news.id, showOthers = True })
                , onClick (toPageMsg ClickedReadHighlightedNews)
                ]
                [ text <| t "news.read" ]
            , button
                [ class "hover:text-red focus:text-red focus:outline-none"
                , ariaLabel <| t "menu.close"
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


viewHeader : Page -> Model -> Profile.Model -> Html (Msg externalMsg)
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
            [ class "flex items-center justify-end space-x-8 my-auto shrink-0"
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


viewCommunitySelector : Model -> Html (Msg externalMsg)
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


communitySelectorModal : Model -> Html (Msg externalMsg)
communitySelectorModal model =
    let
        t s =
            model.shared.translators.t s

        text_ s =
            text (t s)

        viewCommunityItem : Profile.CommunityInfo -> Html (Msg externalMsg)
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


viewMainMenu : Page -> Model -> Html (Msg externalMsg)
viewMainMenu page model =
    let
        closeClaimWithPhoto =
            GotActionMsg Action.ClaimConfirmationClosed

        menuItem title route =
            a
                [ class "text-center uppercase py-2 hover:text-orange-300 focus-ring focus-visible:ring-orange-300 focus-visible:ring-opacity-50 rounded-sm"
                , classList
                    [ ( "text-orange-300 font-bold", isActive page route )
                    , ( "text-gray-900", not (isActive page route) )
                    ]
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


isAdminPage : Page -> Bool
isAdminPage page =
    List.member page
        [ CommunitySettings
        , CommunitySettingsInfo
        , CommunitySettingsNews
        , CommunitySettingsNewsEditor
        , CommunitySettingsCurrency
        , CommunitySettingsFeatures
        , CommunitySettingsSponsorship
        , CommunitySettingsSponsorshipFiat
        , CommunitySettingsSponsorshipThankYouMessage
        , ObjectiveEditor
        , ActionEditor
        ]


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
    | SetCommunityField Community.FieldValue
    | RequiredPrivateKey { successMsg : msg, errorMsg : msg }
    | RequiredAuthToken { callbackCmd : Api.Graphql.Token -> Cmd msg }
    | RequestQuery (Cmd (Result { callbackCmd : Shared -> Api.Graphql.Token -> Cmd msg } msg))
    | ShowFeedback Feedback.Status String
    | HideFeedback


{-| Perform a GraphQL query. This function is preferred over `Api.Graphql.query`
for logged in users because it automatically detects if the user's auth token is
valid. If it's not valid, it automatically generates a new one (might need to ask
for user's pin), and runs the original query again.

It only retries once though, so if there are multiple authentication errors for
the same query, we stop trying, and send the error to the page that requested
the query. If the error is not related to authentication, we don't retry, we
just send the error the page that requested the query.

-}
query :
    Model
    -> SelectionSet result RootQuery
    -> (RemoteData (Graphql.Http.Error result) result -> msg)
    -> External msg
query model selectionSet toMsg =
    graphqlOperation Api.Graphql.query model selectionSet toMsg


mutation :
    Model
    -> SelectionSet result RootMutation
    -> (RemoteData (Graphql.Http.Error result) result -> msg)
    -> External msg
mutation model selectionSet toMsg =
    graphqlOperation Api.Graphql.mutation model selectionSet toMsg


graphqlOperation :
    (Shared
     -> Maybe Api.Graphql.Token
     -> SelectionSet result typeLock
     -> (rawOperationResult -> rawOperationResult)
     -> Cmd (RemoteData (Graphql.Http.Error result) result)
    )
    -> Model
    -> SelectionSet result typeLock
    -> (RemoteData (Graphql.Http.Error result) result -> msg)
    -> External msg
graphqlOperation operation model selectionSet toMsg =
    -- TODO - If this works, we don't need `initRequestingAuthToken`, we just need to use this instead
    -- TODO - We also won't need `withAuthToken`
    let
        operationCmd : Shared -> Api.Graphql.Token -> Cmd (RemoteData (Graphql.Http.Error result) result)
        operationCmd shared authToken =
            operation shared
                (Just authToken)
                selectionSet
                identity

        treatAuthError : RemoteData (Graphql.Http.Error result) result -> Result { callbackCmd : Shared -> Api.Graphql.Token -> Cmd msg } msg
        treatAuthError operationResult =
            case operationResult of
                RemoteData.Success success ->
                    -- We can return the result as a page msg
                    Ok (toMsg (RemoteData.Success success))

                RemoteData.Failure err ->
                    if Debug.todo "Is auth error err" then
                        -- TODO - queryCmd would cause a recursive type
                        -- TODO - We should probably create a regular queryCmd and just use Cmd.map
                        -- We need to run stuff inside LoggedIn
                        -- Debug.todo "Generate new auth token"
                        Err
                            { callbackCmd =
                                \newShared ->
                                    operationCmd newShared
                                        >> Cmd.map toMsg
                            }

                    else
                        -- We can return the result as a page msg
                        Ok (toMsg (RemoteData.Failure err))

                _ ->
                    -- We can return the result as a page msg
                    Ok (toMsg operationResult)
    in
    case model.authToken of
        Nothing ->
            -- We need to run stuff inside LoggedIn
            RequiredAuthToken { callbackCmd = Debug.todo "This needs to be a Cmd Msg" }

        Just authToken ->
            operationCmd model.shared authToken
                |> Cmd.map treatAuthError
                |> RequestQuery


addFeedback :
    Feedback.Model
    -> UR.UpdateResult model msg (External msg)
    -> UR.UpdateResult model msg (External msg)
addFeedback feedback ur =
    UR.addExt (executeFeedback feedback) ur


executeFeedback : Feedback.Model -> External msg
executeFeedback feedback =
    case feedback of
        Feedback.Visible status message ->
            ShowFeedback status message

        Feedback.Hidden ->
            HideFeedback


mapMsg : (msg -> otherMsg) -> Msg msg -> Msg otherMsg
mapMsg mapFn msg =
    case msg of
        NoOp ->
            NoOp

        CompletedLoadTranslation language result ->
            CompletedLoadTranslation language result

        ClickedTryAgainTranslation ->
            ClickedTryAgainTranslation

        CompletedLoadProfile result ->
            CompletedLoadProfile result

        CompletedLoadCommunity result ->
            CompletedLoadCommunity result

        CompletedLoadCommunityField community result ->
            CompletedLoadCommunityField community result

        CompletedLoadCommunityFields community result ->
            CompletedLoadCommunityFields community result

        ClickedTryAgainProfile account ->
            ClickedTryAgainProfile account

        ClickedLogout ->
            ClickedLogout

        ShowUserNav showUserNav ->
            ShowUserNav showUserNav

        ToggleLanguageItems ->
            ToggleLanguageItems

        ClickedLanguage language ->
            ClickedLanguage language

        ClosedAuthModal ->
            ClosedAuthModal

        GotAuthMsg subMsg ->
            GotAuthMsg subMsg

        CompletedLoadUnread jsonValue ->
            CompletedLoadUnread jsonValue

        OpenCommunitySelector ->
            OpenCommunitySelector

        CloseCommunitySelector ->
            CloseCommunitySelector

        SelectedCommunity communityInfo ->
            SelectedCommunity communityInfo

        GotFeedbackMsg subMsg ->
            GotFeedbackMsg subMsg

        GotSearchMsg subMsg ->
            GotSearchMsg subMsg

        GotActionMsg subMsg ->
            GotActionMsg subMsg

        SearchClosed ->
            SearchClosed

        ClickedProfileIcon ->
            ClickedProfileIcon

        GotTimeInternal time ->
            GotTimeInternal time

        CompletedLoadContributionCount result ->
            CompletedLoadContributionCount result

        ClickedReadHighlightedNews ->
            ClickedReadHighlightedNews

        ClosedHighlightedNews ->
            ClosedHighlightedNews

        ReceivedNewHighlightedNews jsonValue ->
            ReceivedNewHighlightedNews jsonValue

        GotAuthTokenPhrase callback result ->
            GotAuthTokenPhrase (callback >> Cmd.map (mapMsg mapFn)) result

        SignedAuthTokenPhrase password ->
            SignedAuthTokenPhrase password

        CompletedGeneratingAuthToken result ->
            CompletedGeneratingAuthToken result


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

        SetCommunityField value ->
            SetCommunityField value

        RequestedReloadCommunityField field ->
            RequestedReloadCommunityField field

        RequiredPrivateKey { successMsg, errorMsg } ->
            RequiredPrivateKey { successMsg = mapFn successMsg, errorMsg = mapFn errorMsg }

        RequiredAuthToken { callbackCmd } ->
            RequiredAuthToken { callbackCmd = callbackCmd >> Cmd.map mapFn }

        RequestQuery externalCmdResult ->
            RequestQuery
                (Cmd.map
                    (\result ->
                        case result of
                            Err { callbackCmd } ->
                                Err
                                    { callbackCmd =
                                        \shared authToken ->
                                            callbackCmd shared authToken
                                                |> Cmd.map mapFn
                                    }

                            Ok callbackMsg ->
                                Ok (mapFn callbackMsg)
                    )
                    externalCmdResult
                )

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
        , cmd : Cmd (Msg msg)
        , broadcastMsg : Maybe BroadcastMsg
        , afterAuthMsg : Maybe { successMsg : msg, errorMsg : msg }
        , afterAuthTokenMsg : Maybe { callbackMsg : msg }
        }
updateExternal externalMsg ({ shared } as model) =
    let
        defaultResult =
            { model = model
            , cmd = Cmd.none
            , broadcastMsg = Nothing
            , afterAuthMsg = Nothing
            , afterAuthTokenMsg = Nothing
            }
    in
    case externalMsg of
        UpdatedLoggedIn newModel ->
            { defaultResult | model = newModel }

        AddedCommunity communityInfo ->
            -- case model.authToken of
            --     Nothing ->
            --         { defaultResult | cmd = generateAuthToken model { callback = Debug.todo "" } }
            --     Just authToken ->
            --         let
            --             ( newModel, cmd ) =
            --                 signUpForCommunity model authToken communityInfo
            --             profileWithCommunity =
            --                 case profile newModel of
            --                     Nothing ->
            --                         newModel.profile
            --                     Just profile_ ->
            --                         RemoteData.Success
            --                             { profile_ | communities = communityInfo :: profile_.communities }
            --         in
            --         { defaultResult
            --             | model = { newModel | profile = profileWithCommunity }
            --             , cmd = cmd
            --         }
            Debug.todo "Use withAuthTokenCmd or similar"

        CreatedCommunity symbol subdomain ->
            -- case model.authToken of
            -- Nothing ->
            --     { defaultResult | cmd = generateAuthToken model { callback = Debug.todo "" } }
            -- Just authToken ->
            --     let
            --         ( newModel, cmd ) =
            --             selectCommunity model authToken { symbol = symbol, subdomain = subdomain } Route.Dashboard
            --     in
            --     { defaultResult | model = newModel, cmd = cmd }
            Debug.todo "Use withAuthTokenCmd or similar"

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
            { defaultResult
                | cmd =
                    (\authToken ->
                        model.selectedCommunity
                            |> RemoteData.map .symbol
                            |> RemoteData.toMaybe
                            |> loadCommunity model authToken
                            |> Tuple.second
                    )
                        |> withAuthTokenCmd model
            }

        ReloadResource ProfileResource ->
            { defaultResult
                | cmd =
                    (\authToken ->
                        -- TODO - Use a queryInternal function to handle auth errors
                        Api.Graphql.query model.shared
                            (Just authToken)
                            (Profile.query model.accountName)
                            CompletedLoadProfile
                    )
                        |> withAuthTokenCmd model
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
                            Just fieldValue ->
                                { defaultResult
                                    | broadcastMsg =
                                        Just
                                            (CommunityFieldLoaded community fieldValue)
                                }

                            Nothing ->
                                { defaultResult
                                    | cmd =
                                        (\authToken ->
                                            Community.queryForField community.symbol
                                                shared
                                                authToken
                                                field
                                                (CompletedLoadCommunityField community)
                                        )
                                            |> withAuthTokenCmd model
                                    , model =
                                        { model
                                            | selectedCommunity =
                                                Community.setFieldAsLoading field community
                                                    |> RemoteData.Success
                                        }
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
                            (\authToken ->
                                -- TODO - Use a queryInternal function to handle auth errors
                                Community.queryForField community.symbol
                                    shared
                                    authToken
                                    field
                                    (CompletedLoadCommunityField community)
                            )
                                |> withAuthTokenCmd model
                    }

                _ ->
                    { defaultResult
                        | model =
                            { model
                                | queuedCommunityFields =
                                    field :: model.queuedCommunityFields
                            }
                    }

        SetCommunityField value ->
            case model.selectedCommunity of
                RemoteData.Success community ->
                    { defaultResult
                        | model =
                            { model
                                | selectedCommunity =
                                    Community.setFieldValue value community
                                        |> RemoteData.Success
                            }
                    }

                _ ->
                    { defaultResult
                        | cmd =
                            Log.fromImpossible externalMsg
                                "Tried setting community field, but community wasn't loaded"
                                (Just model.accountName)
                                { moduleName = "Session.LoggedIn", function = "updateExternal" }
                                [ Log.contextFromCommunity model.selectedCommunity ]
                                |> Log.send externalMsgToString
                    }

        RequiredPrivateKey afterAuthMsg ->
            { defaultResult
                | model = askedAuthentication model
                , afterAuthMsg = Just afterAuthMsg
            }

        RequiredAuthToken afterAuthTokenMsg ->
            -- TODO - We onl need this in `query`, so we can change it's signature if needed
            { defaultResult
                | cmd = Debug.todo ""

                -- TODO
                -- | cmd =
                --     Api.Graphql.askForPhrase shared
                --         model.accountName
                --         GotAuthTokenPhraseWithCmd
                -- , afterAuthTokenMsg = Just afterAuthTokenMsg
            }

        RequestQuery queryCmd ->
            Debug.todo ""

        ShowFeedback status message ->
            { defaultResult | model = { model | feedback = Feedback.Visible status message } }

        HideFeedback ->
            { defaultResult | model = { model | feedback = Feedback.Hidden } }


type alias UpdateResult msg =
    UR.UpdateResult Model (Msg msg) (ExternalMsg msg)


{-| Messages that LoggedIn can fire, and pages/Main will react to
-}
type ExternalMsg msg
    = AuthenticationSucceed
    | AuthenticationFailed
    | AddAfterAuthTokenCallback (Api.Graphql.Token -> Cmd msg)
    | AddAfterAuthTokenCallbackInternal (Api.Graphql.Token -> Cmd (Msg msg))
    | RunAfterAuthTokenCallbacks Api.Graphql.Token
    | AddAfterPrivateKeyCallback (Msg msg)
    | RunAfterPrivateKeyCallbacks
      -- TODO
      -- | AuthTokenFailed
    | Broadcast BroadcastMsg
    | RunExternalMsg msg


type BroadcastMsg
    = CommunityLoaded Community.Model
    | CommunityFieldLoaded Community.Model Community.FieldValue
    | ProfileLoaded Profile.Model
    | GotTime Time.Posix
    | TranslationsLoaded


type Msg externalMsg
    = NoOp
    | CompletedLoadTranslation Translation.Language (Result Http.Error Translations)
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
    | ReceivedNewHighlightedNews Value
    | GotAuthTokenPhrase (Api.Graphql.Token -> Cmd (Msg externalMsg)) (RemoteData (Graphql.Http.Error Api.Graphql.Phrase) Api.Graphql.Phrase)
    | SignedAuthTokenPhrase Api.Graphql.Password
    | CompletedGeneratingAuthToken (RemoteData (Graphql.Http.Error Api.Graphql.SignInResponse) Api.Graphql.SignInResponse)


update : Msg msg -> Model -> UpdateResult msg
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

        ClickedProfileIcon ->
            { closeAllModals | searchModel = Search.closeSearch model.searchModel }
                |> UR.init

        GotSearchMsg searchMsg ->
            case model.selectedCommunity of
                RemoteData.Success community ->
                    (\authToken ->
                        Search.update shared authToken community.symbol model.searchModel searchMsg
                            |> UR.fromChild (\searchModel -> { model | searchModel = searchModel })
                                GotSearchMsg
                                (\feedback -> UR.mapModel (\newModel -> { newModel | feedback = feedback }))
                                { model | hasSeenDashboard = model.hasSeenDashboard || Search.isOpenMsg searchMsg }
                            |> UR.mapModel
                                (\newModel -> { newModel | hasSeenDashboard = newModel.hasSeenDashboard || Search.isOpenMsg searchMsg })
                    )
                        |> withAuthTokenInternal model { callbackMsg = msg }

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
            (\authToken ->
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
                            (Just authToken)
                            (Profile.contributionCountQuery community.symbol model.accountName)
                            CompletedLoadContributionCount
                in
                { newModel
                    | selectedCommunity = RemoteData.Success newCommunity
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
                            authToken
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
            )
                |> withAuthTokenInternal model { callbackMsg = msg }

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
            (\authToken ->
                UR.init { model | profile = RemoteData.Loading }
                    |> UR.addCmd
                        (Api.Graphql.query shared
                            (Just authToken)
                            (Profile.query accountName)
                            CompletedLoadProfile
                        )
            )
                |> withAuthTokenInternal model { callbackMsg = msg }

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
                            Auth.CompletedAuth accountName auth ->
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
                                    |> UR.mapModel (\m -> { m | auth = auth })
                                    |> UR.addExt AuthenticationSucceed
                                    |> UR.addCmd cmd
                                    |> UR.addExt RunAfterPrivateKeyCallbacks
                                    |> UR.addBreadcrumb
                                        { type_ = Log.DefaultBreadcrumb
                                        , category = msg
                                        , message = "Successfully authenticated user through PIN"
                                        , data = Dict.fromList [ ( "username", Eos.encodeName accountName ) ]
                                        , level = Log.Info
                                        }

                            Auth.UpdatedShared newShared ->
                                uResult
                                    |> UR.mapModel (\m -> { m | shared = newShared })

                            Auth.SetFeedback feedbackModel ->
                                uResult
                                    |> UR.mapModel (\m -> { m | feedback = feedbackModel })
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
            (\authToken ->
                let
                    ( loadCommunityModel, loadCommunityCmd ) =
                        loadCommunity model authToken (Just symbol)
                in
                case model.selectedCommunity of
                    RemoteData.Success selectedCommunity ->
                        if symbol == selectedCommunity.symbol then
                            UR.init { model | showCommunitySelector = False }

                        else
                            let
                                ( newModel, cmd ) =
                                    selectCommunity model
                                        authToken
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
            )
                |> withAuthTokenInternal model { callbackMsg = msg }

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
                    UR.init model
                        |> UR.logImpossible msg
                            "Received new highlighted news, but community wasn't loaded"
                            (Just model.accountName)
                            { moduleName = "Session.LoggedIn", function = "update" }
                            [ Log.contextFromCommunity model.selectedCommunity ]

        GotAuthTokenPhrase callback (RemoteData.Success phrase) ->
            (\privateKey ->
                model
                    |> UR.init
                    |> UR.addPort (Api.Graphql.signPhrasePort msg privateKey phrase)
                    |> UR.addExt (AddAfterAuthTokenCallbackInternal callback)
            )
                |> withPrivateKeyInternal msg model

        GotAuthTokenPhrase _ (RemoteData.Failure err) ->
            -- TODO - Maybe we should let the pages know there was an error?
            { model
                | auth = Auth.removePrivateKey model.auth
                , feedback = Feedback.Visible Feedback.Failure (shared.translators.t "auth.failed")
            }
                |> UR.init
                |> UR.addPort
                    { responseAddress = NoOp
                    , responseData = Encode.null
                    , data = Encode.object [ ( "name", Encode.string "logout" ) ]
                    }
                |> UR.logGraphqlError msg
                    (Just model.accountName)
                    "Got an error when fetching phrase to sign for auth token"
                    { moduleName = "Session.LoggedIn"
                    , function = "update"
                    }
                    []
                    err

        GotAuthTokenPhrase _ _ ->
            UR.init model

        SignedAuthTokenPhrase signedPhrase ->
            model
                |> UR.init
                |> UR.addCmd
                    (Api.Graphql.signIn shared
                        { account = model.accountName
                        , password = signedPhrase
                        , invitationId = getInvitation model
                        }
                        CompletedGeneratingAuthToken
                    )

        CompletedGeneratingAuthToken (RemoteData.Success signInResponse) ->
            { model
                | profile = RemoteData.Success signInResponse.profile
                , authToken = Just signInResponse.token
            }
                |> UR.init
                |> UR.addCmd (Api.Graphql.storeToken signInResponse.token)
                |> UR.addExt (RunAfterAuthTokenCallbacks signInResponse.token)

        CompletedGeneratingAuthToken (RemoteData.Failure err) ->
            -- TODO - Maybe we should let the pages know there was an error?
            { model
                | auth = Auth.removePrivateKey model.auth
                , feedback = Feedback.Visible Feedback.Failure (shared.translators.t "auth.failed")
            }
                |> UR.init
                |> UR.addPort
                    { responseAddress = NoOp
                    , responseData = Encode.null
                    , data = Encode.object [ ( "name", Encode.string "logout" ) ]
                    }
                |> UR.logGraphqlError msg
                    (Just model.accountName)
                    "Got an error when signing in"
                    { moduleName = "Session.LoggedIn"
                    , function = "update"
                    }
                    []
                    err

        CompletedGeneratingAuthToken _ ->
            model
                |> UR.init


handleActionMsg : Model -> Action.Msg -> UpdateResult msg
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
                community.symbol
                model.accountName
                actionMsg
                model.claimingAction
                |> UR.map
                    actionModelToLoggedIn
                    GotActionMsg
                    (\feedback -> UR.mapModel (\prevModel -> { prevModel | feedback = feedback }))
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
again. Necessary to perform EOS transactions
-}
withPrivateKey :
    Model
    -> subModel
    -> { successMsg : subMsg, errorMsg : subMsg }
    -> UR.UpdateResult subModel subMsg (External subMsg)
    -> UR.UpdateResult subModel subMsg (External subMsg)
withPrivateKey loggedIn subModel subMsg successfulUR =
    if hasPrivateKey loggedIn then
        successfulUR

    else
        UR.init subModel
            |> UR.addExt (RequiredPrivateKey subMsg)


withPrivateKeyInternal : Msg msg -> Model -> (Eos.PrivateKey -> UpdateResult msg) -> UpdateResult msg
withPrivateKeyInternal msg loggedIn successfulUR =
    case maybePrivateKey loggedIn of
        Just privateKey ->
            successfulUR privateKey

        Nothing ->
            -- askedAuthentication { loggedIn | internalAfterAuthMsg = Just msg }
            askedAuthentication loggedIn
                |> UR.init
                |> UR.addExt (AddAfterPrivateKeyCallback msg)


withAuthToken :
    Model
    -> subModel
    -> { callbackMsg : subMsg }
    -> (Api.Graphql.Token -> UR.UpdateResult subModel subMsg (External subMsg))
    -> UR.UpdateResult subModel subMsg (External subMsg)
withAuthToken loggedIn subModel subMsg successfulUR =
    -- TODO - Remove this function in favor of `query`
    case loggedIn.authToken of
        Just authToken ->
            successfulUR authToken

        Nothing ->
            UR.init subModel
                -- |> UR.addExt (RequiredAuthToken subMsg)
                |> Debug.todo "Use `LoggedIn.query` instead of `LoggedIn.withAuthToken`"


withAuthTokenInternal :
    Model
    -> { callbackMsg : Msg externalMsg }
    -> (Api.Graphql.Token -> UpdateResult msg)
    -> UpdateResult msg
withAuthTokenInternal model { callbackMsg } successfulUR =
    let
        _ =
            Debug.log "WITH AUTH TOKEN INTERNAL CALLED" True
    in
    case model.authToken of
        Just authToken ->
            successfulUR authToken

        Nothing ->
            -- UR.init model
            --     |> UR.addCmd (generateAuthToken model { callback = \_ -> MsgCallback callbackMsg })
            Debug.todo ""


{-| Check if model has auth token. If it does, just calls the callback cmd.
Otherwise, it generates a new auth token, and, in the end, calls the callback cmd.
-}
withAuthTokenCmd : Model -> (Api.Graphql.Token -> Cmd (Msg externalMsg)) -> Cmd (Msg externalMsg)
withAuthTokenCmd model callback =
    case model.authToken of
        Just authToken ->
            -- TODO - We should check the result to see if it's an auth error
            -- TODO - If it is an auth error, we should re-generate the token
            callback authToken

        Nothing ->
            Api.Graphql.askForPhrase model.shared
                model.accountName
                (GotAuthTokenPhrase callback)


generateAuthToken : Model -> { callback : Api.Graphql.Token -> Cmd (Msg externalMsg) } -> Cmd (Msg externalMsg)
generateAuthToken model { callback } =
    Api.Graphql.askForPhrase model.shared
        model.accountName
        (GotAuthTokenPhrase callback)


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


loadCommunity : Model -> Api.Graphql.Token -> Maybe Eos.Symbol -> ( Model, Cmd (Msg externalMsg) )
loadCommunity ({ shared } as model) authToken maybeSymbol =
    ( { model
        | showCommunitySelector = False
        , selectedCommunity = RemoteData.Loading
      }
    , fetchCommunity shared authToken maybeSymbol
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
setCommunity : Community.Model -> Model -> ( Model, Cmd (Msg externalMsg) )
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


signUpForCommunity : Model -> Api.Graphql.Token -> Profile.CommunityInfo -> ( Model, Cmd (Msg externalMsg) )
signUpForCommunity ({ shared } as model) authToken communityInfo =
    ( { model | selectedCommunity = RemoteData.Loading }
    , Api.Graphql.query shared
        (Just authToken)
        (Community.symbolQuery communityInfo.symbol)
        CompletedLoadCommunity
    )


{-| Given minimal information, selects a community. This means querying for the
entire `Community.Model`, and then setting it in the `Model`
-}
selectCommunity : Model -> Api.Graphql.Token -> { community | symbol : Eos.Symbol, subdomain : String } -> Route -> ( Model, Cmd (Msg externalMsg) )
selectCommunity ({ shared } as model) authToken community route =
    if shared.useSubdomain then
        ( model
        , Route.loadExternalCommunity shared community route
        )

    else
        ( { model | selectedCommunity = RemoteData.Loading }
        , fetchCommunity shared authToken (Just community.symbol)
        )


closeModal : UpdateResult msg -> UpdateResult msg
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
    Subscription.highlightedNews { communityId = Eos.symbolToString symbol }
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


jsAddressToMsg : List String -> Value -> Maybe (Msg externalMsg)
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

        "GotAuthTokenPhrase" :: _ ->
            Api.Graphql.decodeSignedPhrasePort SignedAuthTokenPhrase val

        _ ->
            Nothing


msgToString : Msg externalMsg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

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

        GotAuthTokenPhrase _ _ ->
            [ "GotAuthTokenPhrase" ]

        SignedAuthTokenPhrase _ ->
            [ "SignedAuthTokenPhrase" ]

        CompletedGeneratingAuthToken r ->
            [ "CompletedGeneratingAuthToken", UR.remoteDataToString r ]


externalMsgToString : External msg -> List String
externalMsgToString externalMsg =
    case externalMsg of
        UpdatedLoggedIn _ ->
            [ "UpdatedLoggedIn" ]

        AddedCommunity _ ->
            [ "AddedCommunity" ]

        CreatedCommunity symbol _ ->
            [ "CreatedCommunity", Eos.symbolToString symbol ]

        ExternalBroadcast _ ->
            [ "ExternalBroadcast" ]

        ReloadResource _ ->
            [ "ReloadResource" ]

        RequestedReloadCommunityField _ ->
            [ "RequestedReloadCommunityField" ]

        RequestedCommunityField _ ->
            [ "RequestedCommunityField" ]

        SetCommunityField _ ->
            [ "SetCommunityField" ]

        RequiredPrivateKey _ ->
            [ "RequiredPrivateKey" ]

        RequiredAuthToken _ ->
            [ "RequiredAuthToken" ]

        RequestQuery _ ->
            [ "RequestQuery" ]

        ShowFeedback _ _ ->
            [ "ShowFeedback" ]

        HideFeedback ->
            [ "HideFeedback" ]
