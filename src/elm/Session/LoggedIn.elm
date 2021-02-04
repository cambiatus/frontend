module Session.LoggedIn exposing
    ( External(..)
    , ExternalMsg(..)
    , FeatureStatus(..)
    , FeedbackStatus(..)
    , FeedbackVisibility(..)
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

import Action
import Api.Graphql
import Auth
import Avatar
import Browser.Dom as Dom
import Browser.Events
import Cambiatus.Object
import Cambiatus.Object.UnreadNotifications
import Cambiatus.Subscription as Subscription
import Community
import Eos exposing (Symbol)
import Eos.Account as Eos
import Flags exposing (Flags)
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootSubscription)
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, footer, img, nav, p, span, text)
import Html.Attributes exposing (class, classList, src, style, type_)
import Html.Events exposing (onClick, onMouseEnter)
import Http
import I18Next exposing (Delims(..), Translations)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Notification exposing (Notification)
import Ports exposing (JavascriptOutModel)
import Profile exposing (Model)
import Route exposing (Route)
import Search exposing (FoundItemsKind(..), State(..))
import Session.Shared as Shared exposing (Shared)
import Shop
import Task
import Translation
import UpdateResult as UR
import View.Modal as Modal



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
        , Api.Graphql.query shared (Community.settingsQuery flags.selectedCommunity) CompletedLoadSettings
        , Ports.getRecentSearches ()
        ]
    )


fetchTranslations : String -> Shared -> Cmd Msg
fetchTranslations language _ =
    CompletedLoadTranslation language
        |> Translation.get language


initLogin : Shared -> Auth.Model -> Profile.Model -> ( Model, Cmd Msg )
initLogin shared authModel profile_ =
    let
        selectedCommunity : Symbol
        selectedCommunity =
            List.head profile_.communities
                |> Maybe.map .id
                |> Maybe.withDefault Eos.cambiatusSymbol

        model =
            initModel shared authModel profile_.account selectedCommunity
    in
    ( { model
        | profile = Loaded profile_
      }
    , Task.perform
        (\_ -> SelectCommunity selectedCommunity Cmd.none)
        (Task.succeed ())
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotAuthMsg (Auth.subscriptions model.auth)
        , Sub.map KeyDown (Browser.Events.onKeyDown (Decode.field "key" Decode.string))
        , Sub.map GotSearchMsg Search.subscriptions
        ]



-- MODEL


type alias Model =
    { shared : Shared
    , accountName : Eos.Name
    , profile : ProfileStatus
    , selectedCommunity : Symbol
    , showUserNav : Bool
    , showLanguageItems : Bool
    , showNotificationModal : Bool
    , showMainNav : Bool
    , notification : Notification.Model
    , unreadCount : Int
    , showAuthModal : Bool
    , auth : Auth.Model
    , showCommunitySelector : Bool
    , feedback : FeedbackVisibility
    , hasShop : FeatureStatus
    , hasObjectives : FeatureStatus
    , hasKyc : FeatureStatus
    , searchModel : Search.Model
    , actionToClaim : Maybe Action.Model
    }


type FeatureStatus
    = FeatureLoaded Bool
    | FeatureLoading


initModel : Shared -> Auth.Model -> Eos.Name -> Symbol -> Model
initModel shared authModel accountName selectedCommunity =
    { shared = shared
    , accountName = accountName
    , profile = Loading accountName
    , selectedCommunity = selectedCommunity
    , showUserNav = False
    , showLanguageItems = False
    , showNotificationModal = False
    , showMainNav = False
    , notification = Notification.init
    , unreadCount = 0
    , showAuthModal = False
    , auth = authModel
    , feedback = Hidden
    , showCommunitySelector = False
    , hasShop = FeatureLoading
    , hasObjectives = FeatureLoading
    , hasKyc = FeatureLoading
    , searchModel = Search.init selectedCommunity
    , actionToClaim = Nothing
    }


type FeedbackStatus
    = Success
    | Failure


type FeedbackVisibility
    = Show FeedbackStatus String
    | Hidden


type ProfileStatus
    = Loading Eos.Name
    | LoadingFailed Eos.Name (Graphql.Http.Error (Maybe Profile.Model))
    | Loaded Profile.Model


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
    | Community
    | CommunitySettings
    | CommunitySettingsFeatures
    | CommunityEditor
    | Objectives
    | ObjectiveEditor
    | ActionEditor
    | ClaimAction
    | Claim
    | News
    | Learn
    | Notification
    | Shop
    | ShopEditor
    | ShopViewer
    | FAQ
    | Profile
    | ProfilePublic
    | ProfileEditor
    | ProfileAddKyc
    | ProfileClaims
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

        ( _, Loading _ ) ->
            Shared.viewFullLoading

        ( _, LoadingFailed accountName err ) ->
            Shared.viewFullGraphqlError shared
                err
                (ClickedTryAgainProfile accountName)
                "An error occurred while loading profile."
                |> Html.map thisMsg

        ( _, Loaded profile_ ) ->
            viewHelper thisMsg page profile_ model content


viewFeedback : FeedbackStatus -> String -> Html Msg
viewFeedback status message =
    let
        color =
            case status of
                Success ->
                    " bg-green"

                Failure ->
                    " bg-red"
    in
    div
        [ class <| "w-full sticky z-10 top-0 transition duration-500 ease-in-out bg-blue-500 hover:bg-red-500 transform hover:-translate-y-1 hover:scale-110" ++ color
        , style "display" "grid"
        , style "grid-template" "\". text x\" 100% / 10% 80% 10%"
        ]
        [ span
            [ class "flex justify-center items-center text-sm h-10 leading-snug text-white font-bold"
            , style "grid-area" "text"
            ]
            [ text message ]
        , span
            [ class "flex justify-center items-center ml-auto mr-6 cursor-pointer"
            , style "grid-area" "x"
            , onClick HideFeedbackLocal
            ]
            [ Icons.close "fill-current text-white"
            ]
        ]


viewHelper : (Msg -> msg) -> Page -> Profile.Model -> Model -> Html msg -> Html msg
viewHelper thisMsg page profile_ ({ shared } as model) content =
    let
        { t } =
            shared.translators
    in
    div
        [ class "min-h-screen flex flex-col" ]
        ([ div [ class "bg-white" ]
            [ div [ class "container mx-auto" ]
                [ viewHeader model profile_ |> Html.map thisMsg
                , case model.actionToClaim of
                    Just ca ->
                        Action.viewClaimConfirmation
                            model.selectedCommunity
                            shared.translators
                            ca.claimConfirmationModalStatus
                            |> Html.map (thisMsg << GotActionMsg)

                    Nothing ->
                        text ""
                , -- Search form is separated from search results because it needs to
                  -- be between community selector and user dropdown on Desktops.
                  Search.viewForm model.searchModel
                    |> Html.map (GotSearchMsg >> thisMsg)
                , if Search.isActive model.searchModel then
                    text ""

                  else
                    viewMainMenu page model |> Html.map thisMsg
                ]
            ]
         ]
            ++ (if Search.isActive model.searchModel then
                    [ div [ class "container mx-auto flex flex-grow" ]
                        [ case model.searchModel.found of
                            Just ({ actions, offers } as results) ->
                                case ( List.length actions, List.length offers ) of
                                    ( 0, 0 ) ->
                                        Search.viewEmptyResults model.searchModel.queryText
                                            |> Html.map (GotSearchMsg >> thisMsg)

                                    _ ->
                                        let
                                            wrapWithClass c inner =
                                                div [ class ("flex-grow " ++ c) ]
                                                    [ inner ]
                                        in
                                        case model.searchModel.state of
                                            ResultsShowed Offers ->
                                                div []
                                                    [ Search.viewTabs results Offers
                                                    , Search.viewOffers model.selectedCommunity results.offers
                                                        |> wrapWithClass "bg-gray-100"
                                                    ]
                                                    |> Html.map (GotSearchMsg >> thisMsg)

                                            ResultsShowed Actions ->
                                                div []
                                                    [ Search.viewTabs results Actions
                                                        |> Html.map (GotSearchMsg >> thisMsg)
                                                    , Action.viewSearchActions model.selectedCommunity results.actions
                                                        |> wrapWithClass "bg-gray-100"
                                                        |> Html.map (GotActionMsg >> thisMsg)
                                                    ]

                                            _ ->
                                                Search.viewResultsOverview results
                                                    |> wrapWithClass "bg-white p-4"
                                                    |> Html.map (GotSearchMsg >> thisMsg)

                            Nothing ->
                                Search.viewRecentQueries model.searchModel |> Html.map (GotSearchMsg >> thisMsg)
                        ]
                    ]

                else
                    viewPageBody t model profile_ page content thisMsg
               )
            ++ [ viewFooter shared
               , Modal.initWith
                    { closeMsg = ClosedAuthModal
                    , isVisible = model.showAuthModal
                    }
                    |> Modal.withBody
                        (Auth.view True shared model.auth
                            |> List.map (Html.map GotAuthMsg)
                        )
                    |> Modal.toHtml
                    |> Html.map thisMsg
               , communitySelectorModal model
                    |> Html.map thisMsg
               ]
        )


viewPageBody t model profile_ page content thisMsg =
    let
        hasUserKycFilled =
            case profile_.kyc of
                Just _ ->
                    True

                Nothing ->
                    False

        availableWithoutKyc : List Page
        availableWithoutKyc =
            [ Other
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
    [ case model.feedback of
        Show status message ->
            viewFeedback status message |> Html.map thisMsg

        Hidden ->
            text ""
    , div [ class "flex-grow" ]
        [ case model.hasKyc of
            FeatureLoading ->
                div [ class "full-spinner-container h-full" ]
                    [ div [ class "spinner spinner--delay mt-8" ] [] ]

            FeatureLoaded isKycEnabled ->
                let
                    isContentAllowed =
                        List.member page availableWithoutKyc
                            || not isKycEnabled
                            || (isKycEnabled && hasUserKycFilled)
                in
                if isContentAllowed then
                    content

                else
                    viewKycRestriction
        ]
    ]


viewHeader : Model -> Profile.Model -> Html Msg
viewHeader ({ shared } as model) profile_ =
    let
        text_ str =
            text (shared.translators.t str)

        tr str values =
            shared.translators.tr str values
    in
    div [ class "flex flex-wrap items-center justify-between px-4 pt-6 pb-4" ]
        [ viewCommunitySelector model
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
            , div [ class "relative z-20" ]
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
                    [ class "absolute right-0 lg:w-full py-2 px-4 shadow-lg bg-white rounded-t-lg rounded-b-lg lg:rounded-t-none"
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

        hasMultipleCommunities : Bool
        hasMultipleCommunities =
            case model.profile of
                Loaded p ->
                    List.length p.communities > 1

                _ ->
                    False
    in
    case findCommunity model.selectedCommunity of
        Just community ->
            button [ class "flex items-center", onClick OpenCommunitySelector ]
                [ img [ class "h-10", src community.logo ] []
                , if hasMultipleCommunities then
                    Icons.arrowDown ""

                  else
                    text ""
                ]

        Nothing ->
            button [ class "flex items-center", onClick OpenCommunitySelector ]
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
            model.shared.translators.t s

        text_ s =
            text (t s)

        viewCommunityItem : Profile.CommunityInfo -> Html Msg
        viewCommunityItem c =
            div
                [ class "flex items-center p-4 text-body cursor-pointer hover:text-black hover:bg-gray-100"
                , onClick <| SelectCommunity c.id (Route.replaceUrl model.shared.navKey Route.Dashboard)
                ]
                [ img [ src c.logo, class "h-16 w-16 mr-5 object-scale-down" ] []
                , text c.name
                ]
    in
    if model.showCommunitySelector then
        case model.profile of
            Loaded pro ->
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
            , text (model.shared.translators.t "menu.dashboard")
            ]
        , case model.hasShop of
            FeatureLoaded True ->
                a
                    [ classList
                        [ ( menuItemClass, True )
                        , ( activeClass, isActive page (Route.Shop Shop.All) )
                        ]
                    , Route.href (Route.Shop Shop.All)
                    ]
                    [ Icons.shop iconClass
                    , text (model.shared.translators.t "menu.shop")
                    ]

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


type External msg
    = UpdatedLoggedIn Model
    | RequiredAuthentication (Maybe msg)
    | ShowFeedback FeedbackStatus String
    | HideFeedback


mapExternal : (msg -> msg2) -> External msg -> External msg2
mapExternal transform ext =
    case ext of
        UpdatedLoggedIn m ->
            UpdatedLoggedIn m

        RequiredAuthentication maybeM ->
            RequiredAuthentication (Maybe.map transform maybeM)

        ShowFeedback message status ->
            ShowFeedback message status

        HideFeedback ->
            HideFeedback


type alias UpdateResult =
    UR.UpdateResult Model Msg ExternalMsg


type ExternalMsg
    = AuthenticationSucceed
    | AuthenticationFailed


type Msg
    = NoOp
    | CompletedLoadTranslation String (Result Http.Error Translations)
    | ClickedTryAgainTranslation
    | CompletedLoadProfile (Result (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))
    | CompletedLoadSettings (Result (Graphql.Http.Error (Maybe Community.Settings)) (Maybe Community.Settings))
    | ClickedTryAgainProfile Eos.Name
    | ClickedLogout
    | ShowNotificationModal Bool
    | ShowUserNav Bool
    | ShowMainNav Bool
    | ToggleLanguageItems
    | ClickedLanguage String
    | ClosedAuthModal
    | GotAuthMsg Auth.Msg
    | CompletedLoadUnread Value
    | KeyDown String
    | OpenCommunitySelector
    | CloseCommunitySelector
    | SelectCommunity Symbol (Cmd Msg)
    | HideFeedbackLocal
    | GotSearchMsg Search.Msg
    | GotActionMsg Action.Msg
    | SearchClosed


update : Msg -> Model -> UpdateResult
update msg model =
    let
        shared =
            model.shared

        { t, tr } =
            shared.translators

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

        GotActionMsg actionMsg ->
            let
                updateClaimingAction : Action.Model -> Model
                updateClaimingAction actionModel =
                    { model
                        | actionToClaim =
                            Action.update shared.translators actionMsg actionModel
                                |> Just
                    }
            in
            case actionMsg of
                Action.ClaimConfirmationOpen action ->
                    updateClaimingAction (Action.initClaimingActionModel action)
                        |> UR.init

                Action.ActionClaimed ->
                    case model.actionToClaim of
                        Just ca ->
                            let
                                claimPort : JavascriptOutModel Msg
                                claimPort =
                                    Action.claimActionPort
                                        (GotActionMsg Action.ActionClaimed)
                                        ca.action
                                        shared.contracts.community
                                        model.accountName
                            in
                            if isAuth model then
                                updateClaimingAction ca
                                    |> UR.init
                                    |> UR.addPort claimPort

                            else
                                updateClaimingAction ca
                                    |> UR.init

                        -- TODO: !!! Maybe return UpdateResult from Action.view?
                        --|> UR.addExt (RequiredAuthentication (Just (GotActionMsg Action.ActionClaimed)))
                        Nothing ->
                            model
                                |> UR.init

                Action.ActionWithPhotoLinkClicked route ->
                    case model.actionToClaim of
                        Just ca ->
                            updateClaimingAction ca
                                |> UR.init
                                |> UR.addCmd (Route.replaceUrl model.shared.navKey route)

                        Nothing ->
                            model
                                |> UR.init

                Action.GotActionClaimedResponse resp ->
                    case ( model.actionToClaim, resp ) of
                        ( Just ca, Ok _ ) ->
                            let
                                message =
                                    tr "dashboard.check_claim.success"
                                        [ ( "symbolCode", Eos.symbolToSymbolCodeString model.selectedCommunity ) ]
                            in
                            updateClaimingAction ca
                                |> (\m -> { m | feedback = Show Success message })
                                |> UR.init

                        ( Just ca, Err _ ) ->
                            updateClaimingAction ca
                                |> (\m -> { m | feedback = Show Failure (t "dashboard.check_claim.failure") })
                                |> UR.init

                        ( Nothing, _ ) ->
                            model
                                |> UR.init

                _ ->
                    case model.actionToClaim of
                        Just ca ->
                            updateClaimingAction ca
                                |> UR.init

                        Nothing ->
                            model
                                |> UR.init

        SearchClosed ->
            { model
                | searchModel =
                    Search.closeSearch shared model.searchModel
                        |> Tuple.first
            }
                |> UR.init

        GotSearchMsg searchMsg ->
            let
                ( searchModel, searchCmd ) =
                    Search.update shared model.searchModel searchMsg
            in
            { model | searchModel = searchModel }
                |> UR.init
                |> UR.addCmd (Cmd.map GotSearchMsg searchCmd)

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

        CompletedLoadSettings (Ok settings_) ->
            case settings_ of
                Just settings ->
                    { model
                        | hasShop = FeatureLoaded settings.hasShop
                        , hasObjectives = FeatureLoaded settings.hasObjectives
                        , hasKyc = FeatureLoaded settings.hasKyc
                    }
                        |> UR.init

                Nothing ->
                    UR.init model

        CompletedLoadSettings (Err err) ->
            UR.init model
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
            Auth.update authMsg shared model.auth
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

        HideFeedbackLocal ->
            { model | feedback = Hidden }
                |> UR.init

        OpenCommunitySelector ->
            { model | showCommunitySelector = True }
                |> UR.init

        CloseCommunitySelector ->
            { model | showCommunitySelector = False }
                |> UR.init

        SelectCommunity communityId doNext ->
            { model
                | selectedCommunity = communityId
                , showCommunitySelector = False
                , searchModel =
                    Search.closeSearch shared model.searchModel
                        |> Tuple.first
                        |> (\searchModel -> { searchModel | selectedCommunity = communityId })
            }
                |> UR.init
                |> UR.addCmd (Api.Graphql.query shared (Community.settingsQuery communityId) CompletedLoadSettings)
                |> UR.addPort
                    { responseAddress = msg
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "selectedCommunity", Eos.encodeSymbol communityId )
                            , ( "name", Encode.string "setSelectedCommunity" )
                            ]
                    }
                |> UR.addCmd doNext


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


profile : Model -> Maybe Profile.Model
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
            [ "Ignored" ]

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
            [ "CompletedLoadProfile", UR.resultToString r ]

        CompletedLoadSettings r ->
            [ "CompletedLoadSettings", UR.resultToString r ]

        ClickedTryAgainProfile _ ->
            [ "ClickedTryAgainProfile" ]

        ClickedLogout ->
            [ "ClickedLogout" ]

        ShowNotificationModal _ ->
            [ "ShowNotificationModal" ]

        ShowUserNav _ ->
            [ "ShowUserNav" ]

        ShowMainNav _ ->
            [ "ShowMainNav" ]

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

        SelectCommunity _ _ ->
            [ "SelectCommunity" ]

        HideFeedbackLocal ->
            [ "HideFeedbackLocal" ]
