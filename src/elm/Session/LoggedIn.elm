module Session.LoggedIn exposing
    ( BroadcastMsg(..)
    , External(..)
    , ExternalMsg(..)
    , Model
    , Msg(..)
    , Page(..)
    , Resource(..)
    , addFeedback
    , codeOfConductUrl
    , hasPermissions
    , init
    , initLogin
    , isAccount
    , jsAddressToMsg
    , mapExternal
    , mapMsg
    , maybeInitWith
    , msgToString
    , mutation
    , profile
    , query
    , subscriptions
    , update
    , updateExternal
    , view
    , viewFrozenAccountCard
    , withPrivateKey
    )

import Action
import Api.Graphql
import Auth
import Avatar
import Cambiatus.Enum.Language
import Cambiatus.Enum.Permission exposing (Permission)
import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.UnreadNotifications
import Cambiatus.Subscription as Subscription
import Community
import Community.News
import Contact
import Dict
import Environment
import Eos
import Eos.Account as Eos
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, br, button, div, footer, h2, img, li, nav, p, span, strong, text, ul)
import Html.Attributes exposing (alt, class, classList, disabled, src, title, type_)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel, ariaLive, role)
import Html.Events exposing (onClick, onMouseEnter)
import Http
import I18Next exposing (Translations)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Log
import Markdown
import Maybe.Extra
import Notification
import Ports
import Profile
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Search exposing (State(..))
import Session.Shared as Shared exposing (Shared, Translators)
import Task
import Time
import Translation
import UpdateResult as UR
import Utils
import Version exposing (Version)
import View.Components
import View.Feedback as Feedback
import View.Modal as Modal



-- INIT


{-| Initialize already logged in user when the page is [re]loaded.
-}
init : Shared -> Eos.Name -> Maybe Api.Graphql.Token -> ( Model, Cmd (Msg externalMsg) )
init shared accountName authToken =
    let
        ( model, cmd ) =
            initModel shared Nothing Nothing accountName authToken
    in
    ( model
    , Cmd.batch
        [ internalQuery model (Profile.query accountName) CompletedLoadProfile
        , fetchCommunity model Nothing
        , Task.perform GotTimeInternal Time.now
        , cmd
        , case authToken of
            Nothing ->
                -- Socket is created automatically when we generate an auth token
                Cmd.none

            Just token ->
                Api.Graphql.createAbsintheSocket token
        ]
    )


fetchCommunity : Model -> Maybe Eos.Symbol -> Cmd (Msg externalMsg)
fetchCommunity model maybeSymbol =
    if model.shared.useSubdomain then
        internalQuery model
            (Community.subdomainQuery (Environment.communityDomain model.shared.url))
            CompletedLoadCommunity

    else
        let
            symbol =
                Maybe.Extra.or maybeSymbol model.shared.selectedCommunity
                    |> Maybe.withDefault Eos.cambiatusSymbol
        in
        internalQuery model (Community.symbolQuery symbol) CompletedLoadCommunity


fetchTranslations : Version -> Translation.Language -> Cmd (Msg externalMsg)
fetchTranslations version language =
    CompletedLoadTranslation language
        |> Translation.get version language


sendPreferredLanguage : Model -> Translation.Language -> Cmd (Msg externalMsg)
sendPreferredLanguage model language =
    let
        languageEnum =
            case language of
                Translation.Amharic ->
                    Just Cambiatus.Enum.Language.Amheth

                Translation.English ->
                    Just Cambiatus.Enum.Language.Enus

                Translation.Spanish ->
                    Just Cambiatus.Enum.Language.Eses

                Translation.Portuguese ->
                    Just Cambiatus.Enum.Language.Ptbr

                Translation.Catalan ->
                    -- We don't support catalan on the backend (See frontend issue #672)
                    Nothing
    in
    case languageEnum of
        Nothing ->
            Cmd.none

        Just languageArg ->
            internalMutation model
                (Cambiatus.Mutation.preference
                    (\optionals -> { optionals | language = OptionalArgument.Present languageArg })
                    (Graphql.SelectionSet.succeed ())
                )
                CompletedSendingLanguagePreference


{-| Initialize logged in user after signing-in.
-}
initLogin : Shared -> String -> Maybe Eos.PrivateKey -> Profile.Model -> Api.Graphql.Token -> ( Model, Cmd (Msg externalMsg) )
initLogin shared pin maybePrivateKey_ profile_ authToken =
    let
        loadedProfile =
            Just profile_
                |> RemoteData.Success
                |> Task.succeed
                |> Task.perform CompletedLoadProfile

        ( model, cmd ) =
            initModel shared (Just pin) maybePrivateKey_ profile_.account (Just authToken)
    in
    ( model
    , Cmd.batch
        [ loadedProfile
        , fetchCommunity model Nothing
        , Task.perform GotTimeInternal Time.now
        , cmd
        , Api.Graphql.createAbsintheSocket authToken
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub (Msg externalMsg)
subscriptions model =
    Sub.batch
        [ Sub.map GotSearchMsg Search.subscriptions
        , Time.every model.updateTimeEvery GotTimeInternal
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
    , updateTimeEvery : Float
    , codeOfConductModalStatus : CodeOfConductModalStatus
    , hasAcceptedCodeOfConduct : Bool
    , routeHistory : List Route
    , accountName : Eos.Name
    , profile : RemoteData (Graphql.Http.Error (Maybe Profile.Model)) Profile.Model
    , showInsufficientPermissionsModal : Bool
    , selectedCommunity : RemoteData (Graphql.Http.Error (Maybe Community.Model)) Community.Model
    , contributionCount : RemoteData (Graphql.Http.Error (Maybe Int)) Int
    , showUserNav : Bool
    , showLanguageItems : Bool
    , showNotificationModal : Bool
    , showMainNav : Bool
    , showCommunityContactsModal : Bool
    , notification : Notification.Model
    , unreadCount : Int
    , showAuthModal : Bool
    , auth : Auth.Model
    , showCommunitySelector : Bool
    , feedback : Feedback.Model
    , searchModel : Search.Model
    , claimingAction : Action.ClaimingStatus
    , authToken : Maybe Api.Graphql.Token
    , hasSeenDashboard : Bool
    , queuedCommunityFields : List Community.Field
    , maybeHighlightedNews : Maybe Community.News.Model
    , isGeneratingAuthToken : Bool
    , needsToCreateAbsintheSocket : Bool
    }


type CodeOfConductModalStatus
    = CodeOfConductNotShown
    | CodeOfConductShown
    | CodeOfConductShownWithWarning { hasCompletedAnimation : Bool }


initModel : Shared -> Maybe String -> Maybe Eos.PrivateKey -> Eos.Name -> Maybe Api.Graphql.Token -> ( Model, Cmd (Msg externalMsg) )
initModel shared lastKnownPin maybePrivateKey_ accountName authToken =
    let
        ( authModel, authCmd ) =
            Auth.init lastKnownPin shared.pinVisibility maybePrivateKey_
    in
    ( { shared = shared
      , updateTimeEvery = 60 * 1000
      , codeOfConductModalStatus = CodeOfConductNotShown
      , hasAcceptedCodeOfConduct = True
      , routeHistory = []
      , accountName = accountName
      , profile = RemoteData.Loading
      , showInsufficientPermissionsModal = False
      , selectedCommunity = RemoteData.Loading
      , contributionCount = RemoteData.NotAsked
      , showUserNav = False
      , showLanguageItems = False
      , showNotificationModal = False
      , showMainNav = False
      , showCommunityContactsModal = False
      , showCommunitySelector = False
      , showAuthModal = False
      , auth = authModel
      , notification = Notification.init
      , unreadCount = 0
      , feedback = Feedback.Hidden
      , searchModel = Search.init
      , claimingAction = Action.notClaiming
      , authToken = authToken
      , hasSeenDashboard = False
      , queuedCommunityFields = []
      , maybeHighlightedNews = Nothing
      , isGeneratingAuthToken = False
      , needsToCreateAbsintheSocket = Maybe.Extra.isNothing authToken
      }
    , Cmd.map GotAuthMsg authCmd
    )



-- VIEW


type Page
    = Redirect
    | NotFound
    | ComingSoon
    | Invite
    | Dashboard
    | News (Maybe Int)
    | CommunityAbout
    | CommunityObjectives
    | CommunitySettings
    | CommunitySettingsShopCategories
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
    | CommunitySettingsObjectives
    | CommunitySettingsObjectiveEditor
    | CommunitySettingsActionEditor
    | CommunitySettingsContacts
    | Claim
    | Notification
    | Settings
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
        mainView =
            if Search.isActive model.searchModel then
                case model.selectedCommunity of
                    RemoteData.Success community ->
                        [ Search.viewSearchBody
                            model
                            community.symbol
                            shared.now
                            (GotSearchMsg >> pageMsg)
                            (GotActionMsg >> pageMsg)
                            model.searchModel
                        ]

                    _ ->
                        []

            else
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
                                    && not (List.member page [ Join, Invite ])
                        in
                        if showHighlightedNews then
                            viewHighlightedNews shared.translators pageMsg news

                        else
                            text ""

                    Nothing ->
                        text ""
               )
            :: mainView
            ++ [ viewFooter model
                    |> Html.map pageMsg
               , viewCommunityContactsModal model
                    |> Html.map pageMsg
               , Action.viewClaimModal model.shared model.claimingAction
                    |> Html.map (GotActionMsg >> pageMsg)
               , viewAuthModal pageMsg model
               , communitySelectorModal model
                    |> Html.map pageMsg
               , insufficientPermissionsModal model
                    |> Html.map pageMsg
               , codeOfConductModal model
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


viewCommunityContactsModal : Model -> Html (Msg pageMsg)
viewCommunityContactsModal model =
    case model.selectedCommunity of
        RemoteData.Success community ->
            let
                { t } =
                    model.shared.translators
            in
            Modal.initWith
                { closeMsg = ClosedCommunityContactsModal
                , isVisible = model.showCommunityContactsModal
                }
                |> Modal.withHeader (t "footer.need_help")
                |> Modal.withBody
                    [ p [] [ text <| t "footer.choose_contact" ]
                    , ul [ class "flex flex-wrap gap-6 items-start justify-center mt-6 mb-10" ]
                        (community.contacts
                            |> List.map
                                (\contact ->
                                    li [ class "w-10" ]
                                        [ a
                                            [ class "flex flex-col items-center hover:opacity-60"
                                            , Html.Attributes.href (Contact.toHref contact)
                                            , Html.Attributes.target "blank"
                                            , title (Contact.toLabel model.shared.translators contact)
                                            , onClick ClosedCommunityContactsModal
                                            ]
                                            [ Contact.circularIconWithGrayBg [ class "w-10 h-10" ]
                                                model.shared.translators
                                                contact
                                            , span [ class "text-gray-900 text-sm text-center mt-2 font-semibold line-clamp-2" ]
                                                [ text (Contact.toLabel model.shared.translators contact) ]
                                            ]
                                        ]
                                )
                        )
                    ]
                |> Modal.toHtml

        _ ->
            text ""


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


viewFrozenAccountCard : Translation.Translators -> { onClick : msg, isHorizontal : Bool } -> List (Html.Attribute msg) -> Html msg
viewFrozenAccountCard { t } options attrs =
    div
        (class "bg-white rounded py-10 px-4"
            :: classList [ ( "md:flex md:space-x-6 lg:max-w-2xl md:py-6 md:px-10", options.isHorizontal ) ]
            :: attrs
        )
        [ img
            [ src "/images/girl-with-ice-cube.svg"
            , alt ""
            , class "mx-auto mb-8"
            , classList [ ( "md:mb-0", options.isHorizontal ) ]
            ]
            []
        , div [ class "flex flex-col" ]
            [ h2 [ class "font-bold text-black text-lg mb-6" ]
                [ text <| t "account_frozen.title" ]
            , p [ class "text-black mb-3" ]
                [ text <| t "account_frozen.why" ]
            , p []
                [ text <| t "account_frozen.solve"
                , a
                    [ class "text-orange-300 hover:underline focus-ring focus-visible:ring-orange-300 focus-visible:ring-opacity-30 rounded-sm"
                    , Html.Attributes.href "#"
                    , onClick options.onClick
                    ]
                    [ text <| t "account_frozen.solve_link" ]
                ]
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
            , Join
            ]

        viewKycRestriction =
            div [ class "mx-auto container max-w-sm" ]
                [ div [ class "my-6 mx-4 text-center" ]
                    [ p [ class "text-2xl font-bold" ]
                        [ text (t "community.kyc.restriction.title") ]
                    , p [ class "mt-2 mb-6" ]
                        [ text (t "community.kyc.restriction.description") ]
                    , a
                        [ class "button button-primary m-auto w-full"
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
            [ class "flex items-center justify-end space-x-8 my-auto flex-shrink-0"
            , classList [ ( "md:flex-shrink md:w-full", not isSearchOpen ) ]
            ]
            [ a
                [ class "relative rounded-sm group focus-ring focus-visible:ring-orange-300 focus-visible:ring-opacity-50"
                , Route.href Route.Notification
                , classList [ ( "mr-4", model.unreadCount > 0 ) ]
                , onClick ClosedSearch
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
                    , onClick ClosedSearch
                    ]
                    [ Icons.settings "fill-current h-6 text-gray-900 md:h-7 group-hover:text-orange-300" ]

              else
                text ""
            , div [ class "relative z-50 lg:min-w-50" ]
                [ button
                    [ class "z-10 pt-3 -mt-3 px-3 relative rounded-t hidden lg:w-full lg:visible lg:flex lg:items-center lg:bg-white lg:focus-ring lg:focus-visible:ring-orange-300 lg:focus-visible:ring-opacity-50"
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
                    [ class "z-10 flex relative rounded focus-ring focus-visible:ring-orange-300 focus-visible:ring-opacity-50 focus-visible:ring-offset-4 lg:hidden"
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
                    let
                        menuItemClass =
                            "flex w-full py-4 lg:py-2 pl-2.5 pr-8 lg:pr-2 justify-start items-center text-base lg:text-sm focus-ring rounded first:rounded-t lg:first:rounded-t last:rounded-b hover:bg-gray-200 focus-visible:bg-gray-200 transition-colors"

                        menuIcon icon =
                            div [ class "w-6 md:w-7 lg:w-8 flex items-center justify-center mr-2" ]
                                [ icon "h-5 lg:h-4 fill-current" ]
                    in
                    View.Components.focusTrap { initialFocusId = Nothing }
                        []
                        [ nav
                            [ class "absolute right-0 lg:w-full py-1 px-1 shadow-lg bg-white rounded lg:rounded-t-none z-50" ]
                            [ a
                                [ class menuItemClass
                                , Route.href (Route.Profile model.accountName)
                                , onClick ClickedNavLink
                                ]
                                [ menuIcon Icons.profile
                                , text_ "menu.profile"
                                ]
                            , a
                                [ class menuItemClass
                                , Route.href Route.Settings
                                , onClick ClickedNavLink
                                ]
                                [ menuIcon Icons.settings
                                , text_ "settings.account.title"
                                ]
                            , button
                                [ class menuItemClass
                                , onClick ToggleLanguageItems
                                ]
                                [ menuIcon Icons.languages
                                , text_ "menu.languages"
                                ]
                            , if model.showLanguageItems then
                                div [ class "ml-4 mb-2" ]
                                    (button
                                        [ class menuItemClass
                                        , class "uppercase !rounded !text-sm lg:!text-xs"
                                        ]
                                        [ Shared.langFlag [ class "w-4 h-4 mr-2" ] shared.language
                                        , text (Translation.languageToLanguageCode shared.language)
                                        ]
                                        :: Shared.viewLanguageItems
                                            { containerAttrs =
                                                [ class menuItemClass
                                                , class "uppercase !rounded !text-sm lg:!text-xs"
                                                ]
                                            , flagIconAttrs = [ class "w-4 h-4 mr-2" ]
                                            }
                                            shared
                                            ClickedLanguage
                                    )

                              else
                                text ""
                            , button
                                [ class menuItemClass
                                , class "text-red hover:bg-red/10 focus:bg-red/10"
                                , onClick ClickedLogout
                                ]
                                [ menuIcon Icons.close
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


insufficientPermissionsModal : Model -> Html (Msg externalMsg)
insufficientPermissionsModal model =
    let
        { t } =
            model.shared.translators
    in
    Modal.initWith
        { closeMsg = ClosedInsufficientPermissionsModal
        , isVisible = model.showInsufficientPermissionsModal
        }
        |> Modal.withHeader (t "permissions.insufficient.title")
        |> Modal.withBody
            [ img
                [ src "/images/girl-with-ice-cube.svg"
                , alt ""
                , class "mx-auto mt-4 mb-6"
                ]
                []
            , p [ class "md:max-w-md md:mx-auto md:text-center" ]
                [ text <| t "permissions.insufficient.explanation" ]
            , p [ class "my-4 md:max-w-md md:mx-auto md:text-center" ]
                [ text <| t "permissions.insufficient.try_again" ]
            ]
        |> Modal.withFooter
            [ button
                [ class "button button-primary w-full mt-2"
                , onClick ClosedInsufficientPermissionsModal
                ]
                [ text <| t "permissions.insufficient.ok" ]
            ]
        |> Modal.withSize Modal.Large
        |> Modal.toHtml


codeOfConductModal : Model -> Html (Msg externalMsg)
codeOfConductModal model =
    let
        { t, tr } =
            model.shared.translators

        needsToWaitBeforeDenyingAgain =
            case model.codeOfConductModalStatus of
                CodeOfConductShownWithWarning { hasCompletedAnimation } ->
                    not hasCompletedAnimation

                _ ->
                    False
    in
    Modal.initWith
        { closeMsg = ClickedDenyCodeOfConduct
        , isVisible = model.codeOfConductModalStatus /= CodeOfConductNotShown
        }
        |> Modal.withHeader (tr "terms_of_conduct.title" [ ( "version", codeOfConductVersion ) ])
        |> Modal.withBody
            (case model.codeOfConductModalStatus of
                CodeOfConductNotShown ->
                    []

                CodeOfConductShown ->
                    [ p [ class "mt-4" ] [ text <| t "terms_of_conduct.description" ]
                    , br [] []
                    , p [] [ text <| t "terms_of_conduct.to_have_good_experience" ]
                    , br [] []
                    , p [ class "mb-6" ]
                        [ a
                            [ Html.Attributes.href (codeOfConductUrl model.shared.language)
                            , Html.Attributes.target "_blank"
                            , class "text-orange-300 hover:underline focus-visible:underline focus-visible:outline-none rounded-sm"
                            ]
                            [ text <| tr "terms_of_conduct.link" [ ( "version", codeOfConductVersion ) ] ]
                        ]
                    ]

                CodeOfConductShownWithWarning _ ->
                    [ p [ class "text-red mt-4" ]
                        [ strong [ class "uppercase" ] [ text <| t "terms_of_conduct.attention" ]
                        , text <| t "terms_of_conduct.not_accepting"
                        ]
                    , br [] []
                    , p [] [ text <| tr "terms_of_conduct.to_participate" [ ( "version", codeOfConductVersion ) ] ]
                    , br [] []
                    , p [] [ text <| t "terms_of_conduct.are_you_sure" ]
                    , br [] []
                    , p [ class "mb-6" ]
                        [ a
                            [ Html.Attributes.href (codeOfConductUrl model.shared.language)
                            , Html.Attributes.target "_blank"
                            , class "text-orange-300 hover:underline"
                            ]
                            [ text <| tr "terms_of_conduct.link" [ ( "version", codeOfConductVersion ) ] ]
                        ]
                    ]
            )
        |> Modal.withFooter
            [ div [ class "w-full grid gap-4 md:grid-cols-2" ]
                [ button
                    [ onClick ClickedAcceptCodeOfConduct
                    , class "button button-primary w-full"
                    ]
                    [ text <| t "terms_of_conduct.accept" ]
                , button
                    [ onClick ClickedDenyCodeOfConduct
                    , class "button button-secondary !bg-white w-full relative overflow-hidden"
                    , disabled needsToWaitBeforeDenyingAgain
                    ]
                    [ case model.codeOfConductModalStatus of
                        CodeOfConductShownWithWarning _ ->
                            div
                                [ class "absolute top-0 left-0 bottom-0 bg-gray-500 w-full origin-left animate-scale-down"
                                , Html.Events.on "animationend" (Decode.succeed EndedCodeOfConductWarningAnimation)
                                ]
                                []

                        _ ->
                            text ""
                    , span [ class "z-10" ] [ text <| t "terms_of_conduct.deny" ]
                    ]
                ]
            ]
        |> Modal.toHtml


viewMainMenu : Page -> Model -> Html (Msg externalMsg)
viewMainMenu page model =
    let
        menuItem title route =
            a
                [ class "text-center uppercase py-2 hover:text-orange-300 focus-ring focus-visible:ring-orange-300 focus-visible:ring-opacity-50 rounded-sm"
                , classList
                    [ ( "text-orange-300 font-bold", isActive page route )
                    , ( "text-gray-900", not (isActive page route) )
                    ]
                , Route.href route
                ]
                [ text (model.shared.translators.t title) ]

        hasShop =
            model.selectedCommunity
                |> RemoteData.map .hasShop
                |> RemoteData.withDefault False

        isInDashboard =
            isActive page Route.Dashboard

        isInShop =
            isActive page (Route.Shop { owner = Nothing, categories = [] })
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
            menuItem "menu.shop" (Route.Shop { owner = Nothing, categories = [] })

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
        , CommunitySettingsObjectiveEditor
        , CommunitySettingsActionEditor
        ]


viewFooter : Model -> Html (Msg externalMsg)
viewFooter ({ shared } as model) =
    let
        { t, tr } =
            shared.translators

        viewContactSupport =
            case model.selectedCommunity of
                RemoteData.Success community ->
                    if List.isEmpty community.contacts then
                        text ""

                    else
                        let
                            linkContainer : List (Html.Attribute (Msg externalMsg)) -> List (Html (Msg externalMsg)) -> Html (Msg externalMsg)
                            linkContainer attrs =
                                case community.contacts of
                                    [ contact ] ->
                                        a
                                            (Html.Attributes.href (Contact.toHref contact)
                                                :: Html.Attributes.target "blank"
                                                :: attrs
                                            )

                                    _ ->
                                        button (onClick ClickedOpenCommunityContactsModal :: attrs)
                        in
                        div []
                            [ p [] [ text <| t "footer.need_help" ]
                            , linkContainer [ class "text-orange-300 hover:underline text-left" ]
                                [ text <| t "footer.contact_support" ]
                            ]

                _ ->
                    text ""
    in
    footer
        [ class "bg-white w-full border-t border-grey-500 relative overflow-hidden"
        , role "contentinfo"
        ]
        [ div [ class "container mx-auto px-4 pb-6 pt-12 flex flex-col lg:flex-row lg:py-4 lg:relative lg:overflow-hidden" ]
            [ div [ class "flex flex-col lg:flex-row lg:items-center lg:flex-grow after:h-px after:w-1/4 after:mx-auto after:mt-12 after:bg-gray-500 lg:after:w-px lg:after:h-full lg:after:mr-8 lg:after:mt-0 lg:after:ml-auto" ]
                [ p [ class "sr-only" ] [ text <| t "footer.created_with_love" ]
                , div [ class "flex items-center lg:flex-shrink-0" ]
                    [ p
                        [ class "text-center flex justify-center items-center"
                        , ariaHidden True
                        ]
                        [ span [] [ text <| t "footer.created_with" ]
                        , Icons.heartSolid "w-7"
                        , span [] [ text <| t "footer.created_by" ]
                        ]
                    , div [ class "flex items-center ml-8" ]
                        [ img
                            [ class "h-8"
                            , src "/images/logo-cambiatus-mobile.svg"
                            , alt ""
                            ]
                            []
                        , img
                            [ class "h-12 ml-8"
                            , src "/images/satisfied-vagabonds.svg"
                            , alt ""
                            ]
                            []
                        ]
                    ]
                , a
                    [ Html.Attributes.href (codeOfConductUrl shared.language)
                    , Html.Attributes.target "_blank"
                    , class "text-orange-300 mt-6 hover:underline lg:mt-0 lg:ml-10 lg:mr-4"
                    ]
                    [ text <| tr "terms_of_conduct.title" [ ( "version", codeOfConductVersion ) ] ]
                ]
            , div [ class "mt-4 lg:mt-0 lg:flex-shrink-0" ]
                [ div [ class "mr-32 md:mr-20 lg:mr-52 md:pt-6" ]
                    [ viewContactSupport
                    , p [ class "text-xs mt-6 text-gray-900" ]
                        [ text <| tr "footer.version" [ ( "version", Version.toString shared.version ) ] ]
                    ]
                , img
                    [ src "/images/man-with-envelope.svg"
                    , alt ""
                    , class "pointer-events-none absolute w-40 -bottom-12 -right-8 lg:right-4 md:-bottom-13 md:w-44"
                    ]
                    []
                ]
            ]
        ]


codeOfConductUrl : Translation.Language -> String
codeOfConductUrl language =
    let
        defaultEndpoint =
            "/code-of-conduct"

        baseUrl =
            "https://www.cambiatus.com"

        endpoint =
            case language of
                Translation.English ->
                    defaultEndpoint

                Translation.Portuguese ->
                    "/pt-br/codigo-de-conduta"

                Translation.Spanish ->
                    "/es/codigo-de-conducta"

                Translation.Catalan ->
                    defaultEndpoint

                Translation.Amharic ->
                    defaultEndpoint
    in
    baseUrl ++ endpoint



-- UPDATE


{-| Messages that pages can fire and LoggedIn will react to
-}
type External msg
    = UpdatedLoggedIn Model
    | DropFromRouteHistoryWhile (Route -> Bool)
    | ShowInsufficientPermissionsModal
    | AddedCommunity Profile.CommunityInfo
    | ExternalBroadcast BroadcastMsg
    | ReloadResource Resource
    | RequestedReloadCommunityField Community.Field
    | RequestedCommunityField Community.Field
    | SetCommunityField Community.FieldValue
    | RequiredPrivateKey { successMsg : msg, errorMsg : msg }
    | RequiredAuthToken { callbackCmd : Api.Graphql.Token -> Cmd msg }
    | RequestQuery (Cmd (Result { callbackCmd : Model -> Api.Graphql.Token -> Cmd msg } msg))
    | ShowFeedback Feedback.Status String
    | HideFeedback
    | ShowCodeOfConductModal
    | ExternalActionMsg Action.Msg
    | ChangedPin String


{-| Perform a GraphQL query. This function is preferred over `Api.Graphql.query`
for logged in users because it automatically detects if the user's auth token is
valid. If it's not valid, it automatically generates a new one (might need to ask
for user's pin), and runs the original query again.

It only retries once though, so if there are multiple authentication errors for
the same query, we stop trying, and send the error to the page that requested
the query. If the error is not related to authentication, we don't retry, we
just send the error to the page that requested the query.

-}
query :
    Model
    -> SelectionSet result RootQuery
    -> (RemoteData (Graphql.Http.Error result) result -> msg)
    -> External msg
query model selectionSet toMsg =
    graphqlOperation Api.Graphql.loggedInQuery model selectionSet toMsg


{-| Perform a GraphQL mutation. This function is preferred over `Api.Graphql.mutation`
for logged in users because it automatically detects if the user's auth token is
valid. If it's not valid, it automatically generates a new one (might need to ask
for user's pin), and runs the original query again.

It only retries once though, so if there are multiple authentication errors for
the same mutation, we stop trying, and send the error to the page that requested
the query. If the error is not related to authentication, we don't retry, we
just send the error to the page that requested the query.

-}
mutation :
    Model
    -> SelectionSet result RootMutation
    -> (RemoteData (Graphql.Http.Error result) result -> msg)
    -> External msg
mutation model selectionSet toMsg =
    graphqlOperation Api.Graphql.loggedInMutation model selectionSet toMsg


graphqlOperation :
    (Model
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
    let
        operationCmd : Model -> Api.Graphql.Token -> Cmd (RemoteData (Graphql.Http.Error result) result)
        operationCmd loggedIn authToken =
            operation loggedIn
                (Just authToken)
                selectionSet
                identity

        treatAuthError : RemoteData (Graphql.Http.Error result) result -> Result { callbackCmd : Model -> Api.Graphql.Token -> Cmd msg } msg
        treatAuthError operationResult =
            case operationResult of
                RemoteData.Success success ->
                    Ok (toMsg (RemoteData.Success success))

                RemoteData.Failure err ->
                    if Api.Graphql.isAuthError err then
                        Err
                            { callbackCmd =
                                \newModel ->
                                    operationCmd newModel
                                        >> Cmd.map toMsg
                            }

                    else
                        Ok (toMsg (RemoteData.Failure err))

                _ ->
                    Ok (toMsg operationResult)
    in
    case model.authToken of
        Nothing ->
            RequiredAuthToken
                { callbackCmd =
                    operationCmd model
                        >> Cmd.map toMsg
                }

        Just authToken ->
            operationCmd model authToken
                |> Cmd.map treatAuthError
                |> RequestQuery


{-| Perform a GraphQL query. This function is preferred over `Api.Graphql.query`
for logged in users because it automatically detects if the user's auth token is
valid. If it's not valid, it automatically generates a new one (might need to ask
for user's pin), and runs the original query again.

It only retries once though, so if there are multiple authentication errors for
the same query, we stop trying, and send the error to the page that requested
the query. If the error is not related to authentication, we don't retry, we
just send the error to the page that requested the query.

-}
internalQuery :
    Model
    -> SelectionSet result RootQuery
    -> (RemoteData (Graphql.Http.Error result) result -> Msg externalMsg)
    -> Cmd (Msg externalMsg)
internalQuery model selectionSet toMsg =
    internalGraphqlOperation Api.Graphql.loggedInQuery model selectionSet toMsg


internalMutation :
    Model
    -> SelectionSet result RootMutation
    -> (RemoteData (Graphql.Http.Error result) result -> Msg externalMsg)
    -> Cmd (Msg externalMsg)
internalMutation model selectionSet toMsg =
    internalGraphqlOperation Api.Graphql.loggedInMutation model selectionSet toMsg


internalGraphqlOperation :
    (Model
     -> Maybe Api.Graphql.Token
     -> SelectionSet result typeLock
     -> (rawOperationResult -> rawOperationResult)
     -> Cmd (RemoteData (Graphql.Http.Error result) result)
    )
    -> Model
    -> SelectionSet result typeLock
    -> (RemoteData (Graphql.Http.Error result) result -> Msg externalMsg)
    -> Cmd (Msg externalMsg)
internalGraphqlOperation operation model selectionSet toMsg =
    let
        operationCmd : Api.Graphql.Token -> Cmd (RemoteData (Graphql.Http.Error result) result)
        operationCmd authToken =
            operation model
                (Just authToken)
                selectionSet
                identity

        treatAuthError : RemoteData (Graphql.Http.Error result) result -> Result (Api.Graphql.Token -> Cmd (Msg externalMsg)) (Msg externalMsg)
        treatAuthError operationResult =
            case operationResult of
                RemoteData.Success success ->
                    Ok (toMsg (RemoteData.Success success))

                RemoteData.Failure err ->
                    if Api.Graphql.isAuthError err then
                        Err
                            (\newAuthToken ->
                                operationCmd newAuthToken
                                    |> Cmd.map toMsg
                            )

                    else
                        Ok (toMsg operationResult)

                _ ->
                    Ok (toMsg operationResult)
    in
    case model.authToken of
        Nothing ->
            (operationCmd >> Cmd.map toMsg)
                |> RequestedNewAuthTokenPhrase
                |> Utils.spawnMessage

        Just authToken ->
            operationCmd authToken
                |> Cmd.map (treatAuthError >> RequestedQueryInternal)


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

        CompletedSendingLanguagePreference result ->
            CompletedSendingLanguagePreference result

        ClosedAuthModal ->
            ClosedAuthModal

        ClosedInsufficientPermissionsModal ->
            ClosedInsufficientPermissionsModal

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

        ClickedNavLink ->
            ClickedNavLink

        ClosedSearch ->
            ClosedSearch

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

        RequestedNewAuthTokenPhrase callback ->
            RequestedNewAuthTokenPhrase (callback >> Cmd.map (mapMsg mapFn))

        RequestedNewAuthTokenPhraseExternal callback ->
            RequestedNewAuthTokenPhraseExternal (callback >> Cmd.map mapFn)

        GotAuthTokenPhrase callback result ->
            GotAuthTokenPhrase (callback >> Cmd.map (mapMsg mapFn)) result

        GotAuthTokenPhraseExternal callback result ->
            GotAuthTokenPhraseExternal (callback >> Cmd.map mapFn) result

        SignedAuthTokenPhrase password ->
            SignedAuthTokenPhrase password

        CompletedGeneratingAuthToken result ->
            CompletedGeneratingAuthToken result

        RequestedQuery result ->
            RequestedQuery
                (case result of
                    Err { callbackCmd } ->
                        { callbackCmd =
                            \shared authToken ->
                                callbackCmd shared authToken
                                    |> Cmd.map mapFn
                        }
                            |> Err

                    Ok extMsg ->
                        mapFn extMsg
                            |> Ok
                )

        RequestedQueryInternal result ->
            RequestedQueryInternal
                (case result of
                    Err callbackCmd ->
                        Err (callbackCmd >> Cmd.map (mapMsg mapFn))

                    Ok resultMsg ->
                        Ok (mapMsg mapFn resultMsg)
                )

        ClickedAcceptCodeOfConduct ->
            ClickedAcceptCodeOfConduct

        ClickedDenyCodeOfConduct ->
            ClickedDenyCodeOfConduct

        CompletedAcceptingCodeOfConduct result ->
            CompletedAcceptingCodeOfConduct result

        EndedCodeOfConductWarningAnimation ->
            EndedCodeOfConductWarningAnimation

        ClickedOpenCommunityContactsModal ->
            ClickedOpenCommunityContactsModal

        ClosedCommunityContactsModal ->
            ClosedCommunityContactsModal


mapExternal : (msg -> otherMsg) -> External msg -> External otherMsg
mapExternal mapFn msg =
    case msg of
        UpdatedLoggedIn model ->
            UpdatedLoggedIn model

        DropFromRouteHistoryWhile dropFn ->
            DropFromRouteHistoryWhile dropFn

        ShowInsufficientPermissionsModal ->
            ShowInsufficientPermissionsModal

        AddedCommunity communityInfo ->
            AddedCommunity communityInfo

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
                                        \model authToken ->
                                            callbackCmd model authToken
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

        ShowCodeOfConductModal ->
            ShowCodeOfConductModal

        ExternalActionMsg subMsg ->
            ExternalActionMsg subMsg

        ChangedPin newPin ->
            ChangedPin newPin


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

        DropFromRouteHistoryWhile dropFn ->
            { defaultResult | model = { model | routeHistory = List.dropWhile dropFn model.routeHistory } }

        ShowInsufficientPermissionsModal ->
            { defaultResult | model = { model | showInsufficientPermissionsModal = True } }

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
                    model.selectedCommunity
                        |> RemoteData.map .symbol
                        |> RemoteData.toMaybe
                        |> loadCommunity model
                        |> Tuple.second
            }

        ReloadResource ProfileResource ->
            { defaultResult
                | cmd =
                    internalQuery model
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
                            Just fieldValue ->
                                { defaultResult
                                    | broadcastMsg =
                                        Just
                                            (CommunityFieldLoaded community fieldValue)
                                }

                            Nothing ->
                                { defaultResult
                                    | cmd =
                                        internalQuery model
                                            (Community.fieldSelectionSet community.symbol field)
                                            (CompletedLoadCommunityField community)
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
                            internalQuery model
                                (Community.fieldSelectionSet community.symbol field)
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

        RequiredAuthToken { callbackCmd } ->
            { defaultResult | cmd = Utils.spawnMessage (RequestedNewAuthTokenPhraseExternal callbackCmd) }

        RequestQuery queryCmd ->
            { defaultResult | cmd = Cmd.map RequestedQuery queryCmd }

        ShowFeedback status message ->
            { defaultResult | model = { model | feedback = Feedback.Visible status message } }

        HideFeedback ->
            { defaultResult | model = { model | feedback = Feedback.Hidden } }

        ShowCodeOfConductModal ->
            { defaultResult | model = { model | codeOfConductModalStatus = CodeOfConductShown } }

        ExternalActionMsg subMsg ->
            { defaultResult | cmd = Utils.spawnMessage (GotActionMsg subMsg) }

        ChangedPin newPin ->
            { defaultResult | model = { model | auth = Auth.changeLastKnownPin newPin model.auth } }


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
    | CompletedSendingLanguagePreference (RemoteData (Graphql.Http.Error (Maybe ())) (Maybe ()))
    | ClosedAuthModal
    | ClosedInsufficientPermissionsModal
    | GotAuthMsg Auth.Msg
    | CompletedLoadUnread Value
    | OpenCommunitySelector
    | CloseCommunitySelector
    | SelectedCommunity Profile.CommunityInfo
    | GotFeedbackMsg Feedback.Msg
    | GotSearchMsg Search.Msg
    | GotActionMsg Action.Msg
    | ClickedNavLink
    | ClosedSearch
    | GotTimeInternal Time.Posix
    | CompletedLoadContributionCount (RemoteData (Graphql.Http.Error (Maybe Int)) (Maybe Int))
    | ClickedReadHighlightedNews
    | ClosedHighlightedNews
    | ReceivedNewHighlightedNews Value
    | RequestedNewAuthTokenPhrase (Api.Graphql.Token -> Cmd (Msg externalMsg))
    | RequestedNewAuthTokenPhraseExternal (Api.Graphql.Token -> Cmd externalMsg)
    | GotAuthTokenPhrase (Api.Graphql.Token -> Cmd (Msg externalMsg)) (RemoteData (Graphql.Http.Error Api.Graphql.Phrase) Api.Graphql.Phrase)
    | GotAuthTokenPhraseExternal (Api.Graphql.Token -> Cmd externalMsg) (RemoteData (Graphql.Http.Error Api.Graphql.Phrase) Api.Graphql.Phrase)
    | SignedAuthTokenPhrase Api.Graphql.Password
    | CompletedGeneratingAuthToken (RemoteData (Graphql.Http.Error Api.Graphql.SignInResponse) Api.Graphql.SignInResponse)
    | RequestedQuery (Result { callbackCmd : Model -> Api.Graphql.Token -> Cmd externalMsg } externalMsg)
    | RequestedQueryInternal (Result (Api.Graphql.Token -> Cmd (Msg externalMsg)) (Msg externalMsg))
    | ClickedAcceptCodeOfConduct
    | ClickedDenyCodeOfConduct
    | CompletedAcceptingCodeOfConduct (RemoteData (Graphql.Http.Error (Maybe ())) (Maybe ()))
    | EndedCodeOfConductWarningAnimation
    | ClickedOpenCommunityContactsModal
    | ClosedCommunityContactsModal


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
                , showCommunityContactsModal = False
                , showAuthModal = False
                , showInsufficientPermissionsModal = False
            }
    in
    case msg of
        NoOp ->
            UR.init model

        GotTimeInternal time ->
            UR.init { model | shared = { shared | now = time } }
                |> UR.addExt (GotTime time |> Broadcast)

        GotActionMsg subMsg ->
            Action.update subMsg model.claimingAction model
                |> UR.fromChild (\newClaimingAction -> { model | claimingAction = newClaimingAction })
                    GotActionMsg
                    (\ext ->
                        case ext of
                            Action.SetUpdateTimeEvery interval ->
                                UR.mapModel (\m -> { m | updateTimeEvery = interval })

                            Action.ShowFeedback feedbackModel ->
                                UR.mapModel (\m -> { m | feedback = feedbackModel })

                            Action.RequiredPrivateKey afterAuthMsg ->
                                UR.mapModel askedAuthentication
                                    >> UR.addExt (AddAfterPrivateKeyCallback (GotActionMsg afterAuthMsg))

                            Action.ShowInsufficientPermissionsModal ->
                                UR.mapModel (\m -> { m | showInsufficientPermissionsModal = True })
                    )
                    model

        ClickedNavLink ->
            { closeAllModals | searchModel = Search.closeSearch model.searchModel }
                |> UR.init

        ClosedSearch ->
            { model | searchModel = Search.closeSearch model.searchModel }
                |> UR.init

        GotSearchMsg searchMsg ->
            Search.update shared model.searchModel searchMsg
                |> UR.fromChild (\searchModel -> { model | searchModel = searchModel })
                    GotSearchMsg
                    (\extMsg ur ->
                        case extMsg of
                            Search.SetFeedback feedback ->
                                ur
                                    |> UR.mapModel (\newModel -> { newModel | feedback = feedback })

                            Search.RequestQuery selectionSet resultMsg ->
                                ur
                                    |> UR.addCmd
                                        (internalQuery ur.model
                                            selectionSet
                                            (resultMsg >> GotSearchMsg)
                                        )
                    )
                    { model | hasSeenDashboard = model.hasSeenDashboard || Search.isOpenMsg searchMsg }
                |> UR.mapModel
                    (\newModel -> { newModel | hasSeenDashboard = newModel.hasSeenDashboard || Search.isOpenMsg searchMsg })

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
                |> UR.addCmd (fetchTranslations shared.version shared.language)
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
                    let
                        sendLanguagePreference =
                            -- If the backend doesn't know the user's preferred
                            -- language, we send it so we can have nicer email communication
                            case p.preferredLanguage of
                                Nothing ->
                                    sendPreferredLanguage model model.shared.language

                                Just _ ->
                                    Cmd.none

                        hasAcceptedCodeOfConduct =
                            Maybe.Extra.isJust p.latestAcceptedTerms
                    in
                    { model
                        | profile = RemoteData.Success p
                        , hasAcceptedCodeOfConduct = model.hasAcceptedCodeOfConduct || hasAcceptedCodeOfConduct
                        , codeOfConductModalStatus =
                            if hasAcceptedCodeOfConduct then
                                CodeOfConductNotShown

                            else
                                CodeOfConductShown
                    }
                        |> UR.init
                        |> UR.addCmd sendLanguagePreference
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
                    internalQuery newModel
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
                    { responseAddress = NoOp
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "setFavicon" )
                            , ( "favicon", Encode.string community.logo )
                            ]
                    }
                |> UR.addPort
                    { responseAddress = ReceivedNewHighlightedNews Encode.null
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "subscribeToHighlightedNewsChanged" )
                            , ( "subscription"
                              , highlightedNewsSubscription
                                    |> Graphql.Document.serializeSubscription
                                    |> Encode.string
                              )
                            ]
                    }
                |> UR.addExt (CommunityLoaded newCommunity |> Broadcast)
                |> UR.addCmd
                    (internalQuery newModel
                        (Community.fieldsSelectionSet community.symbol newModel.queuedCommunityFields)
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
                    not (Api.Graphql.isNonExistingCommunityError e)
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
                    (internalQuery model
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
                |> UR.addCmd (fetchTranslations shared.version lang)
                |> UR.addCmd (sendPreferredLanguage model lang)

        CompletedSendingLanguagePreference (RemoteData.Failure err) ->
            model
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just model.accountName)
                    "Got an error when sending language preference"
                    { moduleName = "Session.LoggedIn", function = "update" }
                    []
                    err

        CompletedSendingLanguagePreference _ ->
            model
                |> UR.init

        ClosedAuthModal ->
            UR.init closeAllModals
                |> UR.addExt AuthenticationFailed

        ClosedInsufficientPermissionsModal ->
            { model | showInsufficientPermissionsModal = False }
                |> UR.init

        GotAuthMsg authMsg ->
            Auth.update authMsg shared model.auth
                |> UR.map
                    (\a -> { model | auth = a })
                    GotAuthMsg
                    (\extMsg uResult ->
                        case extMsg of
                            Auth.CompletedAuth accountName auth ->
                                closeModal uResult
                                    |> UR.mapModel (\m -> { m | auth = auth })
                                    |> UR.addExt AuthenticationSucceed
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
                            (Graphql.Document.decoder highlightedNewsSubscription)
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

        RequestedNewAuthTokenPhrase callback ->
            if model.isGeneratingAuthToken then
                model
                    |> UR.init
                    |> UR.addExt (AddAfterAuthTokenCallbackInternal callback)

            else
                { model | isGeneratingAuthToken = True }
                    |> UR.init
                    |> UR.addCmd
                        (Api.Graphql.askForPhrase model.shared
                            model.accountName
                            (GotAuthTokenPhrase callback)
                        )

        RequestedNewAuthTokenPhraseExternal callback ->
            if model.isGeneratingAuthToken then
                model
                    |> UR.init
                    |> UR.addExt (AddAfterAuthTokenCallback callback)

            else
                { model | isGeneratingAuthToken = True }
                    |> UR.init
                    |> UR.addCmd
                        (Api.Graphql.askForPhrase model.shared
                            model.accountName
                            (GotAuthTokenPhraseExternal callback)
                        )

        GotAuthTokenPhrase callback (RemoteData.Success phrase) ->
            (\privateKey ->
                model
                    |> UR.init
                    |> UR.addPort (Api.Graphql.signPhrasePort msg privateKey phrase)
                    |> UR.addExt (AddAfterAuthTokenCallbackInternal callback)
            )
                |> withPrivateKeyInternal msg model []

        GotAuthTokenPhrase _ (RemoteData.Failure err) ->
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

        GotAuthTokenPhraseExternal callback (RemoteData.Success phrase) ->
            (\privateKey ->
                model
                    |> UR.init
                    |> UR.addPort (Api.Graphql.signPhrasePort msg privateKey phrase)
                    |> UR.addExt (AddAfterAuthTokenCallback callback)
            )
                |> withPrivateKeyInternal msg model []

        GotAuthTokenPhraseExternal _ (RemoteData.Failure err) ->
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
                    "Got an error when fetching phrase to sign for auth token (using an external msg)"
                    { moduleName = "Session.LoggedIn"
                    , function = "update"
                    }
                    []
                    err

        GotAuthTokenPhraseExternal _ _ ->
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
            let
                createAbsintheSocket =
                    if model.needsToCreateAbsintheSocket then
                        Api.Graphql.createAbsintheSocket signInResponse.token

                    else
                        Cmd.none
            in
            { model
                | profile = RemoteData.Success signInResponse.profile
                , authToken = Just signInResponse.token
                , isGeneratingAuthToken = False
                , needsToCreateAbsintheSocket = False
            }
                |> UR.init
                |> UR.addCmd (Api.Graphql.storeToken signInResponse.token)
                |> UR.addCmd createAbsintheSocket
                |> UR.addExt (RunAfterAuthTokenCallbacks signInResponse.token)

        CompletedGeneratingAuthToken (RemoteData.Failure err) ->
            { model
                | auth = Auth.removePrivateKey model.auth
                , feedback = Feedback.Visible Feedback.Failure (shared.translators.t "auth.failed")
                , isGeneratingAuthToken = False
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

        RequestedQuery (Ok externalMsg) ->
            model
                |> UR.init
                |> UR.addExt (RunExternalMsg externalMsg)

        RequestedQuery (Err { callbackCmd }) ->
            model
                |> UR.init
                |> UR.addMsg (RequestedNewAuthTokenPhraseExternal (callbackCmd model))

        RequestedQueryInternal (Ok resultMsg) ->
            model
                |> UR.init
                |> UR.addMsg resultMsg

        RequestedQueryInternal (Err callbackCmd) ->
            model
                |> UR.init
                |> UR.addMsg (RequestedNewAuthTokenPhrase callbackCmd)

        ClickedAcceptCodeOfConduct ->
            { model | codeOfConductModalStatus = CodeOfConductNotShown }
                |> UR.init
                |> UR.addCmd
                    (internalMutation model
                        (Cambiatus.Mutation.acceptTerms (Graphql.SelectionSet.succeed ()))
                        CompletedAcceptingCodeOfConduct
                    )

        ClickedDenyCodeOfConduct ->
            case model.codeOfConductModalStatus of
                CodeOfConductNotShown ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Denied code of conduct, but modal wasn't shown"
                            (Just model.accountName)
                            { moduleName = "Session.LoggedIn", function = "update" }
                            []

                CodeOfConductShown ->
                    { model | codeOfConductModalStatus = CodeOfConductShownWithWarning { hasCompletedAnimation = False } }
                        |> UR.init

                CodeOfConductShownWithWarning { hasCompletedAnimation } ->
                    if not hasCompletedAnimation then
                        model
                            |> UR.init

                    else
                        { model
                            | hasAcceptedCodeOfConduct = False
                            , codeOfConductModalStatus = CodeOfConductNotShown
                        }
                            |> UR.init

        CompletedAcceptingCodeOfConduct (RemoteData.Failure err) ->
            { model | feedback = Feedback.Visible Feedback.Failure (shared.translators.t "terms_of_conduct.error_accepting") }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just model.accountName)
                    "Got an error when accepting code of conduct"
                    { moduleName = "Session.LoggedIn", function = "update" }
                    []
                    err

        CompletedAcceptingCodeOfConduct _ ->
            { model | hasAcceptedCodeOfConduct = True }
                |> UR.init

        EndedCodeOfConductWarningAnimation ->
            { model
                | codeOfConductModalStatus =
                    case model.codeOfConductModalStatus of
                        CodeOfConductShownWithWarning _ ->
                            CodeOfConductShownWithWarning { hasCompletedAnimation = True }

                        _ ->
                            model.codeOfConductModalStatus
            }
                |> UR.init

        ClickedOpenCommunityContactsModal ->
            { model | showCommunityContactsModal = True }
                |> UR.init

        ClosedCommunityContactsModal ->
            { model | showCommunityContactsModal = False }
                |> UR.init


{-| Checks if we already have the user's private key loaded. If it does, returns
`successfulUR`. If it doesn't, requires authentication and fires the `subMsg`
again. Necessary to perform EOS transactions
-}
withPrivateKey :
    Model
    -> List Permission
    -> subModel
    -> { successMsg : subMsg, errorMsg : subMsg }
    -> UR.UpdateResult subModel subMsg (External subMsg)
    -> UR.UpdateResult subModel subMsg (External subMsg)
withPrivateKey model requiredPermissions subModel subMsg successfulUR =
    Auth.withPrivateKey model.auth
        { requiredPermissions = requiredPermissions
        , currentPermissions =
            profile model
                |> Maybe.map (.roles >> List.concatMap .permissions)
        }
        { onAskedPrivateKey = UR.addExt (RequiredPrivateKey subMsg)
        , onInsufficientPermissions = UR.addExt ShowInsufficientPermissionsModal
        , onAbsentPermissions =
            UR.logImpossible subMsg.successMsg
                "Tried signing eos transaction, but profile wasn't loaded"
                (Just model.accountName)
                { moduleName = "Session.LoggedIn"
                , function = "withPrivateKey"
                }
                []
        , defaultModel = subModel
        }
        (\_ -> successfulUR)


{-| Determines if a profile has a set of permissions
-}
hasPermissions : Profile.Model -> List Permission -> Bool
hasPermissions profile_ permissions =
    let
        allPermissions =
            List.concatMap .permissions profile_.roles
    in
    List.all (\permission -> List.member permission allPermissions)
        permissions


withPrivateKeyInternal : Msg msg -> Model -> List Permission -> (Eos.PrivateKey -> UpdateResult msg) -> UpdateResult msg
withPrivateKeyInternal msg model requiredPermissions successfulUR =
    Auth.withPrivateKey model.auth
        { requiredPermissions = requiredPermissions
        , currentPermissions =
            profile model
                |> Maybe.map (.roles >> List.concatMap .permissions)
        }
        { onAskedPrivateKey =
            UR.mapModel askedAuthentication
                >> UR.addExt (AddAfterPrivateKeyCallback msg)
        , onInsufficientPermissions = UR.mapModel (\m -> { m | showInsufficientPermissionsModal = True })
        , onAbsentPermissions =
            UR.logImpossible msg
                "Tried signing eos transaction internally, but profile wasn't loaded"
                (Just model.accountName)
                { moduleName = "Session.LoggedIn"
                , function = "withPrivateKeyInternal"
                }
                []
        , defaultModel = model
        }
        successfulUR


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


loadCommunity : Model -> Maybe Eos.Symbol -> ( Model, Cmd (Msg externalMsg) )
loadCommunity model maybeSymbol =
    ( { model
        | showCommunitySelector = False
        , selectedCommunity = RemoteData.Loading
      }
    , fetchCommunity model maybeSymbol
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


signUpForCommunity : Model -> Profile.CommunityInfo -> ( Model, Cmd (Msg externalMsg) )
signUpForCommunity model communityInfo =
    ( { model | selectedCommunity = RemoteData.Loading }
    , internalQuery model
        (Community.symbolQuery communityInfo.symbol)
        CompletedLoadCommunity
    )


{-| Given minimal information, selects a community. This means querying for the
entire `Community.Model`, and then setting it in the `Model`
-}
selectCommunity : Model -> { community | symbol : Eos.Symbol, subdomain : String } -> Route -> ( Model, Cmd (Msg externalMsg) )
selectCommunity ({ shared } as model) community route =
    if shared.useSubdomain then
        ( model
        , Route.loadExternalCommunity shared community route
        )

    else
        ( { model | selectedCommunity = RemoteData.Loading }
        , Cmd.batch
            [ fetchCommunity model (Just community.symbol)
            , Route.replaceUrl shared.navKey route
            ]
        )


closeModal : UpdateResult msg -> UpdateResult msg
closeModal ({ model } as uResult) =
    { uResult
        | model =
            { model
                | showNotificationModal = False
                , showUserNav = False
                , showMainNav = False
                , showCommunityContactsModal = False
                , showAuthModal = False
                , showInsufficientPermissionsModal = False
            }
    }


askedAuthentication : Model -> Model
askedAuthentication model =
    { model
        | showNotificationModal = False
        , showUserNav = False
        , showMainNav = False
        , showCommunityContactsModal = False
        , showAuthModal = True
        , showInsufficientPermissionsModal = False
    }


codeOfConductVersion : String
codeOfConductVersion =
    "1.0"



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


highlightedNewsSubscription : SelectionSet (Maybe Community.News.Model) RootSubscription
highlightedNewsSubscription =
    Subscription.highlightedNews Community.News.selectionSet



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

        "GotAuthTokenPhraseExternal" :: _ ->
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

        ClickedNavLink ->
            [ "ClickedNavLink" ]

        ClosedSearch ->
            [ "ClosedSearch" ]

        GotSearchMsg _ ->
            [ "GotSearchMsg" ]

        GotActionMsg subMsg ->
            "GotActionMsg" :: Action.msgToString subMsg

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

        CompletedSendingLanguagePreference r ->
            [ "CompletedSendingLanguagePreference", UR.remoteDataToString r ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        ClosedInsufficientPermissionsModal ->
            [ "ClosedInsufficientPermissionsModal" ]

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

        RequestedNewAuthTokenPhrase _ ->
            [ "RequestedNewAuthTokenPhrase" ]

        RequestedNewAuthTokenPhraseExternal _ ->
            [ "RequestedNewAuthTokenPhraseExternal" ]

        GotAuthTokenPhrase _ r ->
            [ "GotAuthTokenPhrase", UR.remoteDataToString r ]

        GotAuthTokenPhraseExternal _ r ->
            [ "GotAuthTokenPhraseExternal", UR.remoteDataToString r ]

        SignedAuthTokenPhrase _ ->
            [ "SignedAuthTokenPhrase" ]

        CompletedGeneratingAuthToken r ->
            [ "CompletedGeneratingAuthToken", UR.remoteDataToString r ]

        RequestedQuery r ->
            [ "RequestedQuery", UR.resultToString r ]

        RequestedQueryInternal r ->
            [ "RequestedQueryInternal", UR.resultToString r ]

        ClickedAcceptCodeOfConduct ->
            [ "ClickedAcceptCodeOfConduct" ]

        ClickedDenyCodeOfConduct ->
            [ "ClickedDenyCodeOfConduct" ]

        CompletedAcceptingCodeOfConduct r ->
            [ "CompletedAcceptingCodeOfConduct", UR.remoteDataToString r ]

        EndedCodeOfConductWarningAnimation ->
            [ "EndedCodeOfConductWarningAnimation" ]

        ClickedOpenCommunityContactsModal ->
            [ "ClickedOpenCommunityContactsModal" ]

        ClosedCommunityContactsModal ->
            [ "ClosedCommunityContactsModal" ]


externalMsgToString : External msg -> List String
externalMsgToString externalMsg =
    case externalMsg of
        UpdatedLoggedIn _ ->
            [ "UpdatedLoggedIn" ]

        DropFromRouteHistoryWhile _ ->
            [ "PopFromRouteHistory" ]

        ShowInsufficientPermissionsModal ->
            [ "ShowInsufficientPermissionsModal" ]

        AddedCommunity _ ->
            [ "AddedCommunity" ]

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

        ShowCodeOfConductModal ->
            [ "ShowCodeOfConductModal" ]

        ExternalActionMsg subMsg ->
            "ExternalActionMsg" :: Action.msgToString subMsg

        ChangedPin _ ->
            [ "ChangedPin" ]
