module Main exposing (main)

import Action
import Api.Graphql
import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict
import Eos.Account
import Flags
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Log
import Page exposing (Session)
import Page.ComingSoon as ComingSoon
import Page.Community.About as CommunityAbout
import Page.Community.Invite as Invite
import Page.Community.New as CommunityEditor
import Page.Community.Objectives as CommunityObjectives
import Page.Community.Selector as CommunitySelector
import Page.Community.Settings.ActionEditor as CommunitySettingsActionEditor
import Page.Community.Settings.Contacts as CommunitySettingsContacts
import Page.Community.Settings.Currency as CommunitySettingsCurrency
import Page.Community.Settings.Features as CommunitySettingsFeatures
import Page.Community.Settings.Info as CommunitySettingsInfo
import Page.Community.Settings.News as CommunitySettingsNews
import Page.Community.Settings.News.Editor as CommunitySettingsNewsEditor
import Page.Community.Settings.ObjectiveEditor as CommunitySettingsObjectiveEditor
import Page.Community.Settings.Objectives as CommunitySettingsObjectives
import Page.Community.Settings.Settings as CommunitySettings
import Page.Community.Settings.Shop.Categories as CommunitySettingsShopCategories
import Page.Community.Settings.Sponsorship as CommunitySettingsSponsorship
import Page.Community.Settings.Sponsorship.Fiat as CommunitySettingsSponsorshipFiat
import Page.Community.Settings.Sponsorship.ThankYouMessage as CommunitySettingsSponsorshipThankYouMessage
import Page.Community.Sponsor as CommunitySponsor
import Page.Community.Supporters as CommunitySupporters
import Page.Community.ThankYou as CommunityThankYou
import Page.Community.Transfer as Transfer
import Page.Dashboard as Dashboard
import Page.Dashboard.Analysis as Analysis
import Page.Dashboard.Claim as Claim
import Page.Join as Join
import Page.Login as Login
import Page.News as News
import Page.NotFound as NotFound
import Page.Notification as Notification
import Page.PaymentHistory as PaymentHistory
import Page.Profile as Profile
import Page.Profile.AddContact as ProfileAddContact
import Page.Profile.AddKyc as ProfileAddKyc
import Page.Profile.Claims as ProfileClaims
import Page.Profile.Contributions as ProfileContributions
import Page.Profile.Editor as ProfileEditor
import Page.Register as Register
import Page.Settings as Settings
import Page.Shop as Shop
import Page.Shop.Editor as ShopEditor
import Page.Shop.Viewer as ShopViewer
import Page.ViewTransfer as ViewTransfer
import Ports
import RemoteData
import Route exposing (Route)
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Shop
import Shop.Category
import Task
import Time
import UpdateResult as UR exposing (UpdateResult)
import Url exposing (Url)
import Utils
import View.Feedback as Feedback


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- INIT


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url navKey =
    let
        ( session, pageCmd ) =
            case Decode.decodeValue Flags.decode flagsValue of
                Ok flags ->
                    Page.init flags navKey url
                        |> UR.map identity GotPageMsg (\_ uR -> uR)
                        |> UR.addBreadcrumb
                            { type_ = Log.DebugBreadcrumb
                            , category = Ignored
                            , message = "Started elm app with flags being decoded"
                            , data = Dict.empty
                            , level = Log.DebugLevel
                            }
                        |> UR.toModelCmd (\_ m -> ( m, Cmd.none )) msgToString

                Err e ->
                    Page.init Flags.default navKey url
                        |> UR.map identity GotPageMsg (\_ uR -> uR)
                        |> UR.logDecodingError Ignored
                            Nothing
                            "Could not decode flags"
                            { moduleName = "Main", function = "init" }
                            []
                            e
                        |> UR.toModelCmd (\_ m -> ( m, Cmd.none )) msgToString

        ( model, routeCmd ) =
            changeRouteTo (Route.fromUrl url)
                { session = session
                , afterAuthMsg = Nothing
                , afterAuthTokenCallbacks = []
                , afterPrivateKeyCallbacks = []
                , status = Redirect
                }
    in
    ( model
    , Cmd.batch
        [ pageCmd
        , routeCmd
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotPageMsg (Page.subscriptions model.session)
        , Ports.javascriptInPort GotJavascriptData
        , case model.status of
            CommunitySponsor subModel ->
                CommunitySponsor.subscriptions subModel
                    |> Sub.map GotCommunitySponsorMsg

            CommunitySettingsShopCategories subModel ->
                CommunitySettingsShopCategories.subscriptions subModel
                    |> Sub.map GotCommunitySettingsShopCategoriesMsg

            _ ->
                Sub.none
        ]



-- MODEL


type alias Model =
    { session : Session
    , afterAuthMsg : Maybe { successMsg : Msg, errorMsg : Msg }
    , afterAuthTokenCallbacks : List (Api.Graphql.Token -> Cmd Msg)
    , afterPrivateKeyCallbacks : List Msg
    , status : Status
    }


type Status
    = Redirect
    | NotFound
    | ComingSoon
    | PaymentHistory PaymentHistory.Model
    | CommunityAbout CommunityAbout.Model
    | CommunityObjectives CommunityObjectives.Model
    | CommunityEditor CommunityEditor.Model
    | CommunitySettings CommunitySettings.Model
    | CommunitySettingsShopCategories CommunitySettingsShopCategories.Model
    | CommunitySettingsFeatures CommunitySettingsFeatures.Model
    | CommunitySettingsInfo CommunitySettingsInfo.Model
    | CommunitySettingsNews CommunitySettingsNews.Model
    | CommunitySettingsNewsEditor CommunitySettingsNewsEditor.Model
    | CommunitySettingsCurrency CommunitySettingsCurrency.Model
    | CommunitySettingsSponsorship CommunitySettingsSponsorship.Model
    | CommunitySettingsSponsorshipFiat CommunitySettingsSponsorshipFiat.Model
    | CommunitySettingsSponsorshipThankYouMessage CommunitySettingsSponsorshipThankYouMessage.Model
    | CommunitySelector CommunitySelector.Model
    | CommunityThankYou
    | CommunitySponsor CommunitySponsor.Model
    | CommunitySupporters CommunitySupporters.Model
    | CommunitySettingsObjectives CommunitySettingsObjectives.Model
    | CommunitySettingsObjectiveEditor CommunitySettingsObjectiveEditor.Model
    | CommunitySettingsActionEditor CommunitySettingsActionEditor.Model
    | CommunitySettingsContacts CommunitySettingsContacts.Model
    | Claim Int Int Claim.Model
    | Notification Notification.Model
    | Dashboard Dashboard.Model
    | Login (Maybe Route) Login.Model
    | News News.Model
    | Profile Profile.Model
    | ProfileContributions ProfileContributions.Model
    | ProfileEditor ProfileEditor.Model
    | ProfileAddKyc ProfileAddKyc.Model
    | ProfileClaims ProfileClaims.Model
    | ProfileAddContact ProfileAddContact.Model
    | Register (Maybe String) (Maybe Route) Register.Model
    | Settings Settings.Model
    | Shop { owner : Maybe Eos.Account.Name, categories : List Shop.Category.Id } Shop.Model
    | ShopEditor (Maybe Shop.Id) ShopEditor.Model
    | ShopViewer Shop.Id ShopViewer.Model
    | ViewTransfer Int ViewTransfer.Model
    | Invite Invite.Model
    | Join Join.Model
    | Transfer Transfer.Model
    | Analysis Analysis.Model



-- UPDATE


type Msg
    = Ignored
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotJavascriptData Value
    | GotPageMsg (Page.Msg Msg)
    | GotNotificationMsg Notification.Msg
    | GotCommunityAboutMsg CommunityAbout.Msg
    | GotCommunityObjectivesMsg CommunityObjectives.Msg
    | GotCommunityEditorMsg CommunityEditor.Msg
    | GotCommunitySettingsMsg CommunitySettings.Msg
    | GotCommunitySettingsShopCategoriesMsg CommunitySettingsShopCategories.Msg
    | GotCommunitySettingsFeaturesMsg CommunitySettingsFeatures.Msg
    | GotCommunitySettingsInfoMsg CommunitySettingsInfo.Msg
    | GotCommunitySettingsNewsMsg CommunitySettingsNews.Msg
    | GotCommunitySettingsNewsEditorMsg CommunitySettingsNewsEditor.Msg
    | GotCommunitySettingsCurrencyMsg CommunitySettingsCurrency.Msg
    | GotCommunitySettingsSponsorshipMsg CommunitySettingsSponsorship.Msg
    | GotCommunitySettingsSponsorshipFiatMsg CommunitySettingsSponsorshipFiat.Msg
    | GotCommunitySettingsSponsorshipThankYouMessageMsg CommunitySettingsSponsorshipThankYouMessage.Msg
    | GotCommunitySponsorMsg CommunitySponsor.Msg
    | GotCommunitySupportersMsg CommunitySupporters.Msg
    | GotCommunitySettingsObjectivesMsg CommunitySettingsObjectives.Msg
    | GotCommunitySettingsObjectiveEditorMsg CommunitySettingsObjectiveEditor.Msg
    | GotCommunitySettingsActionEditorMsg CommunitySettingsActionEditor.Msg
    | GotCommunitySettingsContactsMsg CommunitySettingsContacts.Msg
    | GotVerifyClaimMsg Claim.Msg
    | GotDashboardMsg Dashboard.Msg
    | GotLoginMsg Login.Msg
    | GotNewsMsg News.Msg
    | GotPaymentHistoryMsg PaymentHistory.Msg
    | GotProfileMsg Profile.Msg
    | GotProfileContributionsMsg ProfileContributions.Msg
    | GotProfileEditorMsg ProfileEditor.Msg
    | GotProfileAddKycMsg ProfileAddKyc.Msg
    | GotProfileClaimsMsg ProfileClaims.Msg
    | GotProfileAddContactMsg ProfileAddContact.Msg
    | GotRegisterMsg Register.Msg
    | GotSettingsMsg Settings.Msg
    | GotShopMsg Shop.Msg
    | GotShopEditorMsg ShopEditor.Msg
    | GotShopViewerMsg ShopViewer.Msg
    | GotViewTransferScreenMsg ViewTransfer.Msg
    | GotInviteMsg Invite.Msg
    | GotJoinMsg Join.Msg
    | GotTransferMsg Transfer.Msg
    | GotAnalysisMsg Analysis.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        withGuest fn =
            case model.session of
                Page.Guest guest ->
                    fn guest

                Page.LoggedIn _ ->
                    ( model
                    , { username = Nothing
                      , message = "Expected user to be a guest, but they're logged in"
                      , tags = [ Log.IncompatibleAuthentication Log.ExpectedGuest ]
                      , location = { moduleName = "Main", function = "update" }
                      , contexts = []
                      , transaction = msg
                      , level = Log.Error
                      }
                        |> Log.send msgToString
                    )

        withLoggedIn fn =
            case model.session of
                Page.Guest _ ->
                    ( model
                    , { username = Nothing
                      , message = "Expected user to be logged in, but they're a guest"
                      , tags = [ Log.IncompatibleAuthentication Log.ExpectedLoggedIn ]
                      , location = { moduleName = "Main", function = "update" }
                      , contexts = []
                      , transaction = msg
                      , level = Log.Error
                      }
                        |> Log.send msgToString
                    )

                Page.LoggedIn loggedIn ->
                    fn loggedIn

        withSession fn =
            fn model.session
    in
    case ( msg, model.status ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model |> hideFeedback
                    , Nav.pushUrl (.navKey (Page.toShared model.session)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model |> hideFeedback
                    , Nav.load href
                    )

        ( GotJavascriptData val, _ ) ->
            let
                jsAddressResult =
                    Decode.decodeValue
                        (Decode.list Decode.string
                            |> Decode.field "address"
                        )
                        val
            in
            case jsAddressResult of
                Ok jsAddress ->
                    case jsAddressToMsg jsAddress val of
                        Nothing ->
                            ( model
                            , Log.fromImpossible msg
                                "Got invalid address from JavaScript"
                                (Page.maybeAccountName model.session)
                                { moduleName = "Main", function = "update" }
                                [ { name = "Data"
                                  , extras = Dict.fromList [ ( "address", Encode.list Encode.string jsAddress ) ]
                                  }
                                ]
                                |> Log.send msgToString
                            )

                        Just jsMsg ->
                            update jsMsg model

                Err decodeError ->
                    ( model
                    , Log.fromDecodeError msg
                        (Page.maybeAccountName model.session)
                        "Could not decode JavaScript address"
                        { moduleName = "Main", function = "update" }
                        []
                        decodeError
                        |> Log.send msgToString
                    )

        ( GotPageMsg subMsg, _ ) ->
            Page.update subMsg model.session
                |> UR.map
                    (\s -> { model | session = s })
                    GotPageMsg
                    (\extMsg uR -> UR.addExt extMsg uR)
                |> UR.toModelCmd updateExternal msgToString

        ( GotRegisterMsg subMsg, Register maybeInvitation maybeRedirect subModel ) ->
            -- Will return  a function expecting a Guest Model
            Register.update maybeInvitation subMsg subModel
                -- will return a function expecting an UpdateResult
                -- The composition operator will take the result of the above function and use as
                -- an input for the below function
                >> updateGuestUResult (Register maybeInvitation maybeRedirect) GotRegisterMsg model
                -- provides the above composed function with the initial guest input
                |> withGuest

        ( GotSettingsMsg subMsg, Settings subModel ) ->
            Settings.update subMsg subModel
                >> updateLoggedInUResult Settings GotSettingsMsg model
                |> withLoggedIn

        ( GotPaymentHistoryMsg subMsg, PaymentHistory subModel ) ->
            PaymentHistory.update subMsg subModel
                >> updateLoggedInUResult PaymentHistory GotPaymentHistoryMsg model
                |> withLoggedIn

        ( GotLoginMsg subMsg, Login maybeRedirect subModel ) ->
            Login.update subMsg subModel
                >> updateGuestUResult (Login maybeRedirect) GotLoginMsg model
                |> withGuest

        ( GotNewsMsg subMsg, News subModel ) ->
            News.update subMsg subModel
                >> updateLoggedInUResult News GotNewsMsg model
                |> withLoggedIn

        ( GotNotificationMsg subMsg, Notification subModel ) ->
            -- Will return a function expecting a LoggedIn Model
            Notification.update subMsg subModel
                -- will return a function expecting an UpdateResult
                -- The composition operator will take the result of the above function and use as
                -- an input for the below function
                >> updateLoggedInUResult Notification GotNotificationMsg model
                -- provides the above composed function with the LoggedInModel
                |> withLoggedIn

        ( GotCommunityAboutMsg subMsg, CommunityAbout subModel ) ->
            CommunityAbout.update subMsg subModel
                >> updateLoggedInUResult CommunityAbout GotCommunityAboutMsg model
                |> withLoggedIn

        ( GotCommunityObjectivesMsg subMsg, CommunityObjectives subModel ) ->
            CommunityObjectives.update subMsg subModel
                >> updateLoggedInUResult CommunityObjectives GotCommunityObjectivesMsg model
                |> withLoggedIn

        ( GotCommunityEditorMsg subMsg, CommunityEditor subModel ) ->
            CommunityEditor.update subMsg subModel
                >> updateLoggedInUResult CommunityEditor GotCommunityEditorMsg model
                |> withLoggedIn

        ( GotCommunitySettingsObjectivesMsg subMsg, CommunitySettingsObjectives subModel ) ->
            CommunitySettingsObjectives.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsObjectives GotCommunitySettingsObjectivesMsg model
                |> withLoggedIn

        ( GotCommunitySettingsObjectiveEditorMsg subMsg, CommunitySettingsObjectiveEditor subModel ) ->
            CommunitySettingsObjectiveEditor.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsObjectiveEditor GotCommunitySettingsObjectiveEditorMsg model
                |> withLoggedIn

        ( GotDashboardMsg subMsg, Dashboard subModel ) ->
            Dashboard.update subMsg subModel
                >> updateLoggedInUResult Dashboard GotDashboardMsg model
                |> withLoggedIn

        ( GotProfileMsg subMsg, Profile subModel ) ->
            Profile.update subMsg subModel
                >> updateLoggedInUResult Profile GotProfileMsg model
                |> withLoggedIn

        ( GotProfileContributionsMsg subMsg, ProfileContributions subModel ) ->
            ProfileContributions.update subMsg subModel
                >> updateLoggedInUResult ProfileContributions GotProfileContributionsMsg model
                |> withLoggedIn

        ( GotProfileEditorMsg subMsg, ProfileEditor subModel ) ->
            ProfileEditor.update subMsg subModel
                >> updateLoggedInUResult ProfileEditor GotProfileEditorMsg model
                |> withLoggedIn

        ( GotProfileAddKycMsg subMsg, ProfileAddKyc subModel ) ->
            ProfileAddKyc.update subMsg subModel
                >> updateLoggedInUResult ProfileAddKyc GotProfileAddKycMsg model
                |> withLoggedIn

        ( GotProfileClaimsMsg subMsg, ProfileClaims subModel ) ->
            ProfileClaims.update subMsg subModel
                >> updateLoggedInUResult ProfileClaims GotProfileClaimsMsg model
                |> withLoggedIn

        ( GotProfileAddContactMsg subMsg, ProfileAddContact subModel ) ->
            ProfileAddContact.update subMsg subModel
                >> updateLoggedInUResult ProfileAddContact GotProfileAddContactMsg model
                |> withLoggedIn

        ( GotCommunitySettingsMsg subMsg, CommunitySettings subModel ) ->
            CommunitySettings.update subMsg subModel
                >> updateLoggedInUResult CommunitySettings GotCommunitySettingsMsg model
                |> withLoggedIn

        ( GotCommunitySettingsShopCategoriesMsg subMsg, CommunitySettingsShopCategories subModel ) ->
            CommunitySettingsShopCategories.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsShopCategories GotCommunitySettingsShopCategoriesMsg model
                |> withLoggedIn

        ( GotCommunitySettingsFeaturesMsg subMsg, CommunitySettingsFeatures subModel ) ->
            CommunitySettingsFeatures.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsFeatures GotCommunitySettingsFeaturesMsg model
                |> withLoggedIn

        ( GotCommunitySettingsInfoMsg subMsg, CommunitySettingsInfo subModel ) ->
            CommunitySettingsInfo.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsInfo GotCommunitySettingsInfoMsg model
                |> withLoggedIn

        ( GotCommunitySettingsNewsMsg subMsg, CommunitySettingsNews subModel ) ->
            CommunitySettingsNews.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsNews GotCommunitySettingsNewsMsg model
                |> withLoggedIn

        ( GotCommunitySettingsNewsEditorMsg subMsg, CommunitySettingsNewsEditor subModel ) ->
            CommunitySettingsNewsEditor.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsNewsEditor GotCommunitySettingsNewsEditorMsg model
                |> withLoggedIn

        ( GotCommunitySettingsCurrencyMsg subMsg, CommunitySettingsCurrency subModel ) ->
            CommunitySettingsCurrency.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsCurrency GotCommunitySettingsCurrencyMsg model
                |> withLoggedIn

        ( GotCommunitySettingsSponsorshipMsg subMsg, CommunitySettingsSponsorship subModel ) ->
            CommunitySettingsSponsorship.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsSponsorship GotCommunitySettingsSponsorshipMsg model
                |> withLoggedIn

        ( GotCommunitySettingsSponsorshipFiatMsg subMsg, CommunitySettingsSponsorshipFiat subModel ) ->
            CommunitySettingsSponsorshipFiat.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsSponsorshipFiat GotCommunitySettingsSponsorshipFiatMsg model
                |> withLoggedIn

        ( GotCommunitySettingsSponsorshipThankYouMessageMsg subMsg, CommunitySettingsSponsorshipThankYouMessage subModel ) ->
            CommunitySettingsSponsorshipThankYouMessage.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsSponsorshipThankYouMessage GotCommunitySettingsSponsorshipThankYouMessageMsg model
                |> withLoggedIn

        ( GotCommunitySponsorMsg subMsg, CommunitySponsor subModel ) ->
            CommunitySponsor.update subMsg subModel
                >> updateLoggedInUResult CommunitySponsor GotCommunitySponsorMsg model
                |> withLoggedIn

        ( GotCommunitySupportersMsg subMsg, CommunitySupporters subModel ) ->
            CommunitySupporters.update subMsg subModel
                >> updateLoggedInUResult CommunitySupporters GotCommunitySupportersMsg model
                |> withLoggedIn

        ( GotShopMsg subMsg, Shop maybeFilter subModel ) ->
            Shop.update subMsg subModel
                >> updateLoggedInUResult (Shop maybeFilter) GotShopMsg model
                |> withLoggedIn

        ( GotShopEditorMsg subMsg, ShopEditor id subModel ) ->
            ShopEditor.update subMsg subModel
                >> updateLoggedInUResult (ShopEditor id) GotShopEditorMsg model
                |> withLoggedIn

        ( GotShopViewerMsg subMsg, ShopViewer saleId subModel ) ->
            ShopViewer.update subMsg subModel
                >> updatePageUResult (ShopViewer saleId) GotShopViewerMsg model
                |> withSession

        ( GotCommunitySettingsActionEditorMsg subMsg, CommunitySettingsActionEditor subModel ) ->
            CommunitySettingsActionEditor.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsActionEditor GotCommunitySettingsActionEditorMsg model
                |> withLoggedIn

        ( GotCommunitySettingsContactsMsg subMsg, CommunitySettingsContacts subModel ) ->
            CommunitySettingsContacts.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsContacts GotCommunitySettingsContactsMsg model
                |> withLoggedIn

        ( GotVerifyClaimMsg subMsg, Claim objectiveId actionId subModel ) ->
            Claim.update subMsg subModel
                >> updateLoggedInUResult (Claim objectiveId actionId) GotVerifyClaimMsg model
                |> withLoggedIn

        ( GotViewTransferScreenMsg subMsg, ViewTransfer transferId subModel ) ->
            ViewTransfer.update subMsg subModel
                >> updateLoggedInUResult (ViewTransfer transferId) GotViewTransferScreenMsg model
                |> withLoggedIn

        ( GotInviteMsg subMsg, Invite subModel ) ->
            Invite.update model.session subMsg subModel
                |> updateLoggedInUResult Invite GotInviteMsg model

        ( GotJoinMsg subMsg, Join subModel ) ->
            Join.update model.session subMsg subModel
                |> updateLoggedInUResult Join GotJoinMsg model

        ( GotTransferMsg subMsg, Transfer subModel ) ->
            Transfer.update subMsg subModel
                >> updateLoggedInUResult Transfer GotTransferMsg model
                |> withLoggedIn

        ( GotAnalysisMsg subMsg, Analysis subModel ) ->
            Analysis.update subMsg subModel
                >> updateLoggedInUResult Analysis GotAnalysisMsg model
                |> withLoggedIn

        ( _, _ ) ->
            ( model
            , { type_ = Log.InfoBreadcrumb
              , category = msg
              , message = "Msg does not correspond with Model"
              , data = Dict.empty
              , level = Log.Info
              }
                |> Log.addBreadcrumb msgToString
            )


broadcastGuest : Guest.BroadcastMsg -> Status -> Cmd Msg
broadcastGuest broadcastMessage status =
    let
        maybeMsg =
            case status of
                Register _ _ _ ->
                    Register.receiveBroadcast broadcastMessage
                        |> Maybe.map GotRegisterMsg

                _ ->
                    Nothing
    in
    case maybeMsg of
        Just msg ->
            Utils.spawnMessage msg

        Nothing ->
            Cmd.none


broadcast : LoggedIn.Model -> LoggedIn.BroadcastMsg -> Status -> Cmd Msg
broadcast loggedIn broadcastMessage status =
    let
        maybeMsg =
            case status of
                Dashboard _ ->
                    Dashboard.receiveBroadcast broadcastMessage
                        |> Maybe.map GotDashboardMsg

                Transfer _ ->
                    Transfer.receiveBroadcast broadcastMessage
                        |> Maybe.map GotTransferMsg

                ProfileClaims _ ->
                    ProfileClaims.receiveBroadcast broadcastMessage
                        |> Maybe.map GotProfileClaimsMsg

                CommunitySettingsActionEditor _ ->
                    CommunitySettingsActionEditor.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsActionEditorMsg

                CommunitySettingsObjectives _ ->
                    CommunitySettingsObjectives.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsObjectivesMsg

                CommunityAbout _ ->
                    CommunityAbout.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunityAboutMsg

                CommunityObjectives _ ->
                    CommunityObjectives.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunityObjectivesMsg

                CommunitySettingsFeatures _ ->
                    CommunitySettingsFeatures.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsFeaturesMsg

                CommunitySettingsInfo _ ->
                    CommunitySettingsInfo.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsInfoMsg

                CommunitySettingsNews _ ->
                    CommunitySettingsNews.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsNewsMsg

                CommunitySettingsNewsEditor _ ->
                    CommunitySettingsNewsEditor.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsNewsEditorMsg

                CommunitySettingsCurrency _ ->
                    CommunitySettingsCurrency.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsCurrencyMsg

                CommunitySettingsSponsorship _ ->
                    CommunitySettingsSponsorship.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsSponsorshipMsg

                CommunitySettingsSponsorshipFiat _ ->
                    CommunitySettingsSponsorshipFiat.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsSponsorshipFiatMsg

                CommunitySettingsSponsorshipThankYouMessage _ ->
                    CommunitySettingsSponsorshipThankYouMessage.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsSponsorshipThankYouMessageMsg

                CommunitySettings _ ->
                    CommunitySettings.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsMsg

                CommunitySponsor _ ->
                    CommunitySponsor.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySponsorMsg

                CommunitySupporters _ ->
                    CommunitySupporters.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySupportersMsg

                CommunitySettingsObjectiveEditor _ ->
                    CommunitySettingsObjectiveEditor.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsObjectiveEditorMsg

                CommunitySettingsContacts _ ->
                    CommunitySettingsContacts.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsContactsMsg

                ProfileAddContact _ ->
                    ProfileAddContact.receiveBroadcast broadcastMessage
                        |> Maybe.map GotProfileAddContactMsg

                ProfileEditor _ ->
                    ProfileEditor.receiveBroadcast broadcastMessage
                        |> Maybe.map GotProfileEditorMsg

                Profile _ ->
                    Profile.receiveBroadcast broadcastMessage
                        |> Maybe.map GotProfileMsg

                ProfileContributions _ ->
                    ProfileContributions.receiveBroadcast broadcastMessage
                        |> Maybe.map GotProfileContributionsMsg

                ShopEditor _ subModel ->
                    ShopEditor.receiveBroadcast loggedIn.shared.translators broadcastMessage subModel
                        |> Maybe.map GotShopEditorMsg

                Shop _ subModel ->
                    Shop.receiveBroadcast broadcastMessage
                        |> Maybe.map GotShopMsg

                Invite _ ->
                    Invite.receiveBroadcast broadcastMessage
                        |> Maybe.map GotInviteMsg

                Join _ ->
                    Join.receiveBroadcast broadcastMessage
                        |> Maybe.map GotJoinMsg

                News _ ->
                    News.receiveBroadcast broadcastMessage
                        |> Maybe.map GotNewsMsg

                PaymentHistory _ ->
                    PaymentHistory.receiveBroadcast broadcastMessage
                        |> Maybe.map GotPaymentHistoryMsg

                _ ->
                    Nothing
    in
    case maybeMsg of
        Just msg ->
            Utils.spawnMessage msg

        Nothing ->
            Cmd.none


updateStatusWith : (subModel -> Status) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateStatusWith toStatus toMsg model ( subModel, subCmd ) =
    ( { model | status = toStatus subModel }
    , Cmd.map toMsg subCmd
    )


updateSessionWith : (subMsg -> Msg) -> Model -> ( Session, Cmd subMsg ) -> ( Model, Cmd Msg )
updateSessionWith toMsg model ( session, subCmd ) =
    ( { model | session = session }
    , Cmd.map toMsg subCmd
    )


updatePageUResult : (subModel -> Status) -> (subMsg -> Msg) -> Model -> UpdateResult subModel subMsg (Page.External subMsg) -> ( Model, Cmd Msg )
updatePageUResult toStatus toMsg model uResult =
    case model.session of
        Page.Guest _ ->
            uResult
                |> UR.map identity
                    identity
                    (\extMsg currUResult ->
                        case extMsg of
                            Page.GuestExternal guestMsg ->
                                UR.addExt guestMsg currUResult

                            Page.LoggedInExternal _ ->
                                currUResult
                    )
                |> updateGuestUResult toStatus toMsg model

        Page.LoggedIn _ ->
            uResult
                |> UR.map identity
                    identity
                    (\extMsg currUResult ->
                        case extMsg of
                            Page.GuestExternal _ ->
                                currUResult

                            Page.LoggedInExternal loggedInMsg ->
                                UR.addExt loggedInMsg currUResult
                    )
                |> updateLoggedInUResult toStatus toMsg model


updateGuestUResult : (subModel -> Status) -> (subMsg -> Msg) -> Model -> UpdateResult subModel subMsg Guest.External -> ( Model, Cmd Msg )
updateGuestUResult toStatus toMsg model uResult =
    List.foldl
        (\commExtMsg ( m, cmds_ ) ->
            case m.session of
                Page.LoggedIn _ ->
                    ( m, cmds_ )

                Page.Guest guest ->
                    case commExtMsg of
                        Guest.LoggedIn { pin, privateKey, signInResponse } ->
                            let
                                { profile, token } =
                                    signInResponse

                                shared =
                                    guest.shared

                                userWithCommunity =
                                    { profile
                                        | communities =
                                            case guest.community of
                                                RemoteData.Success community ->
                                                    if List.any (\c -> c.symbol == community.symbol) profile.communities then
                                                        profile.communities

                                                    else
                                                        { symbol = community.symbol
                                                        , name = community.name
                                                        , logo = community.logo
                                                        , subdomain = community.subdomain
                                                        , hasShop = community.hasShop
                                                        , hasActions = community.hasObjectives
                                                        , hasKyc = community.hasKyc
                                                        }
                                                            :: profile.communities

                                                _ ->
                                                    profile.communities
                                    }

                                ( session, cmd ) =
                                    LoggedIn.initLogin shared pin (Just privateKey) userWithCommunity token

                                redirectRoute =
                                    case guest.afterLoginRedirect of
                                        Just (Route.Invite _) ->
                                            Route.Dashboard

                                        Just route ->
                                            route

                                        Nothing ->
                                            Route.Dashboard
                            in
                            ( { m
                                | session =
                                    Page.LoggedIn session
                              }
                            , Cmd.map (Page.GotLoggedInMsg >> GotPageMsg) cmd
                                :: Api.Graphql.createAbsintheSocket token
                                :: Route.pushUrl guest.shared.navKey redirectRoute
                                :: Log.addBreadcrumb msgToString
                                    { type_ = Log.InfoBreadcrumb
                                    , category = Ignored
                                    , message = "Guest logged in"
                                    , data = Dict.fromList [ ( "username", Eos.Account.encodeName profile.account ) ]
                                    , level = Log.Info
                                    }
                                :: cmds_
                            )

                        Guest.SetFeedback feedback ->
                            ( { m | session = Page.Guest { guest | feedback = feedback } }
                            , cmds_
                            )

                        Guest.UpdatedShared newShared ->
                            ( { m | session = Page.Guest { guest | shared = newShared } }
                            , cmds_
                            )
        )
        ( { model | status = toStatus uResult.model }
        , []
        )
        uResult.exts
        |> (\( model_, cmds_ ) ->
                ( model_
                , Cmd.batch
                    (Cmd.map toMsg (Cmd.batch uResult.cmds)
                        :: List.map (Ports.mapAddress toMsg >> Ports.javascriptOutCmd msgToString) uResult.ports
                        ++ List.map (Log.map toMsg >> Log.send msgToString) uResult.events
                        ++ cmds_
                    )
                )
           )


updateExternal : Page.ExternalMsg Msg -> Model -> ( Model, Cmd Msg )
updateExternal extMsg model =
    case extMsg of
        Page.LoggedInExternalMsg LoggedIn.AuthenticationSucceed ->
            case model.afterAuthMsg of
                Nothing ->
                    ( model, Cmd.none )

                Just aMsg ->
                    update aMsg.successMsg { model | afterAuthMsg = Nothing }

        Page.LoggedInExternalMsg LoggedIn.AuthenticationFailed ->
            case model.afterAuthMsg of
                Nothing ->
                    ( model, Cmd.none )

                Just aMsg ->
                    update aMsg.errorMsg { model | afterAuthMsg = Nothing }

        Page.LoggedInExternalMsg (LoggedIn.AddAfterAuthTokenCallback callback) ->
            ( { model | afterAuthTokenCallbacks = callback :: model.afterAuthTokenCallbacks }, Cmd.none )

        Page.LoggedInExternalMsg (LoggedIn.AddAfterAuthTokenCallbackInternal callback) ->
            ( { model
                | afterAuthTokenCallbacks =
                    (\token ->
                        callback token
                            |> Cmd.map (Page.GotLoggedInMsg >> GotPageMsg)
                    )
                        :: model.afterAuthTokenCallbacks
              }
            , Cmd.none
            )

        Page.LoggedInExternalMsg (LoggedIn.RunAfterAuthTokenCallbacks authToken) ->
            ( { model | afterAuthTokenCallbacks = [] }
            , model.afterAuthTokenCallbacks
                |> List.map (\callback -> callback authToken)
                |> Cmd.batch
            )

        Page.LoggedInExternalMsg (LoggedIn.AddAfterPrivateKeyCallback callbackMsg) ->
            ( { model
                | afterPrivateKeyCallbacks =
                    (callbackMsg
                        |> Page.GotLoggedInMsg
                        |> GotPageMsg
                    )
                        :: model.afterPrivateKeyCallbacks
              }
            , Cmd.none
            )

        Page.LoggedInExternalMsg LoggedIn.RunAfterPrivateKeyCallbacks ->
            ( { model | afterPrivateKeyCallbacks = [] }
            , model.afterPrivateKeyCallbacks
                |> List.map Utils.spawnMessage
                |> Cmd.batch
            )

        Page.LoggedInExternalMsg (LoggedIn.Broadcast broadcastMsg) ->
            case model.session of
                Page.LoggedIn loggedIn ->
                    ( model, broadcast loggedIn broadcastMsg model.status )

                _ ->
                    ( model, Cmd.none )

        Page.LoggedInExternalMsg (LoggedIn.RunExternalMsg subExternalMsg) ->
            ( model, Utils.spawnMessage subExternalMsg )

        Page.GuestBroadcastMsg broadcastMsg ->
            ( model, broadcastGuest broadcastMsg model.status )


updateLoggedInUResult : (subModel -> Status) -> (subMsg -> Msg) -> Model -> UpdateResult subModel subMsg (LoggedIn.External subMsg) -> ( Model, Cmd Msg )
updateLoggedInUResult toStatus toMsg model uResult =
    List.foldl
        (\commExtMsg ( m, cmds_ ) ->
            case ( commExtMsg, m.session ) of
                ( LoggedIn.UpdatedLoggedIn loggedIn, Page.Guest _ ) ->
                    ( { m | session = Page.LoggedIn loggedIn }, cmds_ )

                ( _, Page.LoggedIn loggedIn ) ->
                    let
                        updateResult =
                            LoggedIn.updateExternal commExtMsg loggedIn

                        broadcastCmd =
                            case updateResult.broadcastMsg of
                                Nothing ->
                                    Cmd.none

                                Just broadcastMsg ->
                                    broadcast loggedIn broadcastMsg m.status
                    in
                    ( { m
                        | session = Page.LoggedIn updateResult.model
                        , afterAuthMsg =
                            Maybe.map
                                (\{ successMsg, errorMsg } ->
                                    { successMsg = toMsg successMsg, errorMsg = toMsg errorMsg }
                                )
                                updateResult.afterAuthMsg
                      }
                    , Cmd.map (LoggedIn.mapMsg toMsg >> Page.GotLoggedInMsg >> GotPageMsg) updateResult.cmd
                        :: broadcastCmd
                        :: cmds_
                    )

                ( _, Page.Guest _ ) ->
                    ( m, cmds_ )
        )
        ( { model | status = toStatus uResult.model }
        , []
        )
        uResult.exts
        |> (\( model_, cmds_ ) ->
                ( model_
                , Cmd.batch
                    (Cmd.map toMsg (Cmd.batch uResult.cmds)
                        :: List.map (Ports.mapAddress toMsg >> Ports.javascriptOutCmd msgToString) uResult.ports
                        ++ List.map (Log.map toMsg >> Log.send msgToString) uResult.events
                        ++ cmds_
                    )
                )
           )


hideFeedback : Model -> Model
hideFeedback model =
    case model.session of
        Page.LoggedIn loggedIn ->
            { model
                | session =
                    Page.LoggedIn { loggedIn | feedback = Feedback.Hidden }
            }

        Page.Guest guest ->
            { model | session = Page.Guest { guest | feedback = Feedback.Hidden } }


statusToRoute : Status -> Session -> Maybe Route
statusToRoute status session =
    case status of
        Redirect ->
            Nothing

        NotFound ->
            Nothing

        ComingSoon ->
            Nothing

        PaymentHistory subModel ->
            subModel.recipientProfile.account
                |> Route.PaymentHistory
                |> Just

        CommunityAbout _ ->
            Just Route.CommunityAbout

        CommunityObjectives _ ->
            Just (Route.CommunityObjectives Route.WithNoObjectiveSelected)

        CommunityEditor _ ->
            Just Route.NewCommunity

        CommunitySettings _ ->
            Just Route.CommunitySettings

        CommunitySettingsShopCategories _ ->
            Just Route.CommunitySettingsShopCategories

        CommunitySettingsFeatures _ ->
            Just Route.CommunitySettingsFeatures

        CommunitySettingsInfo _ ->
            Just Route.CommunitySettingsInfo

        CommunitySettingsNews _ ->
            Just Route.CommunitySettingsNews

        CommunitySettingsNewsEditor _ ->
            Just (Route.CommunitySettingsNewsEditor Route.CreateNews)

        CommunitySettingsCurrency _ ->
            Just Route.CommunitySettingsCurrency

        CommunitySettingsSponsorship _ ->
            Just Route.CommunitySettingsSponsorship

        CommunitySettingsSponsorshipFiat _ ->
            Just Route.CommunitySettingsSponsorshipFiat

        CommunitySettingsSponsorshipThankYouMessage _ ->
            Just Route.CommunitySettingsSponsorshipThankYouMessage

        CommunitySelector subModel ->
            Just (Route.CommunitySelector subModel.maybeRedirect)

        CommunityThankYou ->
            Just Route.CommunityThankYou

        CommunitySponsor _ ->
            Just Route.CommunitySponsor

        CommunitySupporters _ ->
            Just Route.CommunitySupporters

        CommunitySettingsObjectives _ ->
            Just Route.CommunitySettingsObjectives

        CommunitySettingsObjectiveEditor subModel ->
            case subModel.objectiveId of
                Nothing ->
                    Just Route.CommunitySettingsNewObjective

                Just objectiveId ->
                    Just (Route.CommunitySettingsEditObjective (Action.objectiveIdToInt objectiveId))

        CommunitySettingsActionEditor subModel ->
            case subModel.actionId of
                Nothing ->
                    Just (Route.CommunitySettingsNewAction (Action.objectiveIdToInt subModel.objectiveId))

                Just actionId ->
                    Just
                        (Route.CommunitySettingsEditAction
                            (Action.objectiveIdToInt subModel.objectiveId)
                            (Action.idToInt actionId)
                        )

        CommunitySettingsContacts _ ->
            Just Route.CommunitySettingsContacts

        Claim objectiveId actionId subModel ->
            Just (Route.Claim objectiveId actionId subModel.claimId)

        Notification _ ->
            Just Route.Notification

        Dashboard _ ->
            Just Route.Dashboard

        Login maybeRedirect _ ->
            let
                maybeInvitation =
                    case session of
                        Page.LoggedIn _ ->
                            Nothing

                        Page.Guest guest ->
                            guest.maybeInvitation
            in
            Just (Route.Login maybeInvitation maybeRedirect)

        News subModel ->
            Just (Route.News { selectedNews = subModel.newsId, showOthers = subModel.showOtherNews })

        Profile subModel ->
            Just (Route.Profile subModel.profileName)

        ProfileContributions subModel ->
            Just (Route.ProfileContributions subModel.profileName)

        ProfileEditor _ ->
            Just Route.ProfileEditor

        ProfileAddKyc _ ->
            Just Route.ProfileAddKyc

        ProfileClaims subModel ->
            Just (Route.ProfileClaims subModel.accountString)

        ProfileAddContact _ ->
            Just Route.ProfileAddContact

        Register inviteId maybeRedirect _ ->
            Just (Route.Register inviteId maybeRedirect)

        Settings _ ->
            Just Route.Settings

        Shop filter _ ->
            Just (Route.Shop filter)

        ShopEditor maybeSaleId subModel ->
            case maybeSaleId of
                Nothing ->
                    Just (Route.NewSale (ShopEditor.getCurrentStep subModel))

                Just saleId ->
                    Just (Route.EditSale saleId (ShopEditor.getCurrentStep subModel))

        ShopViewer saleId _ ->
            Just (Route.ViewSale saleId)

        ViewTransfer transferId _ ->
            Just (Route.ViewTransfer transferId)

        Invite subModel ->
            Just (Route.Invite subModel.invitationId)

        Join subModel ->
            Just (Route.Join subModel.maybeRedirect)

        Transfer subModel ->
            Just (Route.Transfer subModel.maybeTo)

        Analysis _ ->
            Just Route.Analysis


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            model.session

        shared =
            case session of
                Page.Guest guest ->
                    guest.shared

                Page.LoggedIn loggedIn ->
                    loggedIn.shared

        updateStatus model_ newStatus =
            { model_ | status = newStatus }

        noCmd model_ =
            ( model_, Cmd.none )

        addAfterLoginRedirect : Maybe Route -> Model -> Model
        addAfterLoginRedirect maybeRedirect model_ =
            case model_.session of
                Page.LoggedIn _ ->
                    model_

                Page.Guest guest ->
                    case maybeRedirect of
                        Nothing ->
                            model_

                        Just redirect ->
                            { model_
                                | session =
                                    Guest.addAfterLoginRedirect redirect guest
                                        |> Page.Guest
                            }

        addMaybeInvitation : Maybe String -> Model -> Model
        addMaybeInvitation maybeInvitation model_ =
            case model_.session of
                Page.LoggedIn _ ->
                    model_

                Page.Guest guest ->
                    { model_
                        | session =
                            { guest | maybeInvitation = maybeInvitation }
                                |> Page.Guest
                    }

        withGuest : Maybe String -> Maybe Route -> (Guest.Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
        withGuest maybeInvitation maybeRedirect fn =
            case session of
                Page.LoggedIn _ ->
                    ( model
                    , maybeRedirect
                        |> Maybe.withDefault Route.Dashboard
                        |> Route.replaceUrl shared.navKey
                    )

                Page.Guest guest ->
                    let
                        ( newModel, newCmd ) =
                            fn guest
                    in
                    ( newModel
                        |> addAfterLoginRedirect maybeRedirect
                        |> addMaybeInvitation maybeInvitation
                    , newCmd
                    )

        addRouteToHistory status loggedIn =
            case statusToRoute status (Page.LoggedIn loggedIn) of
                Nothing ->
                    loggedIn

                Just newRoute ->
                    case loggedIn.routeHistory of
                        (_ :: second :: rest) as routeHistory ->
                            if newRoute == second then
                                { loggedIn
                                    | routeHistory = newRoute :: rest
                                    , hasSeenDashboard = loggedIn.hasSeenDashboard || second == Route.Dashboard
                                }

                            else
                                { loggedIn
                                    | routeHistory = newRoute :: routeHistory
                                    , hasSeenDashboard = loggedIn.hasSeenDashboard || second == Route.Dashboard
                                }

                        routeHistory ->
                            { loggedIn | routeHistory = newRoute :: routeHistory }

        withLoggedIn : Route -> (LoggedIn.Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
        withLoggedIn route fn =
            case session of
                Page.LoggedIn loggedIn ->
                    let
                        ( newModel, newCmd ) =
                            fn loggedIn

                        newSession =
                            case newModel.session of
                                Page.LoggedIn newLoggedIn ->
                                    addRouteToHistory newModel.status newLoggedIn
                                        |> Page.LoggedIn

                                Page.Guest guest ->
                                    Page.Guest guest
                    in
                    ( { newModel | session = newSession }
                    , Cmd.batch
                        [ newCmd

                        -- Reload time on every page change
                        , Task.perform
                            (LoggedIn.GotTimeInternal
                                >> Page.GotLoggedInMsg
                                >> GotPageMsg
                            )
                            Time.now
                        ]
                    )

                Page.Guest guest ->
                    ( { model
                        | session =
                            Guest.addAfterLoginRedirect route guest
                                |> Page.Guest
                        , status = Redirect
                      }
                    , Route.replaceUrl shared.navKey (Route.Join (Just route))
                    )

        withSession : Route -> (Session -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
        withSession route fn =
            let
                ( newModel, newCmd ) =
                    fn session
            in
            case newModel.session of
                Page.LoggedIn loggedIn ->
                    let
                        newSession =
                            addRouteToHistory newModel.status loggedIn
                                |> Page.LoggedIn
                    in
                    ( { newModel | session = newSession }
                    , Cmd.batch
                        [ newCmd
                        , Task.perform
                            (LoggedIn.GotTimeInternal
                                >> Page.GotLoggedInMsg
                                >> GotPageMsg
                            )
                            Time.now
                        ]
                    )

                Page.Guest _ ->
                    ( newModel
                        |> addAfterLoginRedirect (Just route)
                    , newCmd
                    )
    in
    case maybeRoute of
        Nothing ->
            NotFound
                |> updateStatus model
                |> noCmd

        Just Route.Root ->
            ( model
            , Route.replaceUrl shared.navKey Route.Dashboard
            )

        Just Route.ComingSoon ->
            ComingSoon
                |> updateStatus model
                |> noCmd

        Just (Route.Register invitation maybeRedirect) ->
            Register.init invitation
                >> updateStatusWith (Register invitation maybeRedirect) GotRegisterMsg model
                |> withGuest invitation maybeRedirect

        Just Route.Settings ->
            Settings.init
                >> updateStatusWith Settings GotSettingsMsg model
                |> withLoggedIn Route.Settings

        Just (Route.Login maybeInvitation maybeRedirect) ->
            Login.init
                >> updateGuestUResult (Login maybeRedirect) GotLoginMsg model
                |> withGuest maybeInvitation maybeRedirect

        Just (Route.News config) ->
            News.init config
                >> updateLoggedInUResult News GotNewsMsg model
                |> withLoggedIn (Route.News config)

        Just (Route.PaymentHistory accountName) ->
            PaymentHistory.init accountName
                >> updateStatusWith PaymentHistory GotPaymentHistoryMsg model
                |> withLoggedIn (Route.PaymentHistory accountName)

        Just Route.Logout ->
            Page.logout
                >> updateSessionWith GotPageMsg model
                |> withLoggedIn Route.Dashboard

        Just Route.Notification ->
            Notification.init
                >> updateLoggedInUResult Notification GotNotificationMsg model
                |> withLoggedIn Route.Notification

        Just (Route.Profile profileName) ->
            (\loggedIn -> Profile.init loggedIn profileName)
                >> updateLoggedInUResult Profile GotProfileMsg model
                |> withLoggedIn (Route.Profile profileName)

        Just (Route.ProfileContributions profileName) ->
            (\l -> ProfileContributions.init l profileName)
                >> updateStatusWith ProfileContributions GotProfileContributionsMsg model
                |> withLoggedIn (Route.ProfileContributions profileName)

        Just Route.ProfileEditor ->
            ProfileEditor.init
                >> updateStatusWith ProfileEditor GotProfileEditorMsg model
                |> withLoggedIn Route.ProfileEditor

        Just Route.ProfileAddKyc ->
            ProfileAddKyc.init
                >> updateStatusWith ProfileAddKyc GotProfileAddKycMsg model
                |> withLoggedIn Route.ProfileAddKyc

        Just (Route.ProfileClaims account) ->
            (\l -> ProfileClaims.init l account)
                >> updateStatusWith ProfileClaims GotProfileClaimsMsg model
                |> withLoggedIn (Route.ProfileClaims account)

        Just Route.ProfileAddContact ->
            ProfileAddContact.init
                >> updateStatusWith ProfileAddContact GotProfileAddContactMsg model
                |> withLoggedIn Route.ProfileAddContact

        Just Route.Dashboard ->
            Dashboard.init
                >> updateStatusWith Dashboard GotDashboardMsg model
                |> withLoggedIn Route.Dashboard

        Just Route.CommunityAbout ->
            CommunityAbout.init
                >> updateLoggedInUResult CommunityAbout GotCommunityAboutMsg model
                |> withLoggedIn Route.CommunityAbout

        Just (Route.CommunityObjectives selectedObjective) ->
            CommunityObjectives.init selectedObjective
                >> updateLoggedInUResult CommunityObjectives GotCommunityObjectivesMsg model
                |> withLoggedIn (Route.CommunityObjectives selectedObjective)

        Just Route.CommunitySettings ->
            CommunitySettings.init
                >> updateStatusWith CommunitySettings GotCommunitySettingsMsg model
                |> withLoggedIn Route.CommunitySettings

        Just Route.CommunitySettingsShopCategories ->
            CommunitySettingsShopCategories.init
                >> updateLoggedInUResult CommunitySettingsShopCategories GotCommunitySettingsShopCategoriesMsg model
                |> withLoggedIn Route.CommunitySettingsShopCategories

        Just Route.CommunitySettingsFeatures ->
            CommunitySettingsFeatures.init
                >> updateStatusWith CommunitySettingsFeatures GotCommunitySettingsFeaturesMsg model
                |> withLoggedIn Route.CommunitySettingsFeatures

        Just Route.CommunitySettingsInfo ->
            CommunitySettingsInfo.init
                >> updateStatusWith CommunitySettingsInfo GotCommunitySettingsInfoMsg model
                |> withLoggedIn Route.CommunitySettingsInfo

        Just Route.CommunitySettingsNews ->
            CommunitySettingsNews.init
                >> updateLoggedInUResult CommunitySettingsNews GotCommunitySettingsNewsMsg model
                |> withLoggedIn Route.CommunitySettingsNews

        Just (Route.CommunitySettingsNewsEditor editorKind) ->
            CommunitySettingsNewsEditor.init editorKind
                >> updateLoggedInUResult CommunitySettingsNewsEditor GotCommunitySettingsNewsEditorMsg model
                |> withLoggedIn (Route.CommunitySettingsNewsEditor editorKind)

        Just Route.CommunitySettingsCurrency ->
            CommunitySettingsCurrency.init
                >> updateStatusWith CommunitySettingsCurrency GotCommunitySettingsCurrencyMsg model
                |> withLoggedIn Route.CommunitySettingsCurrency

        Just Route.CommunitySettingsSponsorship ->
            CommunitySettingsSponsorship.init
                >> updateStatusWith CommunitySettingsSponsorship GotCommunitySettingsSponsorshipMsg model
                |> withLoggedIn Route.CommunitySettingsSponsorship

        Just Route.CommunitySettingsSponsorshipFiat ->
            CommunitySettingsSponsorshipFiat.init
                >> updateStatusWith CommunitySettingsSponsorshipFiat GotCommunitySettingsSponsorshipFiatMsg model
                |> withLoggedIn Route.CommunitySettingsSponsorshipFiat

        Just Route.CommunitySettingsSponsorshipThankYouMessage ->
            CommunitySettingsSponsorshipThankYouMessage.init
                >> updateStatusWith CommunitySettingsSponsorshipThankYouMessage GotCommunitySettingsSponsorshipThankYouMessageMsg model
                |> withLoggedIn Route.CommunitySettingsSponsorshipThankYouMessage

        Just (Route.CommunitySelector maybeRedirect) ->
            CommunitySelector.init maybeRedirect
                >> updateStatusWith CommunitySelector (\_ -> Ignored) model
                |> withLoggedIn (Route.CommunitySelector maybeRedirect)

        Just Route.CommunityThankYou ->
            CommunityThankYou
                |> updateStatus model
                |> noCmd

        Just Route.CommunitySponsor ->
            CommunitySponsor.init
                >> updateStatusWith CommunitySponsor GotCommunitySponsorMsg model
                |> withLoggedIn Route.CommunitySponsor

        Just Route.CommunitySupporters ->
            CommunitySupporters.init
                >> updateLoggedInUResult CommunitySupporters GotCommunitySupportersMsg model
                |> withLoggedIn Route.CommunitySupporters

        Just Route.NewCommunity ->
            CommunityEditor.init
                >> updateStatusWith CommunityEditor GotCommunityEditorMsg model
                |> withLoggedIn Route.NewCommunity

        Just Route.CommunitySettingsObjectives ->
            CommunitySettingsObjectives.init
                >> updateStatusWith CommunitySettingsObjectives GotCommunitySettingsObjectivesMsg model
                |> withLoggedIn Route.CommunitySettingsObjectives

        Just Route.CommunitySettingsNewObjective ->
            CommunitySettingsObjectiveEditor.initNew
                >> updateStatusWith CommunitySettingsObjectiveEditor GotCommunitySettingsObjectiveEditorMsg model
                |> withLoggedIn Route.CommunitySettingsNewObjective

        Just (Route.CommunitySettingsEditObjective objectiveId) ->
            (\l -> CommunitySettingsObjectiveEditor.initEdit l (Action.objectiveIdFromInt objectiveId))
                >> updateStatusWith CommunitySettingsObjectiveEditor GotCommunitySettingsObjectiveEditorMsg model
                |> withLoggedIn (Route.CommunitySettingsEditObjective objectiveId)

        Just (Route.CommunitySettingsNewAction objectiveId) ->
            (\l -> CommunitySettingsActionEditor.init l (Action.objectiveIdFromInt objectiveId) Nothing)
                >> updateLoggedInUResult CommunitySettingsActionEditor GotCommunitySettingsActionEditorMsg model
                |> withLoggedIn (Route.CommunitySettingsNewAction objectiveId)

        Just (Route.CommunitySettingsEditAction objectiveId actionId) ->
            (\l ->
                CommunitySettingsActionEditor.init l
                    (Action.objectiveIdFromInt objectiveId)
                    (Just <| Action.idFromInt actionId)
            )
                >> updateLoggedInUResult CommunitySettingsActionEditor GotCommunitySettingsActionEditorMsg model
                |> withLoggedIn (Route.CommunitySettingsEditAction objectiveId actionId)

        Just Route.CommunitySettingsContacts ->
            (\l -> CommunitySettingsContacts.init l)
                >> updateStatusWith CommunitySettingsContacts GotCommunitySettingsContactsMsg model
                |> withLoggedIn Route.CommunitySettingsContacts

        Just (Route.Claim objectiveId actionId claimId) ->
            (\l -> Claim.init l claimId)
                >> updateLoggedInUResult (Claim objectiveId actionId) GotVerifyClaimMsg model
                |> withLoggedIn (Route.Claim objectiveId actionId claimId)

        Just (Route.Shop maybeFilter) ->
            (\l -> Shop.init l maybeFilter)
                >> updateLoggedInUResult (Shop maybeFilter) GotShopMsg model
                |> withLoggedIn (Route.Shop maybeFilter)

        Just (Route.NewSale step) ->
            let
                newModelCmd l =
                    case model.status of
                        ShopEditor _ shopEditorModel ->
                            ShopEditor.maybeSetStep l
                                step
                                shopEditorModel

                        _ ->
                            ShopEditor.initCreate l
            in
            newModelCmd
                >> updateLoggedInUResult (ShopEditor Nothing) GotShopEditorMsg model
                |> withLoggedIn (Route.NewSale step)

        Just (Route.EditSale saleId saleStep) ->
            let
                newUpdateResult l =
                    case model.status of
                        ShopEditor _ shopEditorModel ->
                            ShopEditor.maybeSetStep l
                                saleStep
                                shopEditorModel

                        _ ->
                            ShopEditor.initUpdate saleId saleStep l
            in
            newUpdateResult
                >> updateLoggedInUResult (ShopEditor (Just saleId)) GotShopEditorMsg model
                |> withLoggedIn (Route.EditSale saleId saleStep)

        Just (Route.ViewSale saleId) ->
            (\s -> ShopViewer.init s saleId)
                >> updatePageUResult (ShopViewer saleId) GotShopViewerMsg model
                |> withSession (Route.ViewSale saleId)

        Just (Route.ViewTransfer transferId) ->
            (\l -> ViewTransfer.init l transferId)
                >> updateLoggedInUResult (ViewTransfer transferId) GotViewTransferScreenMsg model
                |> withLoggedIn (Route.ViewTransfer transferId)

        Just (Route.Invite invitationId) ->
            (\s -> Invite.init s invitationId)
                >> updateStatusWith Invite GotInviteMsg model
                |> withSession (Route.Invite invitationId)

        Just (Route.Join maybeRedirect) ->
            (\s -> Join.init s maybeRedirect)
                >> updateStatusWith Join GotJoinMsg model
                |> withSession (Route.Join maybeRedirect)

        Just (Route.Transfer maybeTo) ->
            (\l -> Transfer.init l maybeTo)
                >> updateLoggedInUResult Transfer GotTransferMsg model
                |> withLoggedIn (Route.Transfer maybeTo)

        Just Route.Analysis ->
            (\l -> Analysis.init l)
                >> updateLoggedInUResult Analysis GotAnalysisMsg model
                |> withLoggedIn Route.Analysis


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg address val =
    case address of
        "GotPageMsg" :: rAddress ->
            Maybe.map GotPageMsg
                (Page.jsAddressToMsg rAddress val)

        "GotLoginMsg" :: rAddress ->
            Maybe.map GotLoginMsg
                (Login.jsAddressToMsg rAddress val)

        "GotDashboardMsg" :: rAddress ->
            Maybe.map GotDashboardMsg
                (Dashboard.jsAddressToMsg rAddress val)

        "GotCommunityAboutMsg" :: rAddress ->
            Maybe.map GotCommunityAboutMsg
                (CommunityAbout.jsAddressToMsg rAddress val)

        "GotCommunityEditorMsg" :: rAddress ->
            Maybe.map GotCommunityEditorMsg
                (CommunityEditor.jsAddressToMsg rAddress val)

        "GotCommunitySettingsObjectiveEditorMsg" :: rAddress ->
            Maybe.map GotCommunitySettingsObjectiveEditorMsg
                (CommunitySettingsObjectiveEditor.jsAddressToMsg rAddress val)

        "GotShopViewerMsg" :: rAddress ->
            Maybe.map GotShopViewerMsg
                (ShopViewer.jsAddressToMsg rAddress val)

        "GotRegisterMsg" :: rAddress ->
            Maybe.map GotRegisterMsg
                (Register.jsAddressToMsg rAddress val)

        "GotSettingsMsg" :: rAddress ->
            Maybe.map GotSettingsMsg
                (Settings.jsAddressToMsg rAddress val)

        "GotCommunitySettingsFeaturesMsg" :: rAddress ->
            Maybe.map GotCommunitySettingsFeaturesMsg
                (CommunitySettingsFeatures.jsAddressToMsg rAddress val)

        "GotCommunitySettingsInfoMsg" :: rAddress ->
            Maybe.map GotCommunitySettingsInfoMsg
                (CommunitySettingsInfo.jsAddressToMsg rAddress val)

        "GotCommunitySettingsCurrencyMsg" :: rAddress ->
            Maybe.map GotCommunitySettingsCurrencyMsg
                (CommunitySettingsCurrency.jsAddressToMsg rAddress val)

        "GotCommunitySettingsActionEditorMsg" :: rAddress ->
            Maybe.map GotCommunitySettingsActionEditorMsg
                (CommunitySettingsActionEditor.jsAddressToMsg rAddress val)

        "GotVerifyClaimMsg" :: rAddress ->
            Maybe.map GotVerifyClaimMsg
                (Claim.jsAddressToMsg rAddress val)

        "GotTransferMsg" :: rAddress ->
            Maybe.map GotTransferMsg
                (Transfer.jsAddressToMsg rAddress val)

        "GotAnalysisMsg" :: rAddress ->
            Maybe.map GotAnalysisMsg
                (Analysis.jsAddressToMsg rAddress val)

        "GotProfileClaimsMsg" :: rAddress ->
            Maybe.map GotProfileClaimsMsg
                (ProfileClaims.jsAddressToMsg rAddress val)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        ChangedUrl _ ->
            [ "ChangedUrl" ]

        ClickedLink _ ->
            [ "ClickedLink" ]

        GotJavascriptData _ ->
            [ "GotJavascriptData" ]

        GotPageMsg subMsg ->
            "GotPageMsg" :: Page.msgToString subMsg

        GotCommunityAboutMsg subMsg ->
            "GotCommunityAboutMsg" :: CommunityAbout.msgToString subMsg

        GotCommunityObjectivesMsg subMsg ->
            "GotCommunityObjectivesMsg" :: CommunityObjectives.msgToString subMsg

        GotCommunityEditorMsg subMsg ->
            "GotCommunityEditorMsg" :: CommunityEditor.msgToString subMsg

        GotCommunitySettingsMsg subMsg ->
            "GotCommunitySettingsMsg" :: CommunitySettings.msgToString subMsg

        GotCommunitySettingsShopCategoriesMsg subMsg ->
            "GotCommunitySettingsShopCategoriesMsg" :: CommunitySettingsShopCategories.msgToString subMsg

        GotCommunitySettingsFeaturesMsg subMsg ->
            "GotCommunitySettingsFeaturesMsg" :: CommunitySettingsFeatures.msgToString subMsg

        GotCommunitySettingsInfoMsg subMsg ->
            "GotCommunitySettingsInfoMsg" :: CommunitySettingsInfo.msgToString subMsg

        GotCommunitySettingsNewsMsg subMsg ->
            "GotCommunitySettingsNewsMsg" :: CommunitySettingsNews.msgToString subMsg

        GotCommunitySettingsNewsEditorMsg subMsg ->
            "GotCommunitySettingsNewsEditorMsg" :: CommunitySettingsNewsEditor.msgToString subMsg

        GotCommunitySettingsCurrencyMsg subMsg ->
            "GotCommunitySettingsCurrencyMsg" :: CommunitySettingsCurrency.msgToString subMsg

        GotCommunitySettingsSponsorshipMsg subMsg ->
            "GotCommunitySettingsSponsorshipMsg" :: CommunitySettingsSponsorship.msgToString subMsg

        GotCommunitySettingsSponsorshipFiatMsg subMsg ->
            "GotCommunitySettingsSponsorshipFiatMsg" :: CommunitySettingsSponsorshipFiat.msgToString subMsg

        GotCommunitySettingsSponsorshipThankYouMessageMsg subMsg ->
            "GotCommunitySettingsSponsorshipThankYouMessageMsg" :: CommunitySettingsSponsorshipThankYouMessage.msgToString subMsg

        GotCommunitySponsorMsg subMsg ->
            "GotCommunitySponsorMsg" :: CommunitySponsor.msgToString subMsg

        GotCommunitySupportersMsg subMsg ->
            "GotCommunitySupportersMsg" :: CommunitySupporters.msgToString subMsg

        GotCommunitySettingsObjectivesMsg subMsg ->
            "GotCommunitySettingsObjectivesMsg" :: CommunitySettingsObjectives.msgToString subMsg

        GotCommunitySettingsObjectiveEditorMsg subMsg ->
            "GotCommunitySettingsObjectiveEditorMsg" :: CommunitySettingsObjectiveEditor.msgToString subMsg

        GotCommunitySettingsActionEditorMsg subMsg ->
            "GotCommunitySettingsActionEditorMsg" :: CommunitySettingsActionEditor.msgToString subMsg

        GotCommunitySettingsContactsMsg subMsg ->
            "GotCommunitySettingsContactsMsg" :: CommunitySettingsContacts.msgToString subMsg

        GotVerifyClaimMsg subMsg ->
            "GotVerifyClaimMsg" :: Claim.msgToString subMsg

        GotNotificationMsg subMsg ->
            "GotNotificationMsg" :: Notification.msgToString subMsg

        GotDashboardMsg subMsg ->
            "GotDashboardMsg" :: Dashboard.msgToString subMsg

        GotLoginMsg subMsg ->
            "GotLoginMsg" :: Login.msgToString subMsg

        GotNewsMsg subMsg ->
            "GotNewsMsg" :: News.msgToString subMsg

        GotPaymentHistoryMsg subMsg ->
            "GotPaymentHistoryMsg" :: PaymentHistory.msgToString subMsg

        GotProfileMsg subMsg ->
            "GotProfileMsg" :: Profile.msgToString subMsg

        GotProfileContributionsMsg subMsg ->
            "GotProfileContributionsMsg" :: ProfileContributions.msgToString subMsg

        GotProfileEditorMsg subMsg ->
            "GotProfileEditorMsg" :: ProfileEditor.msgToString subMsg

        GotProfileAddKycMsg subMsg ->
            "GotProfileAddKycMsg" :: ProfileAddKyc.msgToString subMsg

        GotProfileClaimsMsg subMsg ->
            "GotProfileClaimsMsg" :: ProfileClaims.msgToString subMsg

        GotProfileAddContactMsg subMsg ->
            "GotProfileAddContactMsg" :: ProfileAddContact.msgToString subMsg

        GotRegisterMsg subMsg ->
            "GotRegisterMsg" :: Register.msgToString subMsg

        GotSettingsMsg subMsg ->
            "GotSettingsMsg" :: Settings.msgToString subMsg

        GotShopMsg subMsg ->
            "GotShopMsg" :: Shop.msgToString subMsg

        GotShopEditorMsg subMsg ->
            "GotShopEditorMsg" :: ShopEditor.msgToString subMsg

        GotShopViewerMsg subMsg ->
            "GotShopViewerMsg" :: ShopViewer.msgToString subMsg

        GotViewTransferScreenMsg subMsg ->
            "GotViewTransferScreenMsg" :: ViewTransfer.msgToString subMsg

        GotInviteMsg subMsg ->
            "GotInviteMsg" :: Invite.msgToString subMsg

        GotJoinMsg subMsg ->
            "GotJoinMsg" :: Join.msgToString subMsg

        GotTransferMsg subMsg ->
            "GotTransferMsg" :: Transfer.msgToString subMsg

        GotAnalysisMsg subMsg ->
            "GotAnalysisMsg" :: Analysis.msgToString subMsg



-- VIEW


view : Model -> Document Msg
view model =
    let
        baseTitle =
            "Cambiatus"

        fullPageTitle : String -> String
        fullPageTitle subTitle =
            if subTitle == "" then
                baseTitle

            else
                subTitle ++ " | " ++ baseTitle

        viewGuest :
            subModel
            -> Guest.Page
            -> (subMsg -> Msg)
            -> (Guest.Model -> subModel -> { title : String, content : Html subMsg })
            -> Document Msg
        viewGuest subModel page toMsg subView =
            case model.session of
                Page.Guest guest ->
                    let
                        { title, content } =
                            subView guest subModel
                    in
                    Document
                        (fullPageTitle title)
                        [ Html.map toMsg content
                            |> Page.viewGuest GotPageMsg page guest
                        ]

                Page.LoggedIn _ ->
                    Document (fullPageTitle "") [ text "" ]

        viewLoggedIn :
            subModel
            -> LoggedIn.Page
            -> (subMsg -> Msg)
            -> (LoggedIn.Model -> subModel -> { title : String, content : Html subMsg })
            -> Document Msg
        viewLoggedIn subModel page toMsg subView =
            case model.session of
                Page.Guest _ ->
                    Document (fullPageTitle "") [ text "" ]

                Page.LoggedIn loggedIn ->
                    let
                        { title, content } =
                            subView loggedIn subModel
                    in
                    Document
                        (fullPageTitle title)
                        [ Html.map toMsg content
                            |> Page.viewLoggedIn GotPageMsg page loggedIn
                        ]

        viewPage :
            Guest.Page
            -> LoggedIn.Page
            -> (subMsg -> Msg)
            -> { title : String, content : Html subMsg }
            -> Document Msg
        viewPage guestPage loggedInPage toMsg { title, content } =
            case model.session of
                Page.Guest guest ->
                    Document
                        (fullPageTitle title)
                        [ Html.map toMsg content
                            |> Page.viewGuest GotPageMsg guestPage guest
                        ]

                Page.LoggedIn loggedIn ->
                    Document
                        (fullPageTitle title)
                        [ Html.map toMsg content
                            |> Page.viewLoggedIn GotPageMsg loggedInPage loggedIn
                        ]
    in
    case model.status of
        Redirect ->
            viewPage Guest.Redirect LoggedIn.Redirect (\_ -> Ignored) { title = "", content = text "" }

        NotFound ->
            viewPage Guest.NotFound LoggedIn.NotFound (\_ -> Ignored) (NotFound.view model.session)

        ComingSoon ->
            viewPage Guest.ComingSoon LoggedIn.ComingSoon (\_ -> Ignored) (ComingSoon.view model.session)

        Invite subModel ->
            viewPage Guest.Invite LoggedIn.Invite GotInviteMsg (Invite.view model.session subModel)

        Join subModel ->
            viewPage Guest.Join LoggedIn.Join GotJoinMsg (Join.view model.session subModel)

        PaymentHistory subModel ->
            viewLoggedIn subModel LoggedIn.PaymentHistory GotPaymentHistoryMsg PaymentHistory.view

        Register _ _ subModel ->
            viewGuest subModel Guest.Register GotRegisterMsg Register.view

        Settings subModel ->
            viewLoggedIn subModel LoggedIn.Settings GotSettingsMsg Settings.view

        Login _ subModel ->
            viewGuest subModel Guest.Login GotLoginMsg Login.view

        News subModel ->
            viewLoggedIn subModel (LoggedIn.News subModel.newsId) GotNewsMsg News.view

        Notification subModel ->
            viewLoggedIn subModel LoggedIn.Notification GotNotificationMsg Notification.view

        CommunityAbout subModel ->
            viewLoggedIn subModel LoggedIn.CommunityAbout GotCommunityAboutMsg CommunityAbout.view

        CommunityObjectives subModel ->
            viewLoggedIn subModel LoggedIn.CommunityObjectives GotCommunityObjectivesMsg CommunityObjectives.view

        CommunitySettings subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettings GotCommunitySettingsMsg CommunitySettings.view

        CommunitySettingsShopCategories subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsShopCategories GotCommunitySettingsShopCategoriesMsg CommunitySettingsShopCategories.view

        CommunitySettingsFeatures subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsFeatures GotCommunitySettingsFeaturesMsg CommunitySettingsFeatures.view

        CommunitySettingsInfo subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsInfo GotCommunitySettingsInfoMsg CommunitySettingsInfo.view

        CommunitySettingsNews subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsNews GotCommunitySettingsNewsMsg CommunitySettingsNews.view

        CommunitySettingsNewsEditor subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsNewsEditor GotCommunitySettingsNewsEditorMsg CommunitySettingsNewsEditor.view

        CommunitySettingsCurrency subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsCurrency GotCommunitySettingsCurrencyMsg CommunitySettingsCurrency.view

        CommunitySettingsSponsorship subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsSponsorship GotCommunitySettingsSponsorshipMsg CommunitySettingsSponsorship.view

        CommunitySettingsSponsorshipFiat subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsSponsorshipFiat GotCommunitySettingsSponsorshipFiatMsg CommunitySettingsSponsorshipFiat.view

        CommunitySettingsSponsorshipThankYouMessage subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsSponsorshipThankYouMessage GotCommunitySettingsSponsorshipThankYouMessageMsg CommunitySettingsSponsorshipThankYouMessage.view

        CommunitySelector subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySelector (\_ -> Ignored) CommunitySelector.view

        CommunityThankYou ->
            viewLoggedIn () LoggedIn.CommunityThankYou (\_ -> Ignored) (\l () -> CommunityThankYou.view l)

        CommunitySponsor subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySponsor GotCommunitySponsorMsg CommunitySponsor.view

        CommunitySupporters subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySupporters GotCommunitySupportersMsg CommunitySupporters.view

        CommunityEditor subModel ->
            viewLoggedIn subModel LoggedIn.CommunityEditor GotCommunityEditorMsg CommunityEditor.view

        CommunitySettingsObjectives subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsObjectives GotCommunitySettingsObjectivesMsg CommunitySettingsObjectives.view

        CommunitySettingsObjectiveEditor subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsObjectiveEditor GotCommunitySettingsObjectiveEditorMsg CommunitySettingsObjectiveEditor.view

        CommunitySettingsActionEditor subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsActionEditor GotCommunitySettingsActionEditorMsg CommunitySettingsActionEditor.view

        CommunitySettingsContacts subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsContacts GotCommunitySettingsContactsMsg CommunitySettingsContacts.view

        Claim _ _ subModel ->
            viewLoggedIn subModel LoggedIn.Claim GotVerifyClaimMsg Claim.view

        Dashboard subModel ->
            viewLoggedIn subModel LoggedIn.Dashboard GotDashboardMsg Dashboard.view

        Profile subModel ->
            viewLoggedIn subModel LoggedIn.Profile GotProfileMsg Profile.view

        ProfileContributions subModel ->
            viewLoggedIn subModel LoggedIn.ProfileContributions GotProfileContributionsMsg ProfileContributions.view

        ProfileEditor subModel ->
            viewLoggedIn subModel LoggedIn.ProfileEditor GotProfileEditorMsg ProfileEditor.view

        ProfileAddKyc subModel ->
            viewLoggedIn subModel LoggedIn.ProfileAddKyc GotProfileAddKycMsg ProfileAddKyc.view

        ProfileClaims subModel ->
            viewLoggedIn subModel LoggedIn.ProfileClaims GotProfileClaimsMsg ProfileClaims.view

        ProfileAddContact subModel ->
            viewLoggedIn subModel LoggedIn.ProfileAddContact GotProfileAddContactMsg ProfileAddContact.view

        Shop _ subModel ->
            viewLoggedIn subModel LoggedIn.Shop GotShopMsg Shop.view

        ShopEditor _ subModel ->
            viewLoggedIn subModel LoggedIn.ShopEditor GotShopEditorMsg ShopEditor.view

        ShopViewer _ subModel ->
            viewPage Guest.ShopViewer LoggedIn.ShopViewer GotShopViewerMsg (ShopViewer.view model.session subModel)

        ViewTransfer _ subModel ->
            viewLoggedIn subModel LoggedIn.ViewTransfer GotViewTransferScreenMsg ViewTransfer.view

        Transfer subModel ->
            viewLoggedIn subModel LoggedIn.Transfer GotTransferMsg Transfer.view

        Analysis subModel ->
            viewLoggedIn subModel LoggedIn.Analysis GotAnalysisMsg Analysis.view
