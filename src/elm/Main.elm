module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict
import Eos.Account
import Flags
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Log
import Page exposing (Session)
import Page.ComingSoon as ComingSoon
import Page.Community as CommunityPage
import Page.Community.ActionEditor as ActionEditor
import Page.Community.Invite as Invite
import Page.Community.New as CommunityEditor
import Page.Community.ObjectiveEditor as ObjectiveEditor
import Page.Community.Objectives as Objectives
import Page.Community.Selector as CommunitySelector
import Page.Community.Settings.Currency as CommunitySettingsCurrency
import Page.Community.Settings.Features as CommunitySettingsFeatures
import Page.Community.Settings.Info as CommunitySettingsInfo
import Page.Community.Settings.Settings as CommunitySettings
import Page.Community.Settings.Sponsorship as CommunitySettingsSponsorship
import Page.Community.Sponsor as CommunitySponsor
import Page.Community.Supporters as CommunitySupporters
import Page.Community.ThankYou as CommunityThankYou
import Page.Community.Transfer as Transfer
import Page.Dashboard as Dashboard
import Page.Dashboard.Analysis as Analysis
import Page.Dashboard.Claim as Claim
import Page.Join as Join
import Page.Login as Login
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
import Task
import Time
import UpdateResult as UR exposing (UpdateResult)
import Url exposing (Url)
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
            CommunityEditor subModel ->
                CommunityEditor.subscriptions subModel
                    |> Sub.map GotCommunityEditorMsg

            CommunitySettingsInfo subModel ->
                CommunitySettingsInfo.subscriptions subModel
                    |> Sub.map GotCommunitySettingsInfoMsg

            CommunitySponsor subModel ->
                CommunitySponsor.subscriptions subModel
                    |> Sub.map GotCommunitySponsorMsg

            ShopEditor _ subModel ->
                ShopEditor.subscriptions subModel
                    |> Sub.map GotShopEditorMsg

            ActionEditor subModel ->
                ActionEditor.subscriptions subModel
                    |> Sub.map GotActionEditorMsg

            ObjectiveEditor subModel ->
                ObjectiveEditor.subscriptions subModel
                    |> Sub.map GotObjectiveEditorMsg

            ProfileEditor subModel ->
                ProfileEditor.subscriptions subModel
                    |> Sub.map GotProfileEditorMsg

            Transfer subModel ->
                Transfer.subscriptions subModel
                    |> Sub.map GotTransferMsg

            _ ->
                Sub.none
        ]



-- MODEL


type alias Model =
    { session : Session
    , afterAuthMsg : Maybe { successMsg : Msg, errorMsg : Msg }
    , status : Status
    }


type Status
    = Redirect
    | NotFound
    | ComingSoon
    | PaymentHistory PaymentHistory.Model
    | Community CommunityPage.Model
    | CommunityEditor CommunityEditor.Model
    | CommunitySettings CommunitySettings.Model
    | CommunitySettingsFeatures CommunitySettingsFeatures.Model
    | CommunitySettingsInfo CommunitySettingsInfo.Model
    | CommunitySettingsCurrency CommunitySettingsCurrency.Model
    | CommunitySettingsSponsorship CommunitySettingsSponsorship.Model
    | CommunitySelector CommunitySelector.Model
    | CommunityThankYou
    | CommunitySponsor CommunitySponsor.Model
    | CommunitySupporters CommunitySupporters.Model
    | Objectives Objectives.Model
    | ObjectiveEditor ObjectiveEditor.Model
    | ActionEditor ActionEditor.Model
    | Claim Int Int Claim.Model
    | Notification Notification.Model
    | Dashboard Dashboard.Model
    | Login (Maybe Route) Login.Model
    | Profile Profile.Model
    | ProfileContributions ProfileContributions.Model
    | ProfileEditor ProfileEditor.Model
    | ProfileAddKyc ProfileAddKyc.Model
    | ProfileClaims ProfileClaims.Model
    | ProfileAddContact ProfileAddContact.Model
    | Register (Maybe String) (Maybe Route) Register.Model
    | Shop Shop.Filter Shop.Model
    | ShopEditor (Maybe String) ShopEditor.Model
    | ShopViewer Int ShopViewer.Model
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
    | GotPageMsg Page.Msg
    | GotNotificationMsg Notification.Msg
    | GotCommunityMsg CommunityPage.Msg
    | GotCommunityEditorMsg CommunityEditor.Msg
    | GotCommunitySettingsMsg CommunitySettings.Msg
    | GotCommunitySettingsFeaturesMsg CommunitySettingsFeatures.Msg
    | GotCommunitySettingsInfoMsg CommunitySettingsInfo.Msg
    | GotCommunitySettingsCurrencyMsg CommunitySettingsCurrency.Msg
    | GotCommunitySettingsSponsorshipMsg CommunitySettingsSponsorship.Msg
    | GotCommunitySponsorMsg CommunitySponsor.Msg
    | GotCommunitySupportersMsg CommunitySupporters.Msg
    | GotObjectivesMsg Objectives.Msg
    | GotActionEditorMsg ActionEditor.Msg
    | GotObjectiveEditorMsg ObjectiveEditor.Msg
    | GotVerifyClaimMsg Claim.Msg
    | GotDashboardMsg Dashboard.Msg
    | GotLoginMsg Login.Msg
    | GotPaymentHistoryMsg PaymentHistory.Msg
    | GotProfileMsg Profile.Msg
    | GotProfileEditorMsg ProfileEditor.Msg
    | GotProfileAddKycMsg ProfileAddKyc.Msg
    | GotProfileClaimsMsg ProfileClaims.Msg
    | GotProfileAddContactMsg ProfileAddContact.Msg
    | GotRegisterMsg Register.Msg
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
                                []
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
                |> UR.toModelCmd
                    (\extMsg m ->
                        case extMsg of
                            Page.LoggedInExternalMsg LoggedIn.AuthenticationSucceed ->
                                case m.afterAuthMsg of
                                    Nothing ->
                                        ( m, Cmd.none )

                                    Just aMsg ->
                                        update aMsg.successMsg { m | afterAuthMsg = Nothing }

                            Page.LoggedInExternalMsg LoggedIn.AuthenticationFailed ->
                                case m.afterAuthMsg of
                                    Nothing ->
                                        ( m, Cmd.none )

                                    Just aMsg ->
                                        update aMsg.errorMsg { m | afterAuthMsg = Nothing }

                            Page.LoggedInExternalMsg (LoggedIn.Broadcast broadcastMsg) ->
                                ( m, broadcast broadcastMsg m.status )

                            Page.GuestBroadcastMsg broadcastMsg ->
                                ( m, broadcastGuest broadcastMsg m.status )
                    )
                    msgToString

        ( GotRegisterMsg subMsg, Register maybeInvitation maybeRedirect subModel ) ->
            -- Will return  a function expecting a Guest Model
            Register.update maybeInvitation subMsg subModel
                -- will return a function expecting an UpdateResult
                -- The composition operator will take the result of the above function and use as
                -- an input for the below function
                >> updateGuestUResult (Register maybeInvitation maybeRedirect) GotRegisterMsg model
                -- provides the above composed function with the initial guest input
                |> withGuest

        ( GotPaymentHistoryMsg subMsg, PaymentHistory subModel ) ->
            PaymentHistory.update subMsg subModel
                >> updateLoggedInUResult PaymentHistory GotPaymentHistoryMsg model
                |> withLoggedIn

        ( GotLoginMsg subMsg, Login maybeRedirect subModel ) ->
            Login.update subMsg subModel
                >> updateGuestUResult (Login maybeRedirect) GotLoginMsg model
                |> withGuest

        ( GotNotificationMsg subMsg, Notification subModel ) ->
            -- Will return a function expecting a LoggedIn Model
            Notification.update subMsg subModel
                -- will return a function expecting an UpdateResult
                -- The composition operator will take the result of the above function and use as
                -- an input for the below function
                >> updateLoggedInUResult Notification GotNotificationMsg model
                -- provides the above composed function with the LoggedInModel
                |> withLoggedIn

        ( GotCommunityMsg subMsg, Community subModel ) ->
            CommunityPage.update subMsg subModel
                >> updateLoggedInUResult Community GotCommunityMsg model
                |> withLoggedIn

        ( GotCommunityEditorMsg subMsg, CommunityEditor subModel ) ->
            CommunityEditor.update subMsg subModel
                >> updateLoggedInUResult CommunityEditor GotCommunityEditorMsg model
                |> withLoggedIn

        ( GotObjectivesMsg subMsg, Objectives subModel ) ->
            Objectives.update subMsg subModel
                >> updateLoggedInUResult Objectives GotObjectivesMsg model
                |> withLoggedIn

        ( GotObjectiveEditorMsg subMsg, ObjectiveEditor subModel ) ->
            ObjectiveEditor.update subMsg subModel
                >> updateLoggedInUResult ObjectiveEditor GotObjectiveEditorMsg model
                |> withLoggedIn

        ( GotDashboardMsg subMsg, Dashboard subModel ) ->
            Dashboard.update subMsg subModel
                >> updateLoggedInUResult Dashboard GotDashboardMsg model
                |> withLoggedIn

        ( GotProfileMsg subMsg, Profile subModel ) ->
            Profile.update subMsg subModel
                >> updateLoggedInUResult Profile GotProfileMsg model
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

        ( GotCommunitySettingsFeaturesMsg subMsg, CommunitySettingsFeatures subModel ) ->
            CommunitySettingsFeatures.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsFeatures GotCommunitySettingsFeaturesMsg model
                |> withLoggedIn

        ( GotCommunitySettingsInfoMsg subMsg, CommunitySettingsInfo subModel ) ->
            CommunitySettingsInfo.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsInfo GotCommunitySettingsInfoMsg model
                |> withLoggedIn

        ( GotCommunitySettingsCurrencyMsg subMsg, CommunitySettingsCurrency subModel ) ->
            CommunitySettingsCurrency.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsCurrency GotCommunitySettingsCurrencyMsg model
                |> withLoggedIn

        ( GotCommunitySettingsSponsorshipMsg subMsg, CommunitySettingsSponsorship subModel ) ->
            CommunitySettingsSponsorship.update subMsg subModel
                >> updateLoggedInUResult CommunitySettingsSponsorship GotCommunitySettingsSponsorshipMsg model
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

        ( GotActionEditorMsg subMsg, ActionEditor subModel ) ->
            ActionEditor.update subMsg subModel
                >> updateLoggedInUResult ActionEditor GotActionEditorMsg model
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
            spawnMessage msg

        Nothing ->
            Cmd.none


broadcast : LoggedIn.BroadcastMsg -> Status -> Cmd Msg
broadcast broadcastMessage status =
    let
        maybeMsg =
            case status of
                Dashboard _ ->
                    Dashboard.receiveBroadcast broadcastMessage
                        |> Maybe.map GotDashboardMsg

                Shop _ _ ->
                    Shop.receiveBroadcast broadcastMessage
                        |> Maybe.map GotShopMsg

                ShopViewer _ _ ->
                    ShopViewer.receiveLoggedInBroadcast broadcastMessage
                        |> Maybe.map (ShopViewer.AsLoggedInMsg >> GotShopViewerMsg)

                Transfer _ ->
                    Transfer.receiveBroadcast broadcastMessage
                        |> Maybe.map GotTransferMsg

                Analysis _ ->
                    Analysis.receiveBroadcast broadcastMessage
                        |> Maybe.map GotAnalysisMsg

                ProfileClaims _ ->
                    ProfileClaims.receiveBroadcast broadcastMessage
                        |> Maybe.map GotProfileClaimsMsg

                ActionEditor _ ->
                    ActionEditor.receiveBroadcast broadcastMessage
                        |> Maybe.map GotActionEditorMsg

                Objectives _ ->
                    Objectives.receiveBroadcast broadcastMessage
                        |> Maybe.map GotObjectivesMsg

                Community _ ->
                    CommunityPage.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunityMsg

                CommunitySettingsFeatures _ ->
                    CommunitySettingsFeatures.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsFeaturesMsg

                CommunitySettingsInfo _ ->
                    CommunitySettingsInfo.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsInfoMsg

                CommunitySettingsCurrency _ ->
                    CommunitySettingsCurrency.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsCurrencyMsg

                CommunitySettingsSponsorship _ ->
                    CommunitySettingsSponsorship.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsSponsorshipMsg

                CommunitySettings _ ->
                    CommunitySettings.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySettingsMsg

                CommunitySupporters _ ->
                    CommunitySupporters.receiveBroadcast broadcastMessage
                        |> Maybe.map GotCommunitySupportersMsg

                ObjectiveEditor _ ->
                    ObjectiveEditor.receiveBroadcast broadcastMessage
                        |> Maybe.map GotObjectiveEditorMsg

                ProfileAddContact _ ->
                    ProfileAddContact.receiveBroadcast broadcastMessage
                        |> Maybe.map GotProfileAddContactMsg

                ProfileEditor _ ->
                    ProfileEditor.receiveBroadcast broadcastMessage
                        |> Maybe.map GotProfileEditorMsg

                Profile _ ->
                    Profile.receiveBroadcast broadcastMessage
                        |> Maybe.map GotProfileMsg

                Invite _ ->
                    Invite.receiveBroadcast broadcastMessage
                        |> Maybe.map GotInviteMsg

                Join _ ->
                    Join.receiveBroadcast broadcastMessage
                        |> Maybe.map GotJoinMsg

                PaymentHistory _ ->
                    PaymentHistory.receiveBroadcast broadcastMessage
                        |> Maybe.map GotPaymentHistoryMsg

                _ ->
                    Nothing
    in
    case maybeMsg of
        Just msg ->
            spawnMessage msg

        Nothing ->
            Cmd.none


spawnMessage : Msg -> Cmd Msg
spawnMessage msg =
    Task.succeed ()
        |> Task.perform (\_ -> msg)


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
                        Guest.LoggedIn privateKey { user, token } ->
                            let
                                shared =
                                    guest.shared

                                userWithCommunity =
                                    { user
                                        | communities =
                                            case guest.community of
                                                RemoteData.Success community ->
                                                    if List.any (\c -> c.symbol == community.symbol) user.communities then
                                                        user.communities

                                                    else
                                                        { symbol = community.symbol
                                                        , name = community.name
                                                        , logo = community.logo
                                                        , subdomain = community.subdomain
                                                        , hasShop = community.hasShop
                                                        , hasActions = community.hasObjectives
                                                        , hasKyc = community.hasKyc
                                                        }
                                                            :: user.communities

                                                _ ->
                                                    user.communities
                                    }

                                ( session, cmd ) =
                                    LoggedIn.initLogin shared (Just privateKey) userWithCommunity token

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
                                :: Route.pushUrl guest.shared.navKey redirectRoute
                                :: Log.addBreadcrumb msgToString
                                    { type_ = Log.InfoBreadcrumb
                                    , category = Ignored
                                    , message = "Guest logged in"
                                    , data = Dict.fromList [ ( "username", Eos.Account.encodeName user.account ) ]
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
                                    broadcast broadcastMsg m.status
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
                    , Cmd.map toMsg updateResult.externalCmd
                        :: Cmd.map (Page.GotLoggedInMsg >> GotPageMsg) updateResult.cmd
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

        Community _ ->
            Just Route.Community

        CommunityEditor _ ->
            Just Route.NewCommunity

        CommunitySettings _ ->
            Just Route.CommunitySettings

        CommunitySettingsFeatures _ ->
            Just Route.CommunitySettingsFeatures

        CommunitySettingsInfo _ ->
            Just Route.CommunitySettingsInfo

        CommunitySettingsCurrency _ ->
            Just Route.CommunitySettingsCurrency

        CommunitySettingsSponsorship _ ->
            Just Route.CommunitySettingsSponsorship

        CommunitySelector subModel ->
            Just (Route.CommunitySelector subModel.maybeRedirect)

        CommunityThankYou ->
            Just Route.CommunityThankYou

        CommunitySponsor _ ->
            Just Route.CommunitySponsor

        CommunitySupporters _ ->
            Just Route.CommunitySupporters

        Objectives _ ->
            Just Route.Objectives

        ObjectiveEditor subModel ->
            case subModel.objectiveId of
                Nothing ->
                    Just Route.NewObjective

                Just objectiveId ->
                    Just (Route.EditObjective objectiveId)

        ActionEditor subModel ->
            case subModel.actionId of
                Nothing ->
                    Just (Route.NewAction subModel.objectiveId)

                Just actionId ->
                    Just (Route.EditAction subModel.objectiveId actionId)

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

        Shop filter _ ->
            Just (Route.Shop filter)

        ShopEditor maybeSaleId _ ->
            case maybeSaleId of
                Nothing ->
                    Just Route.NewSale

                Just saleId ->
                    Just (Route.EditSale saleId)

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
                                { loggedIn | routeHistory = newRoute :: rest }

                            else
                                { loggedIn | routeHistory = newRoute :: routeHistory }

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
                    , if guest.isLoggingIn then
                        Cmd.none

                      else
                        Route.replaceUrl shared.navKey (Route.Join (Just route))
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

        Just (Route.Login maybeInvitation maybeRedirect) ->
            Login.init
                >> updateStatusWith (Login maybeRedirect) GotLoginMsg model
                |> withGuest maybeInvitation maybeRedirect

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
                >> updateStatusWith Notification GotNotificationMsg model
                |> withLoggedIn Route.Notification

        Just (Route.Profile profileName) ->
            (\loggedIn -> Profile.init loggedIn profileName)
                >> updateStatusWith Profile GotProfileMsg model
                |> withLoggedIn (Route.Profile profileName)

        Just (Route.ProfileContributions profileName) ->
            (\_ -> ( ProfileContributions.init profileName, Cmd.none ))
                >> updateStatusWith ProfileContributions identity model
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

        Just Route.Community ->
            (\l -> CommunityPage.init l)
                >> updateStatusWith Community GotCommunityMsg model
                |> withLoggedIn Route.Community

        Just Route.CommunitySettings ->
            CommunitySettings.init
                >> updateStatusWith CommunitySettings GotCommunitySettingsMsg model
                |> withLoggedIn Route.CommunitySettings

        Just Route.CommunitySettingsFeatures ->
            CommunitySettingsFeatures.init
                >> updateStatusWith CommunitySettingsFeatures GotCommunitySettingsFeaturesMsg model
                |> withLoggedIn Route.CommunitySettingsFeatures

        Just Route.CommunitySettingsInfo ->
            CommunitySettingsInfo.init
                >> updateStatusWith CommunitySettingsInfo GotCommunitySettingsInfoMsg model
                |> withLoggedIn Route.CommunitySettingsInfo

        Just Route.CommunitySettingsCurrency ->
            CommunitySettingsCurrency.init
                >> updateStatusWith CommunitySettingsCurrency GotCommunitySettingsCurrencyMsg model
                |> withLoggedIn Route.CommunitySettingsCurrency

        Just Route.CommunitySettingsSponsorship ->
            CommunitySettingsSponsorship.init
                >> updateStatusWith CommunitySettingsSponsorship GotCommunitySettingsSponsorshipMsg model
                |> withLoggedIn Route.CommunitySettingsSponsorship

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
                >> updateStatusWith CommunitySupporters GotCommunitySupportersMsg model
                |> withLoggedIn Route.CommunitySupporters

        Just Route.NewCommunity ->
            CommunityEditor.init
                >> updateStatusWith CommunityEditor GotCommunityEditorMsg model
                |> withLoggedIn Route.NewCommunity

        Just Route.Objectives ->
            Objectives.init
                >> updateStatusWith Objectives GotObjectivesMsg model
                |> withLoggedIn Route.Objectives

        Just Route.NewObjective ->
            ObjectiveEditor.initNew
                >> updateStatusWith ObjectiveEditor GotObjectiveEditorMsg model
                |> withLoggedIn Route.NewObjective

        Just (Route.EditObjective objectiveId) ->
            (\l -> ObjectiveEditor.initEdit l objectiveId)
                >> updateStatusWith ObjectiveEditor GotObjectiveEditorMsg model
                |> withLoggedIn (Route.EditObjective objectiveId)

        Just (Route.NewAction objectiveId) ->
            (\l -> ActionEditor.init l objectiveId Nothing)
                >> updateStatusWith ActionEditor GotActionEditorMsg model
                |> withLoggedIn (Route.NewAction objectiveId)

        Just (Route.EditAction objectiveId actionId) ->
            (\l -> ActionEditor.init l objectiveId (Just actionId))
                >> updateStatusWith ActionEditor GotActionEditorMsg model
                |> withLoggedIn (Route.EditAction objectiveId actionId)

        Just (Route.Claim objectiveId actionId claimId) ->
            (\l -> Claim.init l claimId)
                >> updateStatusWith (Claim objectiveId actionId) GotVerifyClaimMsg model
                |> withLoggedIn (Route.Claim objectiveId actionId claimId)

        Just (Route.Shop maybeFilter) ->
            (\l -> Shop.init l maybeFilter)
                >> updateStatusWith (Shop maybeFilter) GotShopMsg model
                |> withLoggedIn (Route.Shop maybeFilter)

        Just Route.NewSale ->
            ShopEditor.initCreate
                >> updateStatusWith (ShopEditor Nothing) GotShopEditorMsg model
                |> withLoggedIn Route.NewSale

        Just (Route.EditSale saleId) ->
            (\l -> ShopEditor.initUpdate saleId l)
                >> updateStatusWith (ShopEditor (Just saleId)) GotShopEditorMsg model
                |> withLoggedIn (Route.EditSale saleId)

        Just (Route.ViewSale saleId) ->
            ShopViewer.init session saleId
                |> updateStatusWith (ShopViewer saleId) GotShopViewerMsg model

        Just (Route.ViewTransfer transferId) ->
            (\l -> ViewTransfer.init l transferId)
                >> updateStatusWith (ViewTransfer transferId) GotViewTransferScreenMsg model
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
                >> updateStatusWith Transfer GotTransferMsg model
                |> withLoggedIn (Route.Transfer maybeTo)

        Just Route.Analysis ->
            (\l -> Analysis.init l)
                >> updateStatusWith Analysis GotAnalysisMsg model
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

        "GotCommunityEditorMsg" :: rAddress ->
            Maybe.map GotCommunityEditorMsg
                (CommunityEditor.jsAddressToMsg rAddress val)

        "GotObjectiveEditorMsg" :: rAddress ->
            Maybe.map GotObjectiveEditorMsg
                (ObjectiveEditor.jsAddressToMsg rAddress val)

        "GotShopEditorMsg" :: rAddress ->
            Maybe.map GotShopEditorMsg
                (ShopEditor.jsAddressToMsg rAddress val)

        "GotShopViewerMsg" :: rAddress ->
            Maybe.map GotShopViewerMsg
                (ShopViewer.jsAddressToMsg rAddress val)

        "GotRegisterMsg" :: rAddress ->
            Maybe.map GotRegisterMsg
                (Register.jsAddressToMsg rAddress val)

        "GotShopMsg" :: rAddress ->
            Maybe.map GotShopMsg
                (Shop.jsAddressToMsg rAddress val)

        "GotProfileMsg" :: rAddress ->
            Maybe.map GotProfileMsg
                (Profile.jsAddressToMsg rAddress val)

        "GotCommunitySettingsFeaturesMsg" :: rAddress ->
            Maybe.map GotCommunitySettingsFeaturesMsg
                (CommunitySettingsFeatures.jsAddressToMsg rAddress val)

        "GotCommunitySettingsInfoMsg" :: rAddress ->
            Maybe.map GotCommunitySettingsInfoMsg
                (CommunitySettingsInfo.jsAddressToMsg rAddress val)

        "GotCommunitySettingsCurrencyMsg" :: rAddress ->
            Maybe.map GotCommunitySettingsCurrencyMsg
                (CommunitySettingsCurrency.jsAddressToMsg rAddress val)

        "GotActionEditorMsg" :: rAddress ->
            Maybe.map GotActionEditorMsg
                (ActionEditor.jsAddressToMsg rAddress val)

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

        GotCommunityMsg subMsg ->
            "GotCommunityMsg" :: CommunityPage.msgToString subMsg

        GotCommunityEditorMsg subMsg ->
            "GotCommunityEditorMsg" :: CommunityEditor.msgToString subMsg

        GotCommunitySettingsMsg subMsg ->
            "GotCommunitySettingsMsg" :: CommunitySettings.msgToString subMsg

        GotCommunitySettingsFeaturesMsg subMsg ->
            "GotCommunitySettingsFeaturesMsg" :: CommunitySettingsFeatures.msgToString subMsg

        GotCommunitySettingsInfoMsg subMsg ->
            "GotCommunitySettingsInfoMsg" :: CommunitySettingsInfo.msgToString subMsg

        GotCommunitySettingsCurrencyMsg subMsg ->
            "GotCommunitySettingsCurrencyMsg" :: CommunitySettingsCurrency.msgToString subMsg

        GotCommunitySettingsSponsorshipMsg subMsg ->
            "GotCommunitySettingsSponsorshipMsg" :: CommunitySettingsSponsorship.msgToString subMsg

        GotCommunitySponsorMsg subMsg ->
            "GotCommunitySponsorMsg" :: CommunitySponsor.msgToString subMsg

        GotCommunitySupportersMsg subMsg ->
            "GotCommunitySupportersMsg" :: CommunitySupporters.msgToString subMsg

        GotObjectivesMsg subMsg ->
            "GotObjectivesMsg" :: Objectives.msgToString subMsg

        GotObjectiveEditorMsg subMsg ->
            "GotObjectiveEditorMsg" :: ObjectiveEditor.msgToString subMsg

        GotActionEditorMsg subMsg ->
            "GotActionEditorMsg" :: ActionEditor.msgToString subMsg

        GotVerifyClaimMsg subMsg ->
            "GotVerifyClaimMsg" :: Claim.msgToString subMsg

        GotNotificationMsg subMsg ->
            "GotNotificationMsg" :: Notification.msgToString subMsg

        GotDashboardMsg subMsg ->
            "GotDashboardMsg" :: Dashboard.msgToString subMsg

        GotLoginMsg subMsg ->
            "GotLoginMsg" :: Login.msgToString subMsg

        GotPaymentHistoryMsg subMsg ->
            "GotPaymentHistoryMsg" :: PaymentHistory.msgToString subMsg

        GotProfileMsg subMsg ->
            "GotProfileMsg" :: Profile.msgToString subMsg

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

        Login _ subModel ->
            viewGuest subModel Guest.Login GotLoginMsg Login.view

        Notification subModel ->
            viewLoggedIn subModel LoggedIn.Notification GotNotificationMsg Notification.view

        Community subModel ->
            viewLoggedIn subModel LoggedIn.Community GotCommunityMsg CommunityPage.view

        CommunitySettings subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettings GotCommunitySettingsMsg CommunitySettings.view

        CommunitySettingsFeatures subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsFeatures GotCommunitySettingsFeaturesMsg CommunitySettingsFeatures.view

        CommunitySettingsInfo subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsInfo GotCommunitySettingsInfoMsg CommunitySettingsInfo.view

        CommunitySettingsCurrency subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsCurrency GotCommunitySettingsCurrencyMsg CommunitySettingsCurrency.view

        CommunitySettingsSponsorship subModel ->
            viewLoggedIn subModel LoggedIn.CommunitySettingsSponsorship GotCommunitySettingsSponsorshipMsg CommunitySettingsSponsorship.view

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

        Objectives subModel ->
            viewLoggedIn subModel LoggedIn.Objectives GotObjectivesMsg Objectives.view

        ObjectiveEditor subModel ->
            viewLoggedIn subModel LoggedIn.ObjectiveEditor GotObjectiveEditorMsg ObjectiveEditor.view

        ActionEditor subModel ->
            viewLoggedIn subModel LoggedIn.ActionEditor GotActionEditorMsg ActionEditor.view

        Claim _ _ subModel ->
            viewLoggedIn subModel LoggedIn.Claim GotVerifyClaimMsg Claim.view

        Dashboard subModel ->
            viewLoggedIn subModel LoggedIn.Dashboard GotDashboardMsg Dashboard.view

        Profile subModel ->
            viewLoggedIn subModel LoggedIn.Profile GotProfileMsg Profile.view

        ProfileContributions subModel ->
            viewLoggedIn subModel LoggedIn.ProfileContributions identity ProfileContributions.view

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
