module Main exposing (main)

import Api
import Auth
import Browser
import Browser.Navigation as Nav
import Community
import Flags
import Html exposing (Html, div, iframe, text)
import Html.Attributes exposing (id, style)
import Http
import Json.Decode as Decode exposing (Value)
import Log
import Page exposing (Session)
import Page.ComingSoon as ComingSoon
import Page.Community as Community
import Page.Community.ActionEditor as ActionEditor
import Page.Community.Editor as CommunityEditor
import Page.Community.Explore as CommunityExplore
import Page.Community.Invite as Invite
import Page.Community.ObjectiveEditor as ObjectiveEditor
import Page.Community.Objectives as Objectives
import Page.Community.VerifyClaim as VerifyClaim
import Page.Dashboard as Dashboard
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Notification as Notification
import Page.Profile as Profile
import Page.Register as Register
import Page.Shop as Shop
import Page.Shop.Editor as ShopEditor
import Page.Shop.Viewer as ShopViewer
import Page.Transfer as Transfer
import Ports
import Route exposing (Route)
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Shop
import UpdateResult as UR exposing (UpdateResult)
import Url exposing (Url)


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view =
            \model ->
                { title = "Cambiatus"
                , body =
                    [ view model
                    ]
                }
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
                    Page.init flags navKey
                        |> UR.map identity GotPageMsg (\_ uR -> uR)
                        |> UR.toModelCmd (\_ m -> ( m, Cmd.none )) msgToString

                Err e ->
                    Page.init Flags.default navKey
                        |> UR.map identity GotPageMsg (\_ uR -> uR)
                        |> UR.logDecodeError Ignored e
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
            Login subModel ->
                Login.subscriptions subModel
                    |> Sub.map GotLoginMsg

            Dashboard subModel ->
                Dashboard.subscriptions subModel
                    |> Sub.map GotDashboardMsg

            Register _ subModel ->
                Register.subscriptions subModel
                    |> Sub.map GotRegisterMsg

            Community subModel ->
                Community.subscriptions subModel
                    |> Sub.map GotCommunityMsg

            CommunityEditor subModel ->
                CommunityEditor.subscriptions subModel
                    |> Sub.map GotCommunityEditorMsg

            ShopEditor _ subModel ->
                ShopEditor.subscriptions subModel
                    |> Sub.map GotShopEditorMsg

            ShopViewer _ subModel ->
                ShopViewer.subscriptions subModel
                    |> Sub.map GotShopViewerMsg

            _ ->
                Sub.none
        ]



-- MODEL


type alias Model =
    { session : Session
    , afterAuthMsg : Maybe Msg
    , status : Status
    }


type Status
    = Redirect
    | NotFound
    | ComingSoon
    | Community Community.Model
    | CommunityEditor CommunityEditor.Model
    | Objectives Objectives.Model
    | ObjectiveEditor ObjectiveEditor.Model
    | ActionEditor ActionEditor.Model
    | VerifyClaim VerifyClaim.Model
    | CommunityExplore CommunityExplore.Model
    | Notification Notification.Model
    | Dashboard Dashboard.Model
    | Login Login.Model
    | Profile Profile.Model
    | Register (Maybe String) Register.Model
    | Shop Shop.Filter Shop.Model
    | ShopEditor (Maybe String) ShopEditor.Model
    | ShopViewer String ShopViewer.Model
    | Transfer Int Transfer.Model
    | Invite Invite.Model



-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotJavascriptData Value
    | GotPageMsg Page.Msg
    | GotNotificationMsg Notification.Msg
    | GotCommunityMsg Community.Msg
    | GotCommunityEditorMsg CommunityEditor.Msg
    | GotObjectivesMsg Objectives.Msg
    | GotActionEditorMsg ActionEditor.Msg
    | GotObjectiveEditorMsg ObjectiveEditor.Msg
    | GotVerifyClaimMsg VerifyClaim.Msg
    | GotCommunityExploreMsg CommunityExplore.Msg
    | GotDashboardMsg Dashboard.Msg
    | GotLoginMsg Login.Msg
    | GotProfileMsg Profile.Msg
    | GotRegisterMsg Register.Msg
    | GotShopMsg Shop.Msg
    | GotShopEditorMsg ShopEditor.Msg
    | GotUpdatedBalances (Result Http.Error (List Community.Balance))
    | GotShopViewerMsg ShopViewer.Msg
    | GotTransferScreenMsg Transfer.Msg
    | GotInviteMsg Invite.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        withGuest fn =
            case model.session of
                Page.Guest guest ->
                    fn guest

                Page.LoggedIn _ ->
                    ( model
                    , Log.impossible "loggedIn"
                    )

        withLoggedIn fn =
            case model.session of
                Page.Guest _ ->
                    ( model
                    , Log.impossible "notLoggedIn"
                    )

                Page.LoggedIn loggedIn ->
                    fn loggedIn
    in
    case ( msg, model.status ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (.navKey (Page.toShared model.session)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
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
                    Maybe.map
                        (\newMsg -> update newMsg model)
                        (jsAddressToMsg jsAddress val)
                        |> Maybe.withDefault
                            ([ "[Main] No handler for: "
                             , String.join "." jsAddress
                             ]
                                |> String.concat
                                |> Log.impossible
                                |> Tuple.pair model
                            )

                Err decodeError ->
                    ( model
                    , Log.decodeError decodeError
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
                                        update aMsg { m | afterAuthMsg = Nothing }

                            Page.LoggedInExternalMsg LoggedIn.AuthenticationFailed ->
                                ( { m | afterAuthMsg = Nothing }, Cmd.none )
                    )
                    msgToString

        ( GotRegisterMsg subMsg, Register maybeInvitation subModel ) ->
            -- Will return  a function expecting a Guest Model
            Register.update maybeInvitation subMsg subModel
                -- will return a function expecting an UpdateResult
                -- The composition operator will take the result of the above function and use as
                -- an input for the below function
                >> updateGuestUResult (Register maybeInvitation) GotRegisterMsg model
                -- provides the above composed function with the initial guest input
                |> withGuest

        ( GotLoginMsg subMsg, Login subModel ) ->
            Login.update subMsg subModel
                >> updateGuestUResult Login GotLoginMsg model
                |> withGuest

        ( GotNotificationMsg subMsg, Notification subModel ) ->
            -- Will return afunction expecting a LoggedIn Model
            Notification.update subMsg subModel
                -- will return a function expecting an UpdateResult
                -- The composition operator will take the result of the above function and use as
                -- an input for the below function
                >> updateLoggedInUResult Notification GotNotificationMsg model
                -- provides the above composed function with the LoggedInModel
                |> withLoggedIn

        ( GotCommunityMsg subMsg, Community subModel ) ->
            Community.update subMsg subModel
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

        ( GotCommunityExploreMsg subMsg, CommunityExplore subModel ) ->
            CommunityExplore.update subMsg subModel
                >> updateLoggedInUResult CommunityExplore GotCommunityExploreMsg model
                |> withLoggedIn

        ( GotDashboardMsg subMsg, Dashboard subModel ) ->
            Dashboard.update subMsg subModel
                >> updateLoggedInUResult Dashboard GotDashboardMsg model
                |> withLoggedIn

        ( GotProfileMsg subMsg, Profile subModel ) ->
            Profile.update subMsg subModel
                >> updateLoggedInUResult Profile GotProfileMsg model
                |> withLoggedIn

        ( GotShopMsg subMsg, Shop maybeFilter subModel ) ->
            Shop.update subMsg subModel
                >> updateLoggedInUResult (Shop maybeFilter) GotShopMsg model
                |> withLoggedIn

        ( GotShopEditorMsg subMsg, ShopEditor id subModel ) ->
            ShopEditor.update subMsg subModel
                >> updateLoggedInUResult (ShopEditor id) GotShopEditorMsg model
                |> withLoggedIn

        ( GotUpdatedBalances (Ok bals), _ ) ->
            case model.session of
                Page.LoggedIn session ->
                    let
                        new_session =
                            { session | balances = bals }

                        new_model =
                            { model | session = Page.LoggedIn new_session }
                    in
                    ( new_model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( GotUpdatedBalances (Err err), _ ) ->
            ( model
            , Log.httpError err
            )

        ( GotShopViewerMsg subMsg, ShopViewer saleId subModel ) ->
            ShopViewer.update subMsg subModel
                >> updateLoggedInUResult (ShopViewer saleId) GotShopViewerMsg model
                |> withLoggedIn

        ( GotActionEditorMsg subMsg, ActionEditor subModel ) ->
            ActionEditor.update subMsg subModel
                >> updateLoggedInUResult ActionEditor GotActionEditorMsg model
                |> withLoggedIn

        ( GotVerifyClaimMsg subMsg, VerifyClaim subModel ) ->
            VerifyClaim.update subMsg subModel
                >> updateLoggedInUResult VerifyClaim GotVerifyClaimMsg model
                |> withLoggedIn

        ( GotTransferScreenMsg subMsg, Transfer transferId subModel ) ->
            Transfer.update subMsg subModel
                >> updateLoggedInUResult (Transfer transferId) GotTransferScreenMsg model
                |> withLoggedIn

        ( GotInviteMsg subMsg, Invite subModel ) ->
            Invite.update model.session subMsg subModel
                |> updateLoggedInUResult Invite GotInviteMsg model

        ( _, _ ) ->
            ( model
            , Log.impossible ("Main" :: msgToString msg |> String.join ".")
            )


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


updateGuestUResult : (subModel -> Status) -> (subMsg -> Msg) -> Model -> UpdateResult subModel subMsg Guest.External -> ( Model, Cmd Msg )
updateGuestUResult toStatus toMsg model uResult =
    List.foldl
        (\commExtMsg ( m, cmds_ ) ->
            case commExtMsg of
                Guest.UpdatedGuest guest ->
                    ( { m | session = Page.Guest guest }
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
                        ++ List.map (Log.map toMsg >> Log.send msgToString) uResult.logs
                        ++ cmds_
                    )
                )
           )


updateLoggedInUResult : (subModel -> Status) -> (subMsg -> Msg) -> Model -> UpdateResult subModel subMsg (LoggedIn.External subMsg) -> ( Model, Cmd Msg )
updateLoggedInUResult toStatus toMsg model uResult =
    List.foldl
        (\commExtMsg ( m, cmds_ ) ->
            case commExtMsg of
                LoggedIn.UpdateBalances ->
                    let
                        update_cmd =
                            case m.session of
                                Page.LoggedIn loggedIn ->
                                    let
                                        u_command =
                                            Api.getBalances loggedIn.shared loggedIn.accountName GotUpdatedBalances
                                    in
                                    u_command

                                _ ->
                                    Cmd.none
                    in
                    ( m, cmds_ ++ [ update_cmd ] )

                LoggedIn.UpdatedLoggedIn loggedIn ->
                    ( { m | session = Page.LoggedIn loggedIn }
                    , cmds_
                    )

                LoggedIn.RequiredAuthentication maybeMsg ->
                    case m.session of
                        Page.LoggedIn loggedIn ->
                            ( { m
                                | session = Page.LoggedIn (LoggedIn.askedAuthentication loggedIn)
                                , afterAuthMsg = Maybe.map toMsg maybeMsg
                              }
                            , cmds_
                            )

                        _ ->
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
                        ++ List.map (Log.map toMsg >> Log.send msgToString) uResult.logs
                        ++ cmds_
                    )
                )
           )


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

        afterLoginRedirect maybeRedirect =
            let
                addRedirect redirect =
                    case model.session of
                        Page.LoggedIn _ ->
                            model

                        Page.Guest guest ->
                            { model
                                | session =
                                    Guest.addAfterLoginRedirect redirect guest
                                        |> Page.Guest
                                , status = Redirect
                            }
            in
            Maybe.map addRedirect maybeRedirect
                |> Maybe.withDefault model

        withGuest init_ update_ maybeRedirect =
            let
                model_ =
                    afterLoginRedirect maybeRedirect

                fn =
                    init_
                        >> update_ model_
            in
            case session of
                Page.Guest guest ->
                    fn guest

                Page.LoggedIn _ ->
                    let
                        redirect =
                            case maybeRedirect of
                                Nothing ->
                                    Route.Dashboard

                                Just route_ ->
                                    route_
                    in
                    ( model_
                    , Route.replaceUrl shared.navKey redirect
                    )

        withLoggedIn route fn =
            case session of
                Page.LoggedIn loggedIn ->
                    fn loggedIn

                Page.Guest guest ->
                    case guest.profile of
                        Nothing ->
                            ( { model
                                | session =
                                    Guest.addAfterLoginRedirect route guest
                                        |> Page.Guest
                                , status = Redirect
                              }
                            , Route.replaceUrl shared.navKey (Route.Login (Just route))
                            )

                        Just profile ->
                            let
                                authModel =
                                    case model.status of
                                        Login subModel ->
                                            subModel.auth

                                        Register _ subModel ->
                                            Maybe.map
                                                (\r ->
                                                    Auth.initRegister r.privateKey
                                                )
                                                subModel.accountKeys
                                                |> Maybe.withDefault
                                                    (Auth.init guest.shared)

                                        _ ->
                                            Auth.init guest.shared

                                ( loggedIn, cmd ) =
                                    Page.login authModel profile guest

                                ( newModel, newCmd ) =
                                    fn loggedIn
                            in
                            ( { newModel | session = Page.LoggedIn loggedIn }
                            , Cmd.batch
                                [ Cmd.map GotPageMsg cmd
                                , newCmd
                                ]
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
            withGuest
                Register.init
                (updateStatusWith (Register invitation) GotRegisterMsg)
                maybeRedirect

        Just (Route.Login maybeRedirect) ->
            withGuest
                Login.init
                (updateStatusWith Login GotLoginMsg)
                maybeRedirect

        Just (Route.LoginWithPrivateKey maybeRedirect) ->
            withGuest
                Login.init
                (updateStatusWith Login GotLoginMsg)
                maybeRedirect

        Just Route.Logout ->
            Page.logout
                >> updateSessionWith GotPageMsg model
                |> withLoggedIn Route.Dashboard

        Just Route.Notification ->
            Notification.init
                >> updateStatusWith Notification GotNotificationMsg model
                |> withLoggedIn Route.Notification

        Just Route.Profile ->
            Profile.init
                >> updateStatusWith Profile GotProfileMsg model
                |> withLoggedIn Route.Profile

        Just Route.Dashboard ->
            Dashboard.init
                >> updateStatusWith Dashboard GotDashboardMsg model
                |> withLoggedIn Route.Dashboard

        Just (Route.Community symbol) ->
            (\l -> Community.init l symbol)
                >> updateStatusWith Community GotCommunityMsg model
                |> withLoggedIn (Route.Community symbol)

        Just Route.NewCommunity ->
            CommunityEditor.initNew
                >> updateStatusWith CommunityEditor GotCommunityEditorMsg model
                |> withLoggedIn Route.NewCommunity

        Just (Route.EditCommunity symbol) ->
            (\l -> CommunityEditor.initEdit l symbol)
                >> updateStatusWith CommunityEditor GotCommunityEditorMsg model
                |> withLoggedIn (Route.EditCommunity symbol)

        Just (Route.Objectives symbol) ->
            (\l -> Objectives.init l symbol)
                >> updateStatusWith Objectives GotObjectivesMsg model
                |> withLoggedIn (Route.Objectives symbol)

        Just (Route.NewObjective symbol) ->
            (\l -> ObjectiveEditor.initNew l symbol)
                >> updateStatusWith ObjectiveEditor GotObjectiveEditorMsg model
                |> withLoggedIn (Route.NewObjective symbol)

        Just (Route.EditObjective symbol objectiveId) ->
            (\l -> ObjectiveEditor.initEdit l symbol objectiveId)
                >> updateStatusWith ObjectiveEditor GotObjectiveEditorMsg model
                |> withLoggedIn (Route.EditObjective symbol objectiveId)

        Just (Route.NewAction symbol objectiveId) ->
            (\l -> ActionEditor.initNew l symbol objectiveId)
                >> updateStatusWith ActionEditor GotActionEditorMsg model
                |> withLoggedIn (Route.NewAction symbol objectiveId)

        Just (Route.EditAction symbol objectiveId actionId) ->
            (\l -> ActionEditor.initEdit l symbol objectiveId actionId)
                >> updateStatusWith ActionEditor GotActionEditorMsg model
                |> withLoggedIn (Route.EditAction symbol objectiveId actionId)

        Just (Route.VerifyClaim communityId objectiveId actionId claimId) ->
            (\l -> VerifyClaim.init l claimId)
                >> updateStatusWith VerifyClaim GotVerifyClaimMsg model
                |> withLoggedIn (Route.VerifyClaim communityId objectiveId actionId claimId)

        Just Route.Communities ->
            CommunityExplore.init
                >> updateStatusWith CommunityExplore GotCommunityExploreMsg model
                |> withLoggedIn Route.Communities

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
            (\l -> ShopViewer.init l saleId)
                >> updateStatusWith (ShopViewer saleId) GotShopViewerMsg model
                |> withLoggedIn (Route.ViewSale saleId)

        Just (Route.Transfer transferId) ->
            (\l -> Transfer.init l transferId)
                >> updateStatusWith (Transfer transferId) GotTransferScreenMsg model
                |> withLoggedIn (Route.Transfer transferId)

        Just (Route.Invite invitationId) ->
            Invite.init session invitationId
                |> updateStatusWith Invite GotInviteMsg model


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

        "GotCommunityMsg" :: rAddress ->
            Maybe.map GotCommunityMsg
                (Community.jsAddressToMsg rAddress val)

        "GotCommunityEditorMsg" :: rAddress ->
            Maybe.map GotCommunityEditorMsg
                (CommunityEditor.jsAddressToMsg rAddress val)

        "GotObjectiveEditorMsg" :: rAddress ->
            Maybe.map GotObjectiveEditorMsg
                (ObjectiveEditor.jsAddressToMsg rAddress val)

        "GotShopEditorMsg" :: rAddress ->
            Maybe.map GotShopEditorMsg
                (ShopEditor.jsAddressToMsg rAddress val)

        "GotRegisterMsg" :: rAddress ->
            Maybe.map GotRegisterMsg
                (Register.jsAddressToMsg rAddress val)

        "GotShopMsg" :: rAddress ->
            Maybe.map GotShopMsg
                (Shop.jsAddressToMsg rAddress val)

        "GotProfileMsg" :: rAddress ->
            Maybe.map GotProfileMsg
                (Profile.jsAddressToMsg rAddress val)

        "GotActionEditor" :: rAddress ->
            Maybe.map GotActionEditorMsg
                (ActionEditor.jsAddressToMsg rAddress val)

        "GotVerifyClaimMsg" :: rAddress ->
            Maybe.map GotVerifyClaimMsg
                (VerifyClaim.jsAddressToMsg rAddress val)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        ChangedRoute _ ->
            [ "ChangedRoute" ]

        ChangedUrl _ ->
            [ "ChangedUrl" ]

        ClickedLink _ ->
            [ "ClickedLink" ]

        GotJavascriptData _ ->
            [ "GotJavascriptData" ]

        GotPageMsg subMsg ->
            "GotPageMsg" :: Page.msgToString subMsg

        GotCommunityMsg subMsg ->
            "GotCommunityMsg" :: Community.msgToString subMsg

        GotCommunityEditorMsg subMsg ->
            "GotCommunityEditorMsg" :: CommunityEditor.msgToString subMsg

        GotObjectivesMsg subMsg ->
            "GotObjectives" :: Objectives.msgToString subMsg

        GotObjectiveEditorMsg subMsg ->
            "GotObjectiveEditorMsg" :: ObjectiveEditor.msgToString subMsg

        GotActionEditorMsg subMsg ->
            "GotActionEditor" :: ActionEditor.msgToString subMsg

        GotVerifyClaimMsg subMsg ->
            "GotVerifyClaimMsg" :: VerifyClaim.msgToString subMsg

        GotCommunityExploreMsg subMsg ->
            "GotCommunityExploreMsg" :: CommunityExplore.msgToString subMsg

        GotNotificationMsg subMsg ->
            "GotNotificationMsg" :: Notification.msgToString subMsg

        GotDashboardMsg subMsg ->
            "GotDashboardMsg" :: Dashboard.msgToString subMsg

        GotLoginMsg subMsg ->
            "GotLoginMsg" :: Login.msgToString subMsg

        GotProfileMsg subMsg ->
            "GotProfileMsg" :: Profile.msgToString subMsg

        GotRegisterMsg subMsg ->
            "GotRegisterMsg" :: Register.msgToString subMsg

        GotShopMsg subMsg ->
            "GotShopMsg" :: Shop.msgToString subMsg

        GotShopEditorMsg subMsg ->
            "GotShopEditorMsg" :: ShopEditor.msgToString subMsg

        GotUpdatedBalances _ ->
            [ "GotUpdatedBalances" ]

        GotShopViewerMsg subMsg ->
            "GotShopViewerMsg" :: ShopViewer.msgToString subMsg

        GotTransferScreenMsg subMsg ->
            "GotTransferScreenMsg" :: Transfer.msgToString subMsg

        GotInviteMsg subMsg ->
            "GotInviteMsg" :: Invite.msgToString subMsg



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewGuest subModel page toMsg content =
            case model.session of
                Page.Guest guest ->
                    Html.map toMsg (content guest subModel)
                        |> Page.viewGuest GotPageMsg page guest

                Page.LoggedIn _ ->
                    text ""

        viewLoggedIn subModel page toMsg content =
            case model.session of
                Page.Guest _ ->
                    text ""

                Page.LoggedIn loggedIn ->
                    Html.map toMsg (content loggedIn subModel)
                        |> Page.viewLoggedIn GotPageMsg page loggedIn

        viewPage guestPage loggedInPage toMsg content =
            case model.session of
                Page.Guest guest ->
                    Html.map toMsg content
                        |> Page.viewGuest GotPageMsg guestPage guest

                Page.LoggedIn loggedIn ->
                    Html.map toMsg content
                        |> Page.viewLoggedIn GotPageMsg loggedInPage loggedIn
    in
    case model.status of
        Redirect ->
            viewPage Guest.Other LoggedIn.Other (\_ -> Ignored) (text "")

        NotFound ->
            viewPage Guest.Other LoggedIn.Other (\_ -> Ignored) (NotFound.view model.session)

        ComingSoon ->
            viewPage Guest.Other LoggedIn.Other (\_ -> Ignored) (ComingSoon.view model.session)

        Register _ subModel ->
            viewGuest subModel Guest.Other GotRegisterMsg Register.view

        Login subModel ->
            viewGuest subModel Guest.Other GotLoginMsg Login.view

        Notification subModel ->
            viewLoggedIn subModel LoggedIn.Other GotNotificationMsg Notification.view

        Community subModel ->
            viewLoggedIn subModel LoggedIn.Other GotCommunityMsg Community.view

        CommunityEditor subModel ->
            viewLoggedIn subModel LoggedIn.Other GotCommunityEditorMsg CommunityEditor.view

        Objectives subModel ->
            viewLoggedIn subModel LoggedIn.Other GotObjectivesMsg Objectives.view

        ObjectiveEditor subModel ->
            viewLoggedIn subModel LoggedIn.Other GotObjectiveEditorMsg ObjectiveEditor.view

        ActionEditor subModel ->
            viewLoggedIn subModel LoggedIn.Other GotActionEditorMsg ActionEditor.view

        VerifyClaim subModel ->
            viewLoggedIn subModel LoggedIn.Other GotVerifyClaimMsg VerifyClaim.view

        CommunityExplore subModel ->
            viewLoggedIn subModel LoggedIn.Communities GotCommunityExploreMsg CommunityExplore.view

        Dashboard subModel ->
            viewLoggedIn subModel LoggedIn.Dashboard GotDashboardMsg Dashboard.view

        Profile subModel ->
            viewLoggedIn subModel LoggedIn.Profile GotProfileMsg Profile.view

        Shop _ subModel ->
            viewLoggedIn subModel LoggedIn.Shop GotShopMsg Shop.view

        ShopEditor _ subModel ->
            viewLoggedIn subModel LoggedIn.Other GotShopEditorMsg ShopEditor.view

        ShopViewer _ subModel ->
            viewLoggedIn subModel LoggedIn.Shop GotShopViewerMsg ShopViewer.view

        Transfer _ subModel ->
            viewLoggedIn subModel LoggedIn.Other GotTransferScreenMsg Transfer.view

        Invite subModel ->
            Html.map (\_ -> Ignored) (Invite.view model.session subModel)
