module Page.Community.Invite exposing
    ( Model
    , Msg
    , init
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api.Graphql
import Community exposing (Invite)
import Dict
import Eos exposing (symbolToString)
import Eos.Account exposing (nameToString)
import Graphql.Http
import Html exposing (Html, button, div, img, p, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Log
import Page exposing (Session(..), toShared)
import Profile exposing (Model)
import Profile.EditKycForm as KycForm
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Translators)
import UpdateResult as UR
import Utils
import View.Feedback as Feedback
import View.Modal as Modal



-- TYPES


type alias InvitationId =
    String


type PageStatus
    = Loading
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Invite))
    | JoinConfirmation Invite
    | AlreadyMemberNotice Invite
    | KycInfo Invite


type ModalStatus
    = Closed
    | Open



-- MODEL


type alias Model =
    { pageStatus : PageStatus
    , confirmationModalStatus : ModalStatus
    , invitationId : InvitationId
    , kycForm : Maybe KycForm.Model
    }



-- INIT


initModel : InvitationId -> Model
initModel invitationId =
    { pageStatus = Loading
    , confirmationModalStatus = Closed
    , invitationId = invitationId
    , kycForm = Nothing
    }


init : Session -> InvitationId -> ( Model, Cmd Msg )
init session invitationId =
    ( initModel invitationId
    , case session of
        Guest _ ->
            Api.Graphql.query
                (toShared session)
                Nothing
                (Community.inviteQuery invitationId)
                CompletedLoadInvite

        LoggedIn loggedIn ->
            LoggedIn.maybeInitWith (\_ -> CompletedLoadProfile) .profile loggedIn
    )



-- VIEW


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    let
        shared =
            toShared session

        { t } =
            shared.translators

        title =
            case model.pageStatus of
                JoinConfirmation invite ->
                    let
                        inviter =
                            invite.creator.name
                                |> Maybe.withDefault (nameToString invite.creator.account)
                    in
                    inviter
                        ++ " "
                        ++ t "community.invitation.title"
                        ++ " "
                        ++ invite.community.name

                _ ->
                    ""

        content =
            case model.pageStatus of
                Loading ->
                    text ""

                NotFound ->
                    text ""

                Failed e ->
                    Page.fullPageGraphQLError (t "") e

                AlreadyMemberNotice invite ->
                    let
                        inner =
                            viewExistingMemberNotice shared.translators invite.community.name
                    in
                    div []
                        [ viewHeader
                        , viewContent shared.translators invite inner
                        ]

                JoinConfirmation invite ->
                    let
                        inner =
                            viewNewMemberConfirmation shared.translators model.invitationId invite
                    in
                    div []
                        [ viewHeader
                        , viewContent shared.translators invite inner
                        , viewModal shared.translators model.confirmationModalStatus model.invitationId invite
                        ]

                KycInfo invite ->
                    let
                        formData =
                            Maybe.withDefault
                                KycForm.init
                                model.kycForm

                        inner =
                            div [ class "md:max-w-sm md:mx-auto my-6" ]
                                [ p []
                                    [ text (t "community.invite.kycRequired") ]
                                , p [ class "mt-2 mb-6" ]
                                    [ text (t "community.kyc.add.canRemove") ]
                                , KycForm.view shared.translators formData
                                    |> Html.map FormMsg
                                ]
                    in
                    div []
                        [ viewHeader
                        , viewContent shared.translators invite inner
                        ]
    in
    { title = title
    , content = content
    }


viewHeader : Html msg
viewHeader =
    div [ class "w-full h-16 flex px-4 items-center bg-indigo-500" ]
        []


viewExistingMemberNotice : Translators -> String -> Html Msg
viewExistingMemberNotice { t, tr } communityTitle =
    div [ class "mt-6 text-center" ]
        [ p [] [ text (t "community.invitation.already_member") ]
        , p [ class "max-w-lg md:mx-auto mt-3" ]
            [ text <|
                tr "community.invitation.choose_community_tip"
                    [ ( "communityTitle", communityTitle ) ]
            ]
        ]


viewNewMemberConfirmation : Translators -> InvitationId -> Invite -> Html Msg
viewNewMemberConfirmation { t } invitationId ({ community } as invite) =
    div []
        [ div [ class "mt-6 px-4 text-center" ]
            [ span [ class "mr-1" ] [ text (t "community.invitation.subtitle") ]
            , text community.name
            , text "?"
            ]
        , div [ class "flex flex-wrap justify-center w-full mt-6" ]
            [ button
                [ class "button button-secondary w-full md:w-48 uppercase mb-4 md:mr-8"
                , onClick OpenConfirmationModal
                ]
                [ text (t "community.invitation.no") ]
            , button
                [ class "button button-primary w-full md:w-48 uppercase"
                , onClick (InvitationAccepted invitationId invite)
                ]
                [ text (t "community.invitation.yes") ]
            ]
        ]


viewContent : Translators -> Invite -> Html Msg -> Html Msg
viewContent { t } { creator, community } innerContent =
    let
        inviter =
            creator.name
                |> Maybe.withDefault (nameToString creator.account)
    in
    div [ class "bg-white pb-20" ]
        [ div [ class "flex flex-wrap content-end" ]
            [ div [ class "flex overflow-hidden items-center justify-center h-24 w-24 rounded-full mx-auto -mt-12 bg-white" ]
                [ img
                    [ src community.logo
                    , class "object-scale-down h-20 w-20"
                    ]
                    []
                ]
            ]
        , div [ class "px-4" ]
            [ div [ class "mt-6" ]
                [ div [ class "text-lg text-center" ]
                    [ span [ class "font-semibold" ]
                        [ text inviter
                        ]
                    , text " "
                    , text (t "community.invitation.title")
                    , text " "
                    , span [ class "font-semibold" ]
                        [ text community.name ]
                    ]
                ]
            , innerContent
            ]
        ]


viewModal : Translators -> ModalStatus -> InvitationId -> Invite -> Html Msg
viewModal { t } modalStatus invitationId invite =
    let
        isModalVisible =
            case modalStatus of
                Closed ->
                    False

                Open ->
                    True
    in
    Modal.initWith
        { closeMsg = CloseConfirmationModal
        , isVisible = isModalVisible
        }
        |> Modal.withHeader (t "community.invitation.modal.title")
        |> Modal.withBody
            [ text (t "community.invitation.modal.body") ]
        |> Modal.withFooter
            [ button
                [ class "modal-cancel"
                , onClick InvitationRejected
                ]
                [ text (t "community.invitation.modal.no")
                ]
            , button
                [ class "modal-accept"
                , onClick (InvitationAccepted invitationId invite)
                ]
                [ text (t "community.invitation.modal.yes") ]
            ]
        |> Modal.toHtml



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoadInvite (RemoteData (Graphql.Http.Error (Maybe Invite)) (Maybe Invite))
    | CompletedLoadProfile
    | OpenConfirmationModal
    | CloseConfirmationModal
    | InvitationRejected
    | InvitationAccepted InvitationId Invite
    | SignedIn
    | FormMsg KycForm.Msg


update : Session -> Msg -> Model -> UpdateResult
update session msg model =
    case msg of
        FormMsg kycFormMsg ->
            case session of
                LoggedIn ({ shared } as loggedIn) ->
                    let
                        formUpdateResult =
                            case model.kycForm of
                                Just f ->
                                    KycForm.update
                                        shared
                                        f
                                        kycFormMsg

                                Nothing ->
                                    KycForm.update
                                        shared
                                        KycForm.init
                                        kycFormMsg

                        newModel =
                            formUpdateResult
                                |> UR.fromChild (\newForm -> { model | kycForm = Just newForm })
                                    FormMsg
                                    LoggedIn.addFeedback
                                    model
                    in
                    case kycFormMsg of
                        KycForm.Submitted formOutput ->
                            newModel
                                |> UR.addExt
                                    (KycForm.saveKycData loggedIn formOutput
                                        |> LoggedIn.mapExternal FormMsg
                                    )

                        KycForm.Saved result ->
                            case result of
                                RemoteData.Failure _ ->
                                    newModel
                                        |> UR.addExt
                                            (ShowFeedback Feedback.Failure
                                                (shared.translators.t "error.unknown")
                                            )

                                _ ->
                                    newModel
                                        |> UR.addExt (LoggedIn.RequiredAuthToken { callbackCmd = \_ -> Utils.spawnMessage SignedIn })

                        _ ->
                            newModel

                Guest _ ->
                    model
                        |> UR.init

        CompletedLoadInvite (RemoteData.Success (Just invite)) ->
            let
                userCommunities =
                    case session of
                        LoggedIn { profile } ->
                            case profile of
                                RemoteData.Success p ->
                                    p.communities

                                _ ->
                                    []

                        _ ->
                            []

                isAlreadyMember =
                    let
                        isMember c =
                            symbolToString c.symbol == symbolToString invite.community.symbol
                    in
                    List.any isMember userCommunities

                newPageStatus =
                    if isAlreadyMember then
                        AlreadyMemberNotice

                    else
                        JoinConfirmation
            in
            UR.init { model | pageStatus = newPageStatus invite }
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = msg
                    , message = "Completed loading invite"
                    , data = Dict.empty
                    , level = Log.DebugLevel
                    }

        CompletedLoadInvite (RemoteData.Success Nothing) ->
            UR.init { model | pageStatus = NotFound }

        CompletedLoadInvite (RemoteData.Failure error) ->
            { model | pageStatus = Failed error }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Page.maybeAccountName session)
                    "Got an error when trying to load invite"
                    { moduleName = "Page.Community.Invite", function = "update" }
                    []
                    error

        CompletedLoadInvite _ ->
            UR.init model

        CompletedLoadProfile ->
            UR.init model
                |> UR.addCmd
                    (Api.Graphql.query (toShared session)
                        Nothing
                        (Community.inviteQuery model.invitationId)
                        CompletedLoadInvite
                    )

        OpenConfirmationModal ->
            { model | confirmationModalStatus = Open }
                |> UR.init

        CloseConfirmationModal ->
            { model | confirmationModalStatus = Closed }
                |> UR.init

        InvitationRejected ->
            let
                addBreadcrumb =
                    UR.addBreadcrumb
                        { type_ = Log.DebugBreadcrumb
                        , category = msg
                        , message = "Rejected invitation"
                        , data = Dict.empty
                        , level = Log.DebugLevel
                        }
            in
            case session of
                LoggedIn loggedIn ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Dashboard |> Route.replaceUrl loggedIn.shared.navKey)
                        |> addBreadcrumb

                Guest guest ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Register Nothing Nothing
                                |> Route.replaceUrl guest.shared.navKey
                            )
                        |> addBreadcrumb

        InvitationAccepted invitationId invite ->
            let
                userKyc =
                    case session of
                        LoggedIn { profile } ->
                            case profile of
                                RemoteData.Success p ->
                                    p.kyc

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                areUserKycFieldsFilled =
                    case userKyc of
                        Just _ ->
                            True

                        Nothing ->
                            False

                allowedToJoinCommunity =
                    (invite.community.hasKyc && areUserKycFieldsFilled)
                        || not invite.community.hasKyc

                addBreadcrumb =
                    UR.addBreadcrumb
                        { type_ = Log.DebugBreadcrumb
                        , category = msg
                        , message = "Accepted invitation"
                        , data = Dict.empty
                        , level = Log.DebugLevel
                        }
            in
            case session of
                LoggedIn _ ->
                    if allowedToJoinCommunity then
                        model
                            |> UR.init
                            |> UR.addExt (LoggedIn.RequiredAuthToken { callbackCmd = \_ -> Utils.spawnMessage SignedIn })
                            |> addBreadcrumb

                    else
                        { model | pageStatus = KycInfo invite }
                            |> UR.init
                            |> addBreadcrumb

                Guest guest ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Register (Just invitationId) Nothing
                                |> Route.replaceUrl guest.shared.navKey
                            )
                        |> addBreadcrumb

        SignedIn ->
            case getInvite model of
                Nothing ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed signin in, but invite was unavailable"
                            (Page.maybeAccountName session)
                            { moduleName = "Page.Community.Invite", function = "update" }
                            []

                Just { community } ->
                    let
                        communityInfo =
                            { symbol = community.symbol
                            , name = community.name
                            , logo = community.logo
                            , subdomain = community.subdomain
                            , hasShop = community.hasShop
                            , hasActions = community.hasObjectives
                            , hasKyc = community.hasKyc
                            }

                        navKey =
                            toShared session
                                |> .navKey

                        redirectRoute =
                            case session of
                                LoggedIn _ ->
                                    Route.Dashboard

                                Guest guest ->
                                    guest.afterLoginRedirect
                                        |> Maybe.withDefault Route.Dashboard
                    in
                    model
                        |> UR.init
                        |> UR.addExt (LoggedIn.AddedCommunity communityInfo)
                        |> UR.addCmd (Route.pushUrl navKey redirectRoute)
                        |> UR.addBreadcrumb
                            { type_ = Log.InfoBreadcrumb
                            , category = msg
                            , message = "Signed in"
                            , data = Dict.empty
                            , level = Log.Info
                            }


getInvite : Model -> Maybe Invite
getInvite model =
    case model.pageStatus of
        JoinConfirmation invite ->
            Just invite

        AlreadyMemberNotice invite ->
            Just invite

        KycInfo invite ->
            Just invite

        _ ->
            Nothing



-- INTEROP


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.ProfileLoaded _ ->
            Just CompletedLoadProfile

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadInvite r ->
            [ "CompletedLoadInvite", UR.remoteDataToString r ]

        CompletedLoadProfile ->
            [ "CompletedLoadProfile" ]

        OpenConfirmationModal ->
            [ "OpenConfirmationModal" ]

        CloseConfirmationModal ->
            [ "CloseConfirmationModal" ]

        InvitationRejected ->
            [ "InvitationRejected" ]

        InvitationAccepted _ _ ->
            [ "InvitationAccepted" ]

        FormMsg _ ->
            [ "FormMsg" ]

        SignedIn ->
            [ "SignedIn" ]
