module Page.Community.Invite exposing
    ( Model
    , Msg
    , init
    , msgToString
    , update
    , view
    )

import Api
import Api.Graphql
import Community exposing (Invite)
import Eos exposing (symbolToString)
import Eos.Account exposing (nameToString)
import Graphql.Http
import Html exposing (Html, button, div, img, p, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Page exposing (Session(..), toShared)
import Profile exposing (Profile)
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Translators)
import UpdateResult as UR
import View.Modal as Modal



-- TYPES


type alias InvitationId =
    String


type PageStatus
    = Loading
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Invite))
    | Loaded Invite
    | Error Http.Error


type ModalStatus
    = Closed
    | Open



-- MODEL


type alias Model =
    { pageStatus : PageStatus
    , confirmationModalStatus : ModalStatus
    , invitationId : InvitationId
    }



-- INIT


initModel : String -> Model
initModel invitationId =
    { pageStatus = Loading
    , confirmationModalStatus = Closed
    , invitationId = invitationId
    }


init : Session -> String -> ( Model, Cmd Msg )
init session invitationId =
    ( initModel invitationId
    , Api.Graphql.query
        (toShared session)
        (Community.inviteQuery invitationId)
        CompletedLoad
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
                Loaded invite ->
                    let
                        inviter =
                            invite.creator.userName
                                |> Maybe.withDefault (nameToString invite.creator.account)
                    in
                    inviter
                        ++ " "
                        ++ t "community.invitation.title"
                        ++ " "
                        ++ invite.community.title

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

                Loaded invite ->
                    let
                        userCommunities =
                            case session of
                                LoggedIn { profile } ->
                                    case profile of
                                        LoggedIn.Loaded p ->
                                            p.communities

                                        _ ->
                                            []

                                Guest _ ->
                                    []

                        isMemberAlready : Bool
                        isMemberAlready =
                            -- True if current user is already a member of the inviter's community
                            let
                                isMember c =
                                    symbolToString c.id == symbolToString invite.community.symbol
                            in
                            List.any isMember userCommunities
                    in
                    div []
                        [ viewHeader
                        , viewContent shared.translators invite model.invitationId isMemberAlready
                        , viewModal shared.translators model.confirmationModalStatus model.invitationId
                        ]

                Error e ->
                    Page.fullPageError (t "") e
    in
    { title = title
    , content = content
    }


viewHeader : Html msg
viewHeader =
    div [ class "w-full h-16 flex px-4 items-center bg-indigo-500" ]
        []


viewContent : Translators -> Invite -> InvitationId -> Bool -> Html Msg
viewContent { t, tr } { creator, community } invitationId isMemberAlready =
    let
        text_ s =
            text (t s)

        inviter =
            creator.userName
                |> Maybe.withDefault (nameToString creator.account)

        viewNewMemberConfirmation =
            div []
                [ div [ class "mt-6 px-4" ]
                    [ span [ class "mr-1" ] [ text_ "community.invitation.subtitle" ]
                    , text community.title
                    , text "?"
                    ]
                , div [ class "flex flex-wrap justify-center w-full mt-6" ]
                    [ button
                        [ class "button button-secondary w-full md:w-48 uppercase mb-4 md:mr-8"
                        , onClick OpenConfirmationModal
                        ]
                        [ text_ "community.invitation.no" ]
                    , button
                        [ class "button button-primary w-full md:w-48 uppercase"
                        , onClick (AcceptInvitation invitationId)
                        ]
                        [ text_ "community.invitation.yes" ]
                    ]
                ]

        viewExistingMemberNotice =
            div [ class "mt-6" ]
                [ p [] [ text (t "community.invitation.already_member") ]
                , p [ class "max-w-lg md:mx-auto mt-3" ]
                    [ text <|
                        tr "community.invitation.choose_community_tip"
                            [ ( "communityTitle", community.title ) ]
                    ]
                ]
    in
    div [ class "bg-white pb-20" ]
        [ div [ class "flex flex-wrap content-end" ]
            [ div [ class "flex items-center justify-center h-24 w-24 rounded-full mx-auto -mt-12 bg-white" ]
                [ img
                    [ src community.logo
                    , class "object-scale-down h-20 w-20"
                    ]
                    []
                ]
            ]
        , div [ class "px-4 text-center text-heading" ]
            [ div [ class "mt-6" ]
                [ div [ class "font" ]
                    [ span [ class "font-medium" ]
                        [ text inviter
                        ]
                    , text " "
                    , text_ "community.invitation.title"
                    , text " "
                    , span [ class "font-medium" ]
                        [ text community.title ]
                    ]
                ]
            , if isMemberAlready then
                viewExistingMemberNotice

              else
                viewNewMemberConfirmation
            ]
        ]


viewModal : Translators -> ModalStatus -> InvitationId -> Html Msg
viewModal { t } modalStatus invitationId =
    let
        text_ s =
            text (t s)

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
            [ text_ "community.invitation.modal.body" ]
        |> Modal.withFooter
            [ button
                [ class "modal-cancel"
                , onClick RejectInvitation
                ]
                [ text_ "community.invitation.modal.no"
                ]
            , button
                [ class "modal-accept"
                , onClick (AcceptInvitation invitationId)
                ]
                [ text_ "community.invitation.modal.yes" ]
            ]
        |> Modal.toHtml



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Invite)) (Maybe Invite))
    | OpenConfirmationModal
    | CloseConfirmationModal
    | RejectInvitation
    | AcceptInvitation String
    | CompletedSignIn LoggedIn.Model (Result Http.Error Profile)


update : Session -> Msg -> Model -> UpdateResult
update session msg model =
    case msg of
        CompletedLoad (Ok (Just invitation)) ->
            UR.init { model | pageStatus = Loaded invitation }

        CompletedLoad (Ok Nothing) ->
            UR.init { model | pageStatus = NotFound }

        CompletedLoad (Err error) ->
            { model | pageStatus = Failed error }
                |> UR.init
                |> UR.logGraphqlError msg error

        OpenConfirmationModal ->
            { model | confirmationModalStatus = Open }
                |> UR.init

        CloseConfirmationModal ->
            { model | confirmationModalStatus = Closed }
                |> UR.init

        RejectInvitation ->
            case session of
                LoggedIn loggedIn ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Dashboard |> Route.replaceUrl loggedIn.shared.navKey)

                Guest guest ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Register Nothing Nothing
                                |> Route.replaceUrl guest.shared.navKey
                            )

        AcceptInvitation invitationId ->
            case session of
                LoggedIn loggedIn ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Api.signInInvitation loggedIn.shared
                                loggedIn.accountName
                                invitationId
                                (CompletedSignIn loggedIn)
                            )

                Guest guest ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Register (Just invitationId) Nothing
                                |> Route.replaceUrl guest.shared.navKey
                            )

        CompletedSignIn { shared } (Ok _) ->
            model
                |> UR.init
                |> UR.addCmd
                    (Route.Dashboard
                        |> Route.replaceUrl shared.navKey
                    )

        CompletedSignIn _ (Err err) ->
            { model | pageStatus = Error err }
                |> UR.init



-- INTEROP


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]

        OpenConfirmationModal ->
            [ "OpenConfirmationModal" ]

        CloseConfirmationModal ->
            [ "CloseConfirmationModal" ]

        RejectInvitation ->
            [ "RejectInvitation" ]

        AcceptInvitation _ ->
            [ "AcceptInvitation" ]

        CompletedSignIn _ _ ->
            [ "CompletedSignIn" ]
