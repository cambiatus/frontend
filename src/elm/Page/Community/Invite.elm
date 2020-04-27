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
import Community exposing (Invite, inviteQuery)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, button, div, img, p, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (t)
import Icons
import Page exposing (Session(..), toShared)
import Profile exposing (Profile)
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Shared)
import UpdateResult as UR


init : Session -> String -> ( Model, Cmd Msg )
init session invitationId =
    ( initModel invitationId
    , Api.Graphql.query
        (toShared session)
        (Community.inviteQuery invitationId)
        CompletedLoad
    )


initModel : String -> Model
initModel invitationId =
    { status = Loading
    , confirmationModal = Closed
    , invitationId = invitationId
    }


type alias Model =
    { status : Status
    , confirmationModal : ModalStatus
    , invitationId : String
    }


type Status
    = Loading
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Invite))
    | Loaded Invite
    | Error Http.Error


type ModalStatus
    = Closed
    | Open


view : Session -> Model -> Html Msg
view session model =
    let
        shared =
            toShared session
    in
    div [ class "flex flex-col min-h-screen" ]
        [ div [ class "flex-grow" ]
            [ case model.status of
                Loading ->
                    div [] []

                NotFound ->
                    div [] []

                Failed e ->
                    Page.fullPageGraphQLError (t shared.translations "") e

                Loaded invite ->
                    div []
                        [ viewHeader
                        , viewContent shared invite model.invitationId
                        , viewModal shared model model.invitationId
                        ]

                Error e ->
                    Page.fullPageError (t shared.translations "") e
            ]
        , viewFooter session
        ]


viewHeader : Html msg
viewHeader =
    div [ class "w-full h-16 flex px-4 items-center bg-indigo-500" ]
        []


viewContent : Shared -> Invite -> String -> Html Msg
viewContent shared invite invitationId =
    let
        text_ s =
            text (I18Next.t shared.translations s)

        inviter =
            invite.creator.userName
                |> Maybe.withDefault (Eos.nameToString invite.creator.account)
    in
    div [ class "bg-white pb-20" ]
        [ div [ class "flex flex-wrap content-end" ]
            [ div [ class "flex items-center justify-center h-24 w-24 rounded-full mx-auto -mt-12 bg-white" ]
                [ img
                    [ src (shared.endpoints.ipfs ++ "/" ++ invite.community.logo)
                    , class "object-scale-down h-20 w-20"
                    ]
                    []
                ]
            ]
        , div [ class " px-4" ]
            [ div [ class "flex mx-auto justify-center justify-center mt-6" ]
                [ div [ class "inline-block text-center text-heading font" ]
                    [ span [ class "mr-1 font-medium" ] [ text inviter ]
                    , text_ "community.invitation.title"
                    , span [ class "ml-1 font-medium" ] [ text invite.community.title ]
                    ]
                ]
            , div [ class "flex mx-auto justify-center mt-6 px-4" ]
                [ div [ class "inline-block text-center text-heading" ]
                    [ span [ class "mr-1" ] [ text_ "community.invitation.subtitle" ]
                    , text invite.community.title
                    , text "?"
                    ]
                ]
            , div [ class "flex flex-wrap justify-center w-full mt-6" ]
                [ button
                    [ class "button button-sm button-secondary w-full md:w-48 uppercase mb-4 md:mr-8"
                    , onClick OpenConfirmationModal
                    ]
                    [ text_ "community.invitation.no" ]
                , button
                    [ class "button button-sm button-primary w-full md:w-48 uppercase"
                    , onClick (AcceptInvitation invitationId)
                    ]
                    [ text_ "community.invitation.yes" ]
                ]
            ]
        ]


viewModal : Shared -> Model -> String -> Html Msg
viewModal shared model invitationId =
    let
        t s =
            I18Next.t shared.translations s

        text_ s =
            text (t s)
    in
    case model.confirmationModal of
        Closed ->
            text ""

        _ ->
            div [ class "modal container" ]
                [ div [ class "modal-bg", onClick CloseConfirmationModal ] []
                , div [ class "modal-content" ]
                    [ div [ class "w-full" ]
                        [ p [ class "text-2xl font-bold mb-4" ]
                            [ text_ "community.invitation.modal.title" ]
                        , button [ onClick CloseConfirmationModal ]
                            [ Icons.close "absolute fill-current text-gray-400 top-0 right-0 mx-8 my-4" ]
                        , div [ class "flex flex-wrap items-center" ]
                            [ div [ class "flex flex-col items-left mb-4" ]
                                [ p [ class "" ]
                                    [ text_ "community.invitation.modal.body" ]
                                ]
                            , div [ class "modal-footer" ]
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
                            ]
                        ]
                    ]
                ]


viewFooter : Session -> Html msg
viewFooter session =
    let
        shared =
            case session of
                LoggedIn loggedIn ->
                    loggedIn.shared

                Guest guest ->
                    guest.shared
    in
    LoggedIn.viewFooter shared



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
            UR.init { model | status = Loaded invitation }

        CompletedLoad (Ok Nothing) ->
            UR.init { model | status = NotFound }

        CompletedLoad (Err error) ->
            { model | status = Failed error }
                |> UR.init
                |> UR.logGraphqlError msg error

        OpenConfirmationModal ->
            { model | confirmationModal = Open }
                |> UR.init

        CloseConfirmationModal ->
            { model | confirmationModal = Closed }
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
            { model | status = Error err }
                |> UR.init


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
