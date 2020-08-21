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
import Html exposing (Html, button, div, form, img, input, label, option, p, select, span, text)
import Html.Attributes exposing (attribute, class, placeholder, selected, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
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
    | JoinConfirmation Invite
    | AlreadyMemberNotice Invite
    | KycForm Invite
    | Error Http.Error


type ModalStatus
    = Closed
    | Open


type KycDocumentType
    = CedulaDeIdentidad
    | DIMEX
    | NITE



-- MODEL


type alias Model =
    { pageStatus : PageStatus
    , confirmationModalStatus : ModalStatus
    , invitationId : InvitationId
    , kycForm : Maybe KycFormModel
    }


type alias KycFormModel =
    { documentType : KycDocumentType
    , documentNumber : String
    , phoneNumber : String
    }



-- INIT


initModel : String -> Model
initModel invitationId =
    { pageStatus = Loading
    , confirmationModalStatus = Closed
    , invitationId = invitationId
    , kycForm = Nothing
    }


init : Session -> InvitationId -> ( Model, Cmd Msg )
init session invitationId =
    ( initModel invitationId
    , Api.Graphql.query
        (toShared session)
        (Community.inviteQuery invitationId)
        CompletedLoad
    )


initKycForm =
    { documentType = CedulaDeIdentidad
    , documentNumber = ""
    , phoneNumber = ""
    }



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

                AlreadyMemberNotice invite ->
                    let
                        inner =
                            viewExistingMemberNotice shared.translators invite.community.title
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

                KycForm invite ->
                    let
                        inner =
                            case model.kycForm of
                                Just f ->
                                    viewKycForm shared.translators f

                                Nothing ->
                                    viewKycForm shared.translators initKycForm
                    in
                    div []
                        [ viewHeader
                        , viewContent shared.translators invite inner
                        ]

                Error e ->
                    Page.fullPageError (t "") e
    in
    { title = title
    , content = content
    }


viewKycForm : Translators -> KycFormModel -> Html Msg
viewKycForm { t } { documentType, documentNumber } =
    div [ class "md:max-w-sm md:mx-auto mt-6" ]
        [ p []
            [ text "This community requires it's members to have some more information. Please, fill these fields below." ]
        , p [ class "mt-2 mb-6" ]
            [ text "You can always remove this information from your profile if you decide to do so." ]
        , form
            [ onSubmit KycFormSubmitted
            ]
            [ div [ class "form-field mb-6" ]
                [ label [ class "input-label block" ]
                    [ text "document type"
                    ]
                , select
                    [ onInput (FormMsg << DocumentTypeChanged)
                    , selected (documentType == CedulaDeIdentidad)
                    , class "form-select"
                    ]
                    [ option
                        [ value "Cedula" ]
                        [ text "Cedula de identidad" ]
                    , option
                        [ value "DIMEX"
                        , selected (documentType == DIMEX)
                        ]
                        [ text "DIMEX" ]
                    , option
                        [ value "NITE"
                        , selected (documentType == NITE)
                        ]
                        [ text "NITE" ]
                    ]
                ]
            , div [ class "form-field mb-6" ]
                [ label [ class "input-label block" ]
                    [ text <|
                        case documentType of
                            CedulaDeIdentidad ->
                                "Cedula de identidad number"

                            DIMEX ->
                                "Dimex number"

                            NITE ->
                                "Nite number"
                    ]
                , input
                    [ type_ "text"
                    , class "form-input"
                    , attribute "inputmode" "numeric"
                    , onInput (FormMsg << DocumentNumberEntered)
                    , value documentNumber
                    , placeholder <|
                        case documentType of
                            CedulaDeIdentidad ->
                                "X-XXXX-XXXX"

                            DIMEX ->
                                "XXXXXXXXXXX"

                            NITE ->
                                "XXXXXXXXXX"
                    ]
                    []
                ]
            , div [ class "form-field mb-10" ]
                [ label [ class "input-label block" ]
                    [ text "phone number" ]
                , input
                    [ type_ "tel"
                    , class "form-input"
                    , onInput (FormMsg << PhoneNumberEntered)
                    , placeholder "XXXX-XXXX"
                    ]
                    []
                ]
            , div []
                [ button
                    [ class "button w-full button-primary" ]
                    [ text "Save and Join" ]
                ]
            ]
        ]


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
            , text community.title
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
            creator.userName
                |> Maybe.withDefault (nameToString creator.account)
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
        , div [ class "px-4" ]
            [ div [ class "mt-6" ]
                [ div [ class "text-heading text-center" ]
                    [ span [ class "font-medium" ]
                        [ text inviter
                        ]
                    , text " "
                    , text (t "community.invitation.title")
                    , text " "
                    , span [ class "font-medium" ]
                        [ text community.title ]
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
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Invite)) (Maybe Invite))
    | OpenConfirmationModal
    | CloseConfirmationModal
    | InvitationRejected
    | InvitationAccepted InvitationId Invite
    | CompletedSignIn LoggedIn.Model (Result Http.Error Profile)
    | KycFormSubmitted
    | FormMsg KycFormMsg


type KycFormMsg
    = DocumentTypeChanged String
    | DocumentNumberEntered String
    | PhoneNumberEntered String


updateKycForm kycModel kycMsg =
    case kycMsg of
        DocumentTypeChanged val ->
            let
                docType =
                    case val of
                        "Cedula" ->
                            CedulaDeIdentidad

                        "DIMEX" ->
                            DIMEX

                        _ ->
                            NITE
            in
            { kycModel
                | documentType = docType
                , documentNumber = ""
            }

        DocumentNumberEntered n ->
            let
                trim : Int -> String -> String -> String
                trim desiredLength oldNum newNum =
                    let
                        corrected =
                            if String.all Char.isDigit newNum then
                                newNum

                            else
                                oldNum
                    in
                    if String.length corrected > desiredLength then
                        String.slice 0 desiredLength corrected

                    else
                        corrected

                trimmedNumber =
                    case kycModel.documentType of
                        CedulaDeIdentidad ->
                            if String.startsWith "0" n then
                                kycModel.documentNumber

                            else
                                trim 9 kycModel.documentNumber n

                        DIMEX ->
                            if String.startsWith "0" n then
                                kycModel.documentNumber

                            else
                                trim 10 kycModel.documentNumber n

                        NITE ->
                            if String.startsWith "0" n then
                                kycModel.documentNumber

                            else
                                trim 10 kycModel.documentNumber n
            in
            { kycModel | documentNumber = trimmedNumber }

        _ ->
            kycModel


update : Session -> Msg -> Model -> UpdateResult
update session msg model =
    case msg of
        FormMsg kycFormMsg ->
            let
                newForm =
                    case model.kycForm of
                        Just f ->
                            updateKycForm f kycFormMsg

                        Nothing ->
                            updateKycForm initKycForm kycFormMsg
            in
            { model | kycForm = Just newForm }
                |> UR.init

        CompletedLoad (Ok (Just invite)) ->
            let
                userCommunities =
                    case session of
                        LoggedIn { profile } ->
                            case profile of
                                LoggedIn.Loaded p ->
                                    p.communities

                                _ ->
                                    []

                        _ ->
                            []

                isAlreadyMember =
                    let
                        isMember c =
                            symbolToString c.id == symbolToString invite.community.symbol
                    in
                    List.any isMember userCommunities

                newPageStatus =
                    if isAlreadyMember then
                        AlreadyMemberNotice

                    else
                        JoinConfirmation
            in
            UR.init { model | pageStatus = newPageStatus invite }

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

        InvitationRejected ->
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

        InvitationAccepted invitationId invite ->
            let
                hasCommunityKycEnabled =
                    -- TODO: Use `community.hasKyc` when it's ready
                    True

                hasInviteeKycFieldsFilled =
                    -- TODO: check KYC fields in the user profile when the KYC query will be ready
                    False

                allowedToJoinCommunity =
                    (hasCommunityKycEnabled && hasInviteeKycFieldsFilled)
                        || not hasCommunityKycEnabled
            in
            case session of
                LoggedIn loggedIn ->
                    if allowedToJoinCommunity then
                        model
                            |> UR.init
                            |> UR.addCmd
                                (Api.signInInvitation loggedIn.shared
                                    loggedIn.accountName
                                    invitationId
                                    (CompletedSignIn loggedIn)
                                )

                    else
                        { model | pageStatus = KycForm invite }
                            |> UR.init

                Guest guest ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Register (Just invitationId) Nothing
                                |> Route.replaceUrl guest.shared.navKey
                            )

        KycFormSubmitted ->
            -- TODO: validate form, save user's Kyc data and signInInvitation
            model |> UR.init

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

        InvitationRejected ->
            [ "RejectInvitation" ]

        InvitationAccepted _ _ ->
            [ "AcceptInvitation" ]

        KycFormSubmitted ->
            [ "KycFormSubmitted" ]

        FormMsg m ->
            [ "FormMsg" ]

        CompletedSignIn _ _ ->
            [ "CompletedSignIn" ]
