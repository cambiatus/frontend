module Page.Settings exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Auth
import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.User
import Eos.Account
import Form.Text
import Form.Toggle
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet
import Html exposing (Html, a, button, div, h2, li, p, span, text, ul)
import Html.Attributes exposing (class, tabindex)
import Html.Attributes.Aria exposing (ariaHidden)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import Markdown
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Translation
import UpdateResult as UR
import View.Components
import View.Feedback
import View.Modal
import View.Pin



-- MODEL


type alias Model =
    { pinInput : View.Pin.Model
    , isExportToSimpleosModalVisible : Bool
    , isNewPinModalVisible : Bool
    , isDeleteKycModalVisible : Bool
    , claimNotificationStatus : ToggleStatus
    , transferNotificationStatus : ToggleStatus
    , digestNotificationStatus : ToggleStatus
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        ( pinModel, pinCmd ) =
            initPin loggedIn
    in
    ( { pinInput = pinModel
      , isNewPinModalVisible = False
      , isExportToSimpleosModalVisible = False
      , isDeleteKycModalVisible = False
      , claimNotificationStatus = NotUpdating
      , transferNotificationStatus = NotUpdating
      , digestNotificationStatus = NotUpdating
      }
    , pinCmd
    )


initPin : LoggedIn.Model -> ( View.Pin.Model, Cmd Msg )
initPin loggedIn =
    View.Pin.init
        { label = "profile.newPin"
        , id = "new-pin-input"
        , withConfirmation = False
        , submitLabel = "profile.pin.button"
        , submittingLabel = "profile.pin.button"
        , pinVisibility = loggedIn.shared.pinVisibility
        , lastKnownPin = loggedIn.auth.pinModel.lastKnownPin
        }
        |> Tuple.mapSecond (Cmd.map GotPinMsg)


type ToggleStatus
    = NotUpdating
    | UpdatingTo Bool


type NotificationPreference
    = Claim
    | Transfer
    | Digest



-- TYPES


type Msg
    = NoOp
    | ClickedDownloadPdf
    | ClickedExportToSimpleos
    | ClickedCopyPrivateKey
    | CopiedPrivateKey
    | ClosedExportToSimpleosModal
    | ClickedChangePin
    | ClosedNewPinModal
    | GotPinMsg View.Pin.Msg
    | SubmittedNewPin String
    | ChangedToNewPin String
    | ClickedDeleteKyc
    | ClosedDeleteKycModal
    | AcceptedDeleteKyc
    | FinishedDeletingKyc (RemoteData (Graphql.Http.Error (Maybe ())) (Maybe ()))
    | ToggledClaimNotification Bool
    | ToggledTransferNotification Bool
    | ToggledDigestNotification Bool
    | CompletedTogglingNotification NotificationPreference (RemoteData (Graphql.Http.Error (Maybe Preferences)) (Maybe Preferences))


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        ClickedDownloadPdf ->
            let
                addDownloadPdfPort =
                    case loggedIn.auth.pinModel.lastKnownPin of
                        Nothing ->
                            -- If there's no PIN, `LoggedIn.withPrivateKey` will
                            -- prompt the user for it, and this Msg will be called again
                            identity

                        Just pin ->
                            UR.addPort
                                { responseAddress = msg
                                , responseData = Json.Encode.null
                                , data =
                                    Json.Encode.object
                                        [ ( "name", Json.Encode.string "downloadAuthPdfFromProfile" )
                                        , ( "pin", Json.Encode.string pin )
                                        ]
                                }
            in
            model
                |> UR.init
                |> addDownloadPdfPort
                |> LoggedIn.withPrivateKey loggedIn
                    []
                    model
                    { successMsg = msg, errorMsg = NoOp }

        ClickedExportToSimpleos ->
            { model | isExportToSimpleosModalVisible = True }
                |> UR.init
                |> LoggedIn.withPrivateKey loggedIn
                    []
                    model
                    { successMsg = msg, errorMsg = NoOp }

        ClickedCopyPrivateKey ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = msg
                    , responseData = Json.Encode.null
                    , data =
                        Json.Encode.object
                            [ ( "name", Json.Encode.string "copyToClipboard" )
                            , ( "id", Json.Encode.string "private-key-copy-input" )
                            ]
                    }

        CopiedPrivateKey ->
            { model | isExportToSimpleosModalVisible = False }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback View.Feedback.Success
                        (loggedIn.shared.translators.t "settings.account.copied_private_key")
                    )

        ClosedExportToSimpleosModal ->
            { model | isExportToSimpleosModalVisible = False }
                |> UR.init

        ClickedChangePin ->
            let
                ( newPin, pinCmd ) =
                    initPin loggedIn
            in
            { model
                | isNewPinModalVisible = True
                , pinInput = newPin
            }
                |> UR.init
                |> UR.addCmd pinCmd

        ClosedNewPinModal ->
            { model | isNewPinModalVisible = False }
                |> UR.init

        GotPinMsg subMsg ->
            View.Pin.update loggedIn.shared subMsg model.pinInput
                |> UR.fromChild (\newPinInput -> { model | pinInput = newPinInput })
                    GotPinMsg
                    (\ext ur ->
                        case ext of
                            View.Pin.SendFeedback feedback ->
                                LoggedIn.addFeedback feedback ur

                            View.Pin.SubmitPin pin ->
                                let
                                    ( newShared, submitCmd ) =
                                        View.Pin.postSubmitAction ur.model.pinInput
                                            pin
                                            loggedIn.shared
                                            SubmittedNewPin
                                in
                                ur
                                    |> UR.mapModel (\m -> { m | isNewPinModalVisible = False })
                                    |> UR.addCmd submitCmd
                                    |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | shared = newShared })
                    )
                    model

        SubmittedNewPin newPin ->
            let
                addChangePinPort =
                    case loggedIn.auth.pinModel.lastKnownPin of
                        Nothing ->
                            identity

                        Just currentPin ->
                            UR.addPort
                                { responseAddress = msg
                                , responseData = Json.Encode.string newPin
                                , data =
                                    Json.Encode.object
                                        [ ( "name", Json.Encode.string "changePin" )
                                        , ( "currentPin", Json.Encode.string currentPin )
                                        , ( "newPin", Json.Encode.string newPin )
                                        ]
                                }
            in
            model
                |> UR.init
                |> addChangePinPort
                |> LoggedIn.withPrivateKey loggedIn
                    []
                    model
                    { successMsg = msg, errorMsg = NoOp }

        ChangedToNewPin newPin ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Success (loggedIn.shared.translators.t "profile.pin.successMsg"))
                |> UR.addExt (LoggedIn.ChangedPin newPin)

        ClickedDeleteKyc ->
            { model | isDeleteKycModalVisible = True }
                |> UR.init

        ClosedDeleteKycModal ->
            { model | isDeleteKycModalVisible = False }
                |> UR.init

        AcceptedDeleteKyc ->
            { model | isDeleteKycModalVisible = False }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.mutation loggedIn
                        (Cambiatus.Mutation.deleteKyc Graphql.SelectionSet.empty)
                        FinishedDeletingKyc
                    )
                |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | profile = RemoteData.Loading })

        FinishedDeletingKyc (RemoteData.Success _) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ReloadResource LoggedIn.ProfileResource)
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Success (loggedIn.shared.translators.t "community.kyc.delete.success"))

        FinishedDeletingKyc RemoteData.Loading ->
            UR.init model

        FinishedDeletingKyc RemoteData.NotAsked ->
            UR.init model

        FinishedDeletingKyc (RemoteData.Failure error) ->
            model
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to delete KYC"
                    { moduleName = "Page.Settings", function = "update" }
                    []
                    error

        ToggledClaimNotification newValue ->
            model
                |> UR.init
                |> actOnNotificationPreferenceToggle loggedIn Claim newValue

        ToggledTransferNotification newValue ->
            model
                |> UR.init
                |> actOnNotificationPreferenceToggle loggedIn Transfer newValue

        ToggledDigestNotification newValue ->
            model
                |> UR.init
                |> actOnNotificationPreferenceToggle loggedIn Digest newValue

        CompletedTogglingNotification preference (RemoteData.Success (Just preferences)) ->
            let
                newModel =
                    case preference of
                        Transfer ->
                            { model | transferNotificationStatus = NotUpdating }

                        Claim ->
                            { model | claimNotificationStatus = NotUpdating }

                        Digest ->
                            { model | digestNotificationStatus = NotUpdating }

                newProfile =
                    case loggedIn.profile of
                        RemoteData.Success oldProfile ->
                            RemoteData.Success
                                { oldProfile
                                    | transferNotification = preferences.transferNotification
                                    , claimNotification = preferences.claimNotification
                                    , digest = preferences.digest
                                }

                        _ ->
                            loggedIn.profile
            in
            newModel
                |> UR.init
                |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | profile = newProfile })

        CompletedTogglingNotification preference (RemoteData.Success Nothing) ->
            let
                newModel =
                    case preference of
                        Transfer ->
                            { model | transferNotificationStatus = NotUpdating }

                        Claim ->
                            { model | claimNotificationStatus = NotUpdating }

                        Digest ->
                            { model | digestNotificationStatus = NotUpdating }
            in
            newModel
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback View.Feedback.Failure
                        (loggedIn.shared.translators.t "profile.preferences.error")
                    )
                |> UR.logImpossible msg
                    "Got Nothing when toggling notification preference"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Settings", function = "update" }
                    []

        CompletedTogglingNotification preference (RemoteData.Failure err) ->
            let
                newModel =
                    case preference of
                        Transfer ->
                            { model | transferNotificationStatus = NotUpdating }

                        Claim ->
                            { model | claimNotificationStatus = NotUpdating }

                        Digest ->
                            { model | digestNotificationStatus = NotUpdating }
            in
            newModel
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback View.Feedback.Failure
                        (loggedIn.shared.translators.t "profile.preferences.error")
                    )
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when settings notification preferences"
                    { moduleName = "Page.Settings", function = "update" }
                    []
                    err

        CompletedTogglingNotification _ _ ->
            UR.init model


actOnNotificationPreferenceToggle :
    LoggedIn.Model
    -> NotificationPreference
    -> Bool
    -> (UpdateResult -> UpdateResult)
actOnNotificationPreferenceToggle loggedIn notification newValue =
    let
        updatePreferencesMutation =
            LoggedIn.mutation loggedIn
                (Cambiatus.Mutation.preference
                    (\optionalArgs ->
                        case notification of
                            Transfer ->
                                { optionalArgs | transferNotification = OptionalArgument.Present newValue }

                            Claim ->
                                { optionalArgs | claimNotification = OptionalArgument.Present newValue }

                            Digest ->
                                { optionalArgs | digest = OptionalArgument.Present newValue }
                    )
                    preferencesSelectionSet
                )
                (CompletedTogglingNotification notification)

        updateModel model =
            case notification of
                Transfer ->
                    { model | transferNotificationStatus = UpdatingTo newValue }

                Claim ->
                    { model | claimNotificationStatus = UpdatingTo newValue }

                Digest ->
                    { model | digestNotificationStatus = UpdatingTo newValue }
    in
    UR.mapModel updateModel
        >> UR.addExt updatePreferencesMutation



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            loggedIn.shared.translators.t "settings.account.title"
    in
    { title = title
    , content =
        div []
            [ Page.viewHeader loggedIn title
            , div [ class "container mx-auto px-4 mt-6 mb-20" ]
                ([ viewAccountSettings loggedIn
                 , viewNotificationPreferences loggedIn model
                 , [ viewExportToSimpleosModal loggedIn model
                   , viewNewPinModal loggedIn.shared.translators model
                   , viewDeleteKycModal loggedIn.shared.translators model
                   ]
                 ]
                    |> List.concat
                )
            ]
    }


viewExportToSimpleosModal : LoggedIn.Model -> Model -> Html Msg
viewExportToSimpleosModal loggedIn model =
    case loggedIn.auth.status of
        Auth.WithPrivateKey privateKey ->
            let
                { t } =
                    loggedIn.shared.translators
            in
            View.Modal.initWith
                { closeMsg = ClosedExportToSimpleosModal
                , isVisible = model.isExportToSimpleosModalVisible
                }
                |> View.Modal.withHeader (t "settings.account.export_private_key")
                |> View.Modal.withBody
                    [ p []
                        [ text <| t "settings.account.export_private_key_details"
                        , a
                            [ class "hover:underline text-orange-300"
                            , Html.Attributes.href "https://eosrio.io/simpleos/"
                            , Html.Attributes.target "_blank"
                            , Html.Attributes.rel "noopener"
                            ]
                            [ text "Simpleos" ]
                        ]
                    , div [ class "flex w-full items-center justify-center p-4 bg-gray-200 rounded mt-4 select-all" ] [ text (Eos.Account.privateKeyToString privateKey) ]
                    , Form.Text.view
                        (Form.Text.init
                            { label = ""
                            , id = "private-key-copy-input"
                            }
                            |> Form.Text.withExtraAttrs [ tabindex -1, ariaHidden True ]
                            |> Form.Text.withContainerAttrs
                                [ class "h-0 absolute opacity-0 left-[-9999em] !mb-0 overflow-hidden"
                                , ariaHidden True
                                ]
                            |> Form.Text.withInputElement (Form.Text.TextareaInput { submitOnEnter = False })
                        )
                        { onChange = \_ -> NoOp
                        , onBlur = NoOp
                        , value = Eos.Account.privateKeyToString privateKey
                        , error = text ""
                        , hasError = False
                        , translators = loggedIn.shared.translators
                        , isRequired = False
                        }
                    ]
                |> View.Modal.withFooter
                    [ button
                        [ class "modal-cancel"
                        , onClick ClosedExportToSimpleosModal
                        ]
                        [ text <| t "menu.close" ]
                    , button
                        [ class "modal-accept"
                        , onClick ClickedCopyPrivateKey
                        ]
                        [ text <| t "menu.copy" ]
                    ]
                |> View.Modal.toHtml

        Auth.WithoutPrivateKey ->
            text ""


viewNewPinModal : Translation.Translators -> Model -> Html Msg
viewNewPinModal translators model =
    View.Modal.initWith
        { closeMsg = ClosedNewPinModal
        , isVisible = model.isNewPinModalVisible
        }
        |> View.Modal.withHeader (translators.t "profile.changePin")
        |> View.Modal.withBody
            [ p [ class "text-sm" ]
                [ text <| translators.t "profile.changePinPrompt"
                ]
            , View.Pin.view translators model.pinInput
                |> Html.map GotPinMsg
            ]
        |> View.Modal.toHtml


viewDeleteKycModal : Translation.Translators -> Model -> Html Msg
viewDeleteKycModal { t } model =
    View.Modal.initWith
        { closeMsg = ClosedDeleteKycModal
        , isVisible = model.isDeleteKycModalVisible
        }
        |> View.Modal.withHeader (t "community.kyc.delete.confirmationHeader")
        |> View.Modal.withBody [ text <| t "community.kyc.delete.confirmationBody" ]
        |> View.Modal.withFooter
            [ button
                [ class "modal-cancel"
                , onClick ClosedDeleteKycModal
                ]
                [ text <| t "community.kyc.delete.cancel" ]
            , button
                [ class "modal-accept"
                , onClick AcceptedDeleteKyc
                ]
                [ text <| t "community.kyc.delete.confirm" ]
            ]
        |> View.Modal.toHtml


viewAccountSettings : LoggedIn.Model -> List (Html Msg)
viewAccountSettings loggedIn =
    let
        { t, tr } =
            loggedIn.shared.translators
    in
    [ h2 [ class "mb-4" ]
        [ Markdown.fromTranslation loggedIn.shared.translators "settings.account.account_settings"
            |> Markdown.view []
        ]
    , ul [ class "bg-white rounded-md p-4 divide-y" ]
        [ viewCardItem
            [ text <| t "profile.12words.title"
            , button
                [ class "button button-secondary flex-shrink-0 ml-4"
                , onClick ClickedDownloadPdf
                ]
                [ text <| t "profile.12words.button" ]
            ]
        , viewCardItem
            [ text <| t "settings.account.my_private_key"
            , button
                [ class "button button-secondary flex-shrink-0 ml-4"
                , onClick ClickedExportToSimpleos
                ]
                [ text <| t "menu.export" ]
            ]
        , viewCardItem
            [ text <| t "profile.pin.title"
            , button
                [ class "button button-secondary flex-shrink-0 ml-4"
                , onClick ClickedChangePin
                ]
                [ text <| t "profile.pin.button" ]
            ]
        , case loggedIn.profile of
            RemoteData.Success profile ->
                viewCardItem
                    [ div []
                        [ span [ class "flex items-center" ]
                            [ text <| t "community.kyc.dataTitle"
                            , View.Components.tooltip
                                { message = t "community.kyc.info"
                                , iconClass = "text-orange-300"
                                , containerClass = ""
                                }
                            ]
                        , case profile.kyc of
                            Just _ ->
                                span [ class "uppercase text-red mt-2 text-sm" ]
                                    [ text <| t "community.kyc.delete.warning" ]

                            Nothing ->
                                text ""
                        ]
                    , case profile.kyc of
                        Just _ ->
                            button
                                [ class "button button-danger flex-shrink-0 ml-4"
                                , onClick ClickedDeleteKyc
                                ]
                                [ text <| t "community.kyc.delete.label" ]

                        Nothing ->
                            a
                                [ class "button button-secondary ml-4"
                                , Route.href Route.ProfileAddKyc
                                ]
                                [ text <| t "menu.add" ]
                    ]

            RemoteData.Loading ->
                div [ class "w-full max-w-xs h-6 rounded-md mt-4 mb-4 animate-skeleton-loading" ] []

            RemoteData.NotAsked ->
                div [ class "w-full max-w-xs h-6 rounded-md mt-4 mb-4 animate-skeleton-loading" ] []

            RemoteData.Failure _ ->
                -- If the profile fails to load, the entire screen shows an error
                text ""
        ]
    ]


viewNotificationPreferences : LoggedIn.Model -> Model -> List (Html Msg)
viewNotificationPreferences loggedIn model =
    let
        { t } =
            loggedIn.shared.translators
    in
    [ h2 [ class "mb-4 mt-10" ]
        [ Markdown.fromTranslation loggedIn.shared.translators "settings.account.notification_settings"
            |> Markdown.view []
        ]
    , case loggedIn.profile of
        RemoteData.Success profile ->
            let
                updatingOrProfile modelGetter profileGetter =
                    case modelGetter model of
                        UpdatingTo newValue ->
                            newValue

                        NotUpdating ->
                            profileGetter profile
            in
            ul [ class "bg-white rounded-md p-4 divide-y" ]
                [ viewCardItem
                    [ viewNotificationToggle loggedIn.shared.translators
                        { label = t "profile.preferences.claim_notification"
                        , id = "claim_notification"
                        , onToggle = ToggledClaimNotification
                        , value = updatingOrProfile .claimNotificationStatus .claimNotification
                        }
                    ]
                , viewCardItem
                    [ viewNotificationToggle loggedIn.shared.translators
                        { label = t "profile.preferences.transfer_notification"
                        , id = "transfer_notification"
                        , onToggle = ToggledTransferNotification
                        , value = updatingOrProfile .transferNotificationStatus .transferNotification
                        }
                    ]
                , viewCardItem
                    [ viewNotificationToggle loggedIn.shared.translators
                        { label = t "profile.preferences.digest"
                        , id = "digest_notification"
                        , onToggle = ToggledDigestNotification
                        , value = updatingOrProfile .digestNotificationStatus .digest
                        }
                    ]
                ]

        RemoteData.Loading ->
            div [ class "bg-white rounded-md p-4 divide-y" ]
                [ div [ class "w-full max-w-xs h-4 mb-4 animate-skeleton-loading" ] []
                , div [ class "w-full max-w-sm h-4 my-4 animate-skeleton-loading" ] []
                , div [ class "w-full max-w-27 h-4 mt-4 animate-skeleton-loading" ] []
                ]

        RemoteData.NotAsked ->
            div [ class "bg-white rounded-md p-4 divide-y" ]
                [ div [ class "w-full max-w-xs h-4 mb-4 animate-skeleton-loading" ] []
                , div [ class "w-full max-w-sm h-4 my-4 animate-skeleton-loading" ] []
                , div [ class "w-full max-w-27 h-4 mt-4 animate-skeleton-loading" ] []
                ]

        RemoteData.Failure err ->
            -- If the profile fails to load, the entire screen shows an error
            text ""
    ]


viewNotificationToggle :
    Translation.Translators
    ->
        { label : String
        , id : String
        , onToggle : Bool -> Msg
        , value : Bool
        }
    -> Html Msg
viewNotificationToggle translators { label, id, onToggle, value } =
    Form.Toggle.init { label = text label, id = id }
        |> Form.Toggle.withContainerAttrs [ class "w-full" ]
        |> Form.Toggle.withToggleContainerAttrs [ class "w-40 flex items-center flex-shrink-0" ]
        |> (\options ->
                Form.Toggle.view options
                    { onToggle = onToggle
                    , onBlur = NoOp
                    , value = value
                    , error = text ""
                    , hasError = False
                    , isRequired = False
                    , translators = translators
                    }
           )


viewCardItem : List (Html Msg) -> Html Msg
viewCardItem body =
    li [ class "flex items-center justify-between py-4 first:pt-0 last:pb-0" ] body



-- GRAPHQL


type alias Preferences =
    { transferNotification : Bool
    , claimNotification : Bool
    , digest : Bool
    }


preferencesSelectionSet : Graphql.SelectionSet.SelectionSet Preferences Cambiatus.Object.User
preferencesSelectionSet =
    Graphql.SelectionSet.succeed Preferences
        |> Graphql.SelectionSet.with Cambiatus.Object.User.transferNotification
        |> Graphql.SelectionSet.with Cambiatus.Object.User.claimNotification
        |> Graphql.SelectionSet.with Cambiatus.Object.User.digest



-- UTILS


jsAddressToMsg : List String -> Json.Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClickedDownloadPdf" :: _ ->
            val
                |> Json.Decode.decodeValue (Json.Decode.field "isDownloaded" Json.Decode.bool)
                |> Result.map (\_ -> NoOp)
                |> Result.toMaybe

        "ClickedCopyPrivateKey" :: _ ->
            case Json.Decode.decodeValue (Json.Decode.field "copied" Json.Decode.bool) val of
                Ok True ->
                    Just CopiedPrivateKey

                Ok False ->
                    Just NoOp

                Err _ ->
                    Just NoOp

        "SubmittedNewPin" :: _ ->
            val
                |> Json.Decode.decodeValue (Json.Decode.field "addressData" Json.Decode.string)
                |> Result.map ChangedToNewPin
                |> Result.toMaybe

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ClickedDownloadPdf ->
            [ "ClickedDownloadPdf" ]

        ClickedExportToSimpleos ->
            [ "ClickedExportToSimpleos" ]

        ClickedCopyPrivateKey ->
            [ "ClickedCopyPrivateKey" ]

        CopiedPrivateKey ->
            [ "CopiedPrivateKey" ]

        ClosedExportToSimpleosModal ->
            [ "ClosedExportToSimpleosModal" ]

        ClickedChangePin ->
            [ "ClickedChangePin" ]

        ClosedNewPinModal ->
            [ "ClosedNewPinModal" ]

        GotPinMsg subMsg ->
            "GotPinMsg" :: View.Pin.msgToString subMsg

        SubmittedNewPin _ ->
            [ "SubmittedNewPin" ]

        ChangedToNewPin _ ->
            [ "ChangedToNewPin" ]

        ClickedDeleteKyc ->
            [ "ClickedDeleteKyc" ]

        ClosedDeleteKycModal ->
            [ "ClosedDeleteKycModal" ]

        AcceptedDeleteKyc ->
            [ "AcceptedDeleteKyc" ]

        FinishedDeletingKyc r ->
            [ "FinishedDeletingKyc", UR.remoteDataToString r ]

        ToggledClaimNotification _ ->
            [ "ToggledClaimNotification" ]

        ToggledTransferNotification _ ->
            [ "ToggledTransferNotification" ]

        ToggledDigestNotification _ ->
            [ "ToggledDigestNotification" ]

        CompletedTogglingNotification _ r ->
            [ "CompletedTogglingNotification", UR.remoteDataToString r ]
