module Page.Settings exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.User
import Eos.Account
import Form.Toggle
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet
import Html exposing (Html, button, div, h2, li, p, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Translation
import UpdateResult as UR
import View.Components
import View.Feedback
import View.Modal



-- MODEL


type alias Model =
    { pdfDownloadStatus : PdfDownloadStatus
    , claimNotificationStatus : ToggleStatus
    , transferNotificationStatus : ToggleStatus
    , digestNotificationStatus : ToggleStatus
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( { pdfDownloadStatus = PdfOk
      , claimNotificationStatus = NotUpdating
      , transferNotificationStatus = NotUpdating
      , digestNotificationStatus = NotUpdating
      }
    , Cmd.none
    )


type ToggleStatus
    = NotUpdating
    | UpdatingTo Bool


type NotificationPreference
    = Claim
    | Transfer
    | Digest


type PdfDownloadStatus
    = PdfOk
    | EncryptedPassphraseNotPresent



-- TYPES


type Msg
    = NoOp
    | ClickedDownloadPdf
    | FinishedDownloadingPdf PdfDownloadStatus
    | ClosedPdfErrorModal
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

        FinishedDownloadingPdf pdfDownloadStatus ->
            { model | pdfDownloadStatus = pdfDownloadStatus }
                |> UR.init

        ClosedPdfErrorModal ->
            { model | pdfDownloadStatus = PdfOk }
                |> UR.init

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
    { title = "TODO"
    , content =
        div [ class "container mx-auto px-4 mt-6 mb-20" ]
            ([ viewAccountSettings
             , viewNotificationPreferences loggedIn model
             , [ viewPdfErrorModal loggedIn model ]
             ]
                |> List.concat
            )
    }


viewPdfErrorModal : LoggedIn.Model -> Model -> Html Msg
viewPdfErrorModal loggedIn model =
    View.Modal.initWith
        { closeMsg = ClosedPdfErrorModal
        , isVisible =
            case model.pdfDownloadStatus of
                PdfOk ->
                    False

                EncryptedPassphraseNotPresent ->
                    True
        }
        -- TODO - I18N
        |> View.Modal.withHeader "Sorry, we can't find your 12 words"
        |> View.Modal.withBody
            [ p [ class "my-3" ]
                -- TODO - I18N
                [ text "Please, check if you have your 12 words saved during the registration process and use them for further signing in." ]
            , p [ class "my-3" ]
                -- TODO - I18N
                [ text "If you completely lost your 12 words, please, contact us and provide this private key and we will help you to recover:"
                ]
            , case LoggedIn.maybePrivateKey loggedIn of
                Nothing ->
                    text ""

                Just pk ->
                    p [ class "font-bold my-3 text-lg text-center border p-4 rounded-sm bg-gray-100" ]
                        [ text (Eos.Account.privateKeyToString pk)
                        ]
            ]
        |> View.Modal.toHtml


viewAccountSettings : List (Html Msg)
viewAccountSettings =
    [ h2 [ class "mb-4" ]
        -- TODO - I18N
        [ text "Account ", span [ class "font-bold" ] [ text "settings" ] ]
    , ul [ class "bg-white rounded-md p-4 divide-y" ]
        [ viewCardItem
            -- TODO - I18N
            [ text "My 12 words"

            -- TODO - I18N
            , button
                [ class "button button-secondary"
                , onClick ClickedDownloadPdf
                ]
                [ text "Download" ]
            ]
        , viewCardItem
            -- TODO - I18N
            [ text "My security PIN"

            -- TODO - I18N
            , button [ class "button button-secondary" ] [ text "Change" ]
            ]
        , viewCardItem
            [ span [ class "flex items-center" ]
                [ -- TODO - I18N
                  text "KYC Data"
                , View.Components.tooltip
                    { message = "TODO"
                    , iconClass = "text-orange-300"
                    , containerClass = ""
                    }
                ]

            -- TODO - I18N
            , button [ class "button button-secondary" ] [ text "Add" ]
            ]
        ]
    ]


viewNotificationPreferences : LoggedIn.Model -> Model -> List (Html Msg)
viewNotificationPreferences loggedIn model =
    [ h2 [ class "mb-4 mt-10" ]
        -- TODO - I18N
        [ span [ class "font-bold" ] [ text "E-mail" ], text " notifications" ]
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
                        -- TODO - I18N
                        { label = "Claim notification"
                        , id = "claim_notification"
                        , onToggle = ToggledClaimNotification
                        , value = updatingOrProfile .claimNotificationStatus .claimNotification
                        }
                    ]
                , viewCardItem
                    [ viewNotificationToggle loggedIn.shared.translators
                        -- TODO - I18N
                        { label = "Transfer notification"
                        , id = "transfer_notification"
                        , onToggle = ToggledTransferNotification
                        , value = updatingOrProfile .transferNotificationStatus .transferNotification
                        }
                    ]
                , viewCardItem
                    [ viewNotificationToggle loggedIn.shared.translators
                        -- TODO - I18N
                        { label = "Monthly news about the community"
                        , id = "digest_notification"
                        , onToggle = ToggledDigestNotification
                        , value = updatingOrProfile .digestNotificationStatus .digest
                        }
                    ]
                ]

        RemoteData.Loading ->
            div [ class "bg-white rounded-md p-4 divide-y" ]
                [ div [ class "w-20 animate-skeleton-loading" ] []
                , div [ class "w-20 animate-skeleton-loading" ] []
                , div [ class "w-20 animate-skeleton-loading" ] []
                ]

        RemoteData.NotAsked ->
            div [ class "bg-white rounded-md p-4 divide-y" ]
                [ div [ class "w-20 animate-skeleton-loading" ] []
                , div [ class "w-20 animate-skeleton-loading" ] []
                , div [ class "w-20 animate-skeleton-loading" ] []
                ]

        RemoteData.Failure err ->
            Debug.todo ""
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
                |> Result.map
                    (\isDownloaded ->
                        FinishedDownloadingPdf
                            (if isDownloaded then
                                PdfOk

                             else
                                EncryptedPassphraseNotPresent
                            )
                    )
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

        FinishedDownloadingPdf _ ->
            [ "FinishedDownloadingPdf" ]

        ClosedPdfErrorModal ->
            [ "ClosedPdfErrorModal" ]

        ToggledClaimNotification _ ->
            [ "ToggledClaimNotification" ]

        ToggledTransferNotification _ ->
            [ "ToggledTransferNotification" ]

        ToggledDigestNotification _ ->
            [ "ToggledDigestNotification" ]

        CompletedTogglingNotification _ r ->
            [ "CompletedTogglingNotification", UR.remoteDataToString r ]
