module Page.Community.Settings.ActionEditor exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Action exposing (Action)
import Browser.Dom
import Cambiatus.Enum.VerificationType as VerificationType
import Community
import Date
import Dict
import Eos
import Eos.Account as Eos
import Form
import Form.Checkbox
import Form.DatePicker
import Form.File
import Form.Radio
import Form.RichText
import Form.Text
import Form.Toggle
import Form.UserPicker
import Form.Validate
import Html exposing (Html, b, button, div, p, span, text)
import Html.Attributes exposing (class, classList, disabled, type_)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Log
import Markdown exposing (Markdown)
import Maybe.Extra
import Page
import Ports
import Profile
import RemoteData
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared exposing (Shared)
import Task
import Time
import Time.Extra
import UpdateResult as UR
import Utils
import View.Components
import View.Feedback as Feedback



-- INIT


init : LoggedIn.Model -> Action.ObjectiveId -> Maybe Action.Id -> UpdateResult
init _ objectiveId actionId =
    { objectiveId = objectiveId
    , actionId = actionId
    , status = Loading
    }
        |> UR.init
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.ObjectivesField)
        |> UR.addCmd
            (Browser.Dom.setViewport 0 0
                |> Task.perform (\_ -> NoOp)
            )



-- MODEL


type alias Model =
    { objectiveId : Action.ObjectiveId
    , actionId : Maybe Action.Id
    , status : Status
    }


type Status
    = Authorized (Form.Model FormInput) (Maybe Action)
    | Loading
    | NotFound
    | Unauthorized


type alias FormInput =
    { description : Form.RichText.Model
    , image : Form.File.SingleModel
    , reward : String
    , useDateValidation : Bool
    , expirationDate : Form.DatePicker.Model
    , useUsagesValidation : Bool
    , maxUsages : String
    , usagesLeft : Maybe String
    , verificationInput : VerificationInput
    }


initFormInput : Shared -> Community.Model -> Maybe Action -> Form.Model FormInput
initFormInput shared community maybeAction =
    let
        afterNow posix =
            if Time.posixToMillis posix >= Time.posixToMillis shared.now then
                posix

            else
                shared.now
    in
    Form.init
        { description =
            maybeAction
                |> Maybe.map .description
                |> Form.RichText.initModel "description-editor"
        , image =
            Form.File.initSingle
                { fileUrl = Maybe.andThen .image maybeAction
                , aspectRatio = Just (388 / 144)
                }
        , reward =
            maybeAction
                |> Maybe.map .reward
                |> Maybe.withDefault 0
                |> Eos.formatSymbolAmount shared.translators community.symbol
        , useDateValidation =
            maybeAction
                |> Maybe.map (.deadline >> Maybe.Extra.isJust)
                |> Maybe.withDefault False
        , expirationDate =
            maybeAction
                |> Maybe.map
                    (.deadline
                        >> Utils.fromMaybeDateTime
                        >> afterNow
                    )
                |> Maybe.withDefault shared.now
                |> Date.fromPosix shared.timezone
                |> Form.DatePicker.initModel
        , useUsagesValidation =
            maybeAction
                |> Maybe.map .usages
                |> Maybe.withDefault 0
                |> (\usages -> usages > 0)
        , maxUsages =
            maybeAction
                |> Maybe.map .usages
                |> Maybe.withDefault 0
                |> String.fromInt
        , usagesLeft =
            maybeAction
                |> Maybe.andThen
                    (\action ->
                        if action.usages > 0 then
                            Just (String.fromInt action.usagesLeft)

                        else
                            Nothing
                    )
        , verificationInput =
            { verificationType =
                maybeAction
                    |> Maybe.map .verificationType
                    |> Maybe.withDefault VerificationType.Automatic
            , minVotes =
                maybeAction
                    |> Maybe.andThen (.verifications >> minVotesFromInt)
                    |> Maybe.withDefault Three
            , verifiers =
                Form.UserPicker.initMultiple
                    { id = "verifiers-picker"
                    , selectedProfiles =
                        maybeAction
                            |> Maybe.map .validators
                            |> Maybe.withDefault []
                    }
            , verifierReward =
                maybeAction
                    |> Maybe.map .verifierReward
                    |> Maybe.withDefault 0
                    |> Eos.formatSymbolAmount shared.translators community.symbol
            , fileValidation =
                { useFileValidation =
                    maybeAction
                        |> Maybe.map .hasProofPhoto
                        |> Maybe.withDefault False
                , useVerificationCode =
                    maybeAction
                        |> Maybe.map .hasProofCode
                        |> Maybe.withDefault False
                , instructions = Form.RichText.initModel "proof-instructions-input" Nothing
                }
            }
        }



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = NoOp
    | CompletedLoadObjectives Community.Model (List Community.Objective)
    | ClosedAuthModal
    | GotFormMsg (Form.Msg FormInput)
    | SubmittedForm (Maybe Action) FormOutput
    | ClickedToggleCompleted Action FormOutput
    | CompletedSavingAction (Result Value String)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared } as loggedIn) =
    let
        { t } =
            shared.translators

        mapForm : (Form.Model FormInput -> Form.Model FormInput) -> Status -> Status
        mapForm fn status =
            case status of
                Authorized form_ maybeAction ->
                    Authorized (fn form_) maybeAction

                _ ->
                    status
    in
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadObjectives community objectives ->
            if community.creator == loggedIn.accountName then
                let
                    maybeObjective =
                        List.find (.id >> (==) model.objectiveId) objectives
                in
                case ( maybeObjective, model.actionId ) of
                    ( Just objective, Just actionId ) ->
                        let
                            maybeAction =
                                List.find (.id >> (==) actionId) objective.actions
                        in
                        case maybeAction of
                            Just action ->
                                { model | status = Authorized (initFormInput shared community (Just action)) (Just action) }
                                    |> UR.init

                            Nothing ->
                                { model | status = NotFound }
                                    |> UR.init

                    ( Just _, Nothing ) ->
                        { model | status = Authorized (initFormInput shared community Nothing) Nothing }
                            |> UR.init

                    ( Nothing, _ ) ->
                        { model | status = NotFound }
                            |> UR.init

            else
                { model | status = Unauthorized }
                    |> UR.init

        ClosedAuthModal ->
            { model | status = mapForm (Form.withDisabled False) model.status }
                |> UR.init

        GotFormMsg subMsg ->
            case model.status of
                Authorized formModel maybeAction ->
                    Form.update shared subMsg formModel
                        |> UR.fromChild
                            (\form_ -> { model | status = Authorized form_ maybeAction })
                            GotFormMsg
                            LoggedIn.addFeedback
                            model

                _ ->
                    UR.init model

        SubmittedForm maybeAction formOutput ->
            { model | status = mapForm (Form.withDisabled True) model.status }
                |> UR.init
                |> UR.addPort
                    (upsertAction
                        msg
                        maybeAction
                        loggedIn
                        model
                        formOutput
                    )
                |> LoggedIn.withPrivateKey loggedIn
                    []
                    model
                    { successMsg = msg, errorMsg = ClosedAuthModal }

        ClickedToggleCompleted action formOutput ->
            { model | status = mapForm (Form.withDisabled True) model.status }
                |> UR.init
                |> UR.addPort
                    (upsertAction
                        msg
                        (Just { action | isCompleted = not action.isCompleted })
                        loggedIn
                        model
                        formOutput
                    )
                |> LoggedIn.withPrivateKey loggedIn
                    []
                    model
                    { successMsg = msg, errorMsg = ClosedAuthModal }

        CompletedSavingAction (Ok _) ->
            model
                |> UR.init
                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey Route.CommunitySettingsObjectives)
                |> UR.addExt (ShowFeedback Feedback.Success (t "community.actions.save_success"))
                -- TODO - This only works sometimes
                |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ObjectivesField)
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = msg
                    , message = "Saved action"
                    , data = Dict.empty
                    , level = Log.DebugLevel
                    }

        CompletedSavingAction (Err val) ->
            { model | status = mapForm (Form.withDisabled False) model.status }
                |> UR.init
                |> UR.logJsonValue msg
                    (Just loggedIn.accountName)
                    "Got an error when saving action"
                    { moduleName = "Page.Community.Settings.ActionEditor", function = "update" }
                    []
                    val
                |> UR.addExt (ShowFeedback Feedback.Failure (t "error.unknown"))


upsertAction : Msg -> Maybe Action -> LoggedIn.Model -> Model -> FormOutput -> Ports.JavascriptOutModel Msg
upsertAction msg maybeAction loggedIn model formOutput =
    let
        posixDeadline date =
            Time.Extra.partsToPosix loggedIn.shared.timezone
                { year = Date.year date
                , month = Date.month date
                , day = Date.day date
                , hour = 23
                , minute = 59
                , second = 59
                , millisecond = 0
                }

        getFromVerification accessor =
            case formOutput.verification of
                Automatic ->
                    Nothing

                Manual manual ->
                    Just (accessor manual)

        getFromFileValidation accessor =
            getFromVerification .fileValidation
                |> Maybe.andThen
                    (\fileValidation ->
                        case fileValidation of
                            NoFileValidation ->
                                Nothing

                            WithFileValidation file ->
                                Just (accessor file)
                    )

        encodeBool =
            Eos.boolToEosBool >> Eos.encodeEosBool
    in
    { responseAddress = msg
    , responseData = Encode.null
    , data =
        Eos.encodeTransaction
            [ { accountName = loggedIn.shared.contracts.community
              , name = "upsertaction"
              , authorization =
                    { actor = loggedIn.accountName
                    , permissionName = Eos.samplePermission
                    }
              , data =
                    Encode.object
                        [ ( "community_id", Eos.encodeSymbol formOutput.reward.symbol )
                        , ( "action_id"
                          , Action.encodeId
                                (Maybe.withDefault (Action.idFromInt 0)
                                    model.actionId
                                )
                          )
                        , ( "objective_id", Action.encodeObjectiveId model.objectiveId )
                        , ( "creator", Eos.encodeName loggedIn.accountName )
                        , ( "description", Markdown.encode formOutput.description )
                        , ( "reward", Eos.encodeAsset formOutput.reward )
                        , ( "deadline"
                          , formOutput.expirationDate
                                |> Maybe.map (posixDeadline >> Time.posixToMillis)
                                |> Maybe.withDefault 0
                                |> Encode.int
                          )
                        , ( "usages"
                          , formOutput.maxUsages
                                |> Maybe.withDefault 0
                                |> Encode.int
                          )
                        , ( "usages_left"
                          , formOutput.usagesLeft
                                |> Maybe.Extra.orElse formOutput.maxUsages
                                |> Maybe.withDefault 0
                                |> Encode.int
                          )
                        , ( "verifications"
                          , getFromVerification (.minVotes >> minVotesToInt)
                                |> Maybe.withDefault 0
                                |> Encode.int
                          )
                        , ( "verification_type"
                          , formOutput.verification
                                |> verificationToString
                                |> Encode.string
                          )
                        , ( "validators_str"
                          , getFromVerification .verifiers
                                |> Maybe.withDefault []
                                |> List.map (.account >> Eos.nameToString)
                                |> String.join "-"
                                |> Encode.string
                          )
                        , ( "verifier_reward"
                          , getFromVerification .verifierReward
                                |> Maybe.withDefault { amount = 0, symbol = formOutput.reward.symbol }
                                |> Eos.encodeAsset
                          )
                        , ( "has_proof_photo"
                          , getFromFileValidation (\_ -> True)
                                |> Maybe.withDefault False
                                |> encodeBool
                          )
                        , ( "has_proof_code"
                          , getFromFileValidation .useVerificationCode
                                |> Maybe.withDefault False
                                |> encodeBool
                          )
                        , ( "photo_proof_instructions"
                          , getFromFileValidation .instructions
                                |> Maybe.withDefault Markdown.empty
                                |> Markdown.encode
                          )
                        , ( "is_completed"
                          , case maybeAction of
                                Nothing ->
                                    encodeBool False

                                Just action ->
                                    encodeBool action.isCompleted
                          )
                        , ( "image"
                          , case formOutput.image of
                                Nothing ->
                                    Encode.string ""

                                Just image ->
                                    Encode.string image
                          )
                        ]
              }
            ]
    }



-- FORMS


type alias FormOutput =
    { description : Markdown
    , image : Maybe String
    , reward : Eos.Asset
    , expirationDate : Maybe Date.Date
    , maxUsages : Maybe Int
    , usagesLeft : Maybe Int
    , verification : Verification
    }


type Verification
    = Automatic
    | Manual
        { minVotes : MinVotes
        , verifiers : List Profile.Minimal
        , verifierReward : Eos.Asset
        , fileValidation : FileValidation
        }


type MinVotes
    = Three
    | Five
    | Seven
    | Nine


type FileValidation
    = NoFileValidation
    | WithFileValidation { useVerificationCode : Bool, instructions : Markdown }


type alias VerificationInput =
    { verificationType : VerificationType.VerificationType
    , minVotes : MinVotes
    , verifiers : Form.UserPicker.MultiplePickerModel
    , verifierReward : String
    , fileValidation : FileValidationInput
    }


type alias FileValidationInput =
    { useFileValidation : Bool
    , useVerificationCode : Bool
    , instructions : Form.RichText.Model
    }


form : LoggedIn.Model -> Community.Model -> Form.Form msg FormInput FormOutput
form loggedIn community =
    let
        { t } =
            loggedIn.shared.translators
    in
    Form.succeed
        (\description image reward useDateValidation expirationDate useMaxUsages maxUsages usagesLeft verification ->
            { description = description
            , image = image
            , reward = { amount = reward, symbol = community.symbol }
            , expirationDate =
                if useDateValidation then
                    Just expirationDate

                else
                    Nothing
            , maxUsages =
                if useMaxUsages then
                    Just maxUsages

                else
                    Nothing
            , usagesLeft =
                if useMaxUsages then
                    Just usagesLeft

                else
                    Nothing
            , verification = verification
            }
        )
        |> Form.with
            (Form.RichText.init { label = t "community.actions.form.description_label" }
                |> Form.RichText.withEditorContainerAttrs [ class "mb-10" ]
                |> Form.richText
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.markdownLongerThan 10
                            >> Form.Validate.validate loggedIn.shared.translators
                    , value = .description
                    , update = \description input -> { input | description = description }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.File.init { id = "action-image-input " }
                |> Form.File.withLabel (loggedIn.shared.translators.t "community.actions.form.image_label")
                |> Form.File.withGrayBoxVariant loggedIn.shared.translators
                |> Form.File.withAddImagesContainerAttributes [ class "w-full min-h-36 max-h-40 rounded overflow-hidden" ]
                |> Form.File.withEntryContainerAttributes (\_ -> [ class "w-full min-h-36 max-h-40 rounded" ])
                |> Form.File.withImageClass "w-full rounded"
                |> Form.File.withContainerAttributes [ class "w-full sm:w-2/5 mb-2" ]
                |> Form.File.withImageCropperAttributes [ class "rounded" ]
                |> Form.File.withEditIconOverlay
                |> Form.file
                    { parser = Ok
                    , translators = loggedIn.shared.translators
                    , value = .image
                    , update = \image input -> { input | image = image }
                    , externalError = always Nothing
                    }
                |> Form.optional
            )
        |> Form.withNoOutput
            (Form.arbitrary
                (p [ class "sm:w-2/5 mb-10 text-gray-600" ]
                    [ text <| loggedIn.shared.translators.t "community.actions.form.image_guidance" ]
                )
            )
        |> Form.with
            (Form.Text.init
                { label = t "community.actions.form.reward_label"
                , id = "claimant-reward-input"
                }
                |> Form.Text.withPlaceholder (Eos.formatSymbolAmount loggedIn.shared.translators community.symbol 0)
                |> Form.Text.withContainerAttrs [ class "w-full sm:w-2/5" ]
                |> Form.Text.withCurrency community.symbol
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat loggedIn.shared.translators
                            >> Form.Validate.validate loggedIn.shared.translators
                    , value = .reward
                    , update = \reward input -> { input | reward = reward }
                    , externalError = always Nothing
                    }
            )
        |> Form.withNoOutput
            (Form.introspect
                (\values ->
                    Form.Toggle.init
                        { label =
                            p [ class "text-green" ]
                                (if values.useDateValidation || values.useUsagesValidation then
                                    [ b []
                                        [ text <| t "community.actions.form.validation_on"
                                        ]
                                    , text <| t "community.actions.form.validation_on_detail"
                                    ]

                                 else
                                    [ b []
                                        [ text <| t "community.actions.form.validation_off"
                                        ]
                                    , text <| t "community.actions.form.validation_detail"
                                    ]
                                )
                        , id = "expiration-toggle"
                        }
                        |> Form.Toggle.withTopLabel (t "community.actions.form.validity_label")
                        |> Form.Toggle.withToggleSide Form.Toggle.Left
                        |> Form.toggle
                            { parser = Ok
                            , value = \input -> input.useDateValidation || input.useUsagesValidation
                            , update = \_ input -> input
                            , externalError = always Nothing
                            }
                )
            )
        |> Form.with
            (Form.Checkbox.init
                { label =
                    p []
                        [ b [] [ text <| t "community.actions.form.date_validity" ]
                        , text <| t "community.actions.form.date_validity_details"
                        ]
                , id = "date-validity-checkbox"
                }
                |> Form.Checkbox.withContainerAttrs [ class "flex mt-6 mb-3 sm:w-2/5" ]
                |> Form.checkbox
                    { parser = Ok
                    , value = .useDateValidation
                    , update = \useDateValidation input -> { input | useDateValidation = useDateValidation }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    if values.useDateValidation then
                        Form.DatePicker.init
                            { label = t "community.actions.form.date_label"
                            , id = "date-validation-picker"
                            }
                            |> Form.DatePicker.withContainerAttrs [ class "mb-6" ]
                            |> Form.datePicker
                                { parser =
                                    Form.Validate.succeed
                                        >> Form.Validate.required
                                        >> Form.Validate.futureDate loggedIn.shared.timezone loggedIn.shared.now
                                        >> Form.Validate.validate loggedIn.shared.translators
                                , value = .expirationDate
                                , update = \expirationDate input -> { input | expirationDate = expirationDate }
                                , externalError = always Nothing
                                }

                    else
                        loggedIn.shared.now
                            |> Date.fromPosix loggedIn.shared.timezone
                            |> Form.succeed
                )
            )
        |> Form.with
            (Form.Checkbox.init
                { label =
                    p []
                        [ b [] [ text <| t "community.actions.form.quantity_validity" ]
                        , text <| t "community.actions.form.quantity_validity_details"
                        ]
                , id = "usages-validity-checkbox"
                }
                |> Form.Checkbox.withContainerAttrs [ class "flex mb-3 sm:w-2/5" ]
                |> Form.checkbox
                    { parser = Ok
                    , value = .useUsagesValidation
                    , update = \useUsagesValidation input -> { input | useUsagesValidation = useUsagesValidation }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    if values.useUsagesValidation then
                        Form.Text.init
                            { label = t "community.actions.form.quantity_label"
                            , id = "usages-validity-input"
                            }
                            |> Form.Text.withPlaceholder (t "community.actions.form.usages_placeholder")
                            |> Form.Text.withType Form.Text.Number
                            |> Form.Text.asNumeric
                            |> Form.Text.withContainerAttrs [ class "mt-6 sm:w-2/5" ]
                            |> Form.textField
                                { parser =
                                    Form.Validate.succeed
                                        >> Form.Validate.int
                                        >> Form.Validate.intGreaterThan 0
                                        >> Form.Validate.validate loggedIn.shared.translators
                                , value = .maxUsages
                                , update = \maxUsages input -> { input | maxUsages = maxUsages }
                                , externalError = always Nothing
                                }

                    else
                        Form.succeed 0
                )
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    if values.useUsagesValidation && Maybe.Extra.isJust values.usagesLeft then
                        Form.Text.init
                            { label = t "community.actions.form.usages_left_label"
                            , id = "usages-left-input"
                            }
                            |> Form.Text.withType Form.Text.Number
                            |> Form.Text.asNumeric
                            |> Form.Text.withContainerAttrs [ class "sm:w-2/5" ]
                            |> Form.textField
                                { parser =
                                    Form.Validate.succeed
                                        >> Form.Validate.int
                                        >> Form.Validate.intGreaterThanOrEqualTo 0
                                        >> Form.Validate.validate loggedIn.shared.translators
                                , value = .usagesLeft >> Maybe.withDefault ""
                                , update = \usagesLeft_ input -> { input | usagesLeft = Just usagesLeft_ }
                                , externalError = always Nothing
                                }

                    else
                        Form.succeed 0
                )
            )
        |> Form.withNesting
            { value = .verificationInput
            , update = \child parent -> { parent | verificationInput = child }
            }
            (verificationForm loggedIn community)


verificationForm : LoggedIn.Model -> Community.Model -> Form.Form msg VerificationInput Verification
verificationForm loggedIn community =
    let
        { t, tr } =
            loggedIn.shared.translators
    in
    Form.succeed
        (\verificationType minVotes verifiers verifierReward fileValidation ->
            case verificationType of
                VerificationType.Automatic ->
                    Automatic

                VerificationType.Claimable ->
                    Manual
                        { minVotes = minVotes
                        , verifiers = verifiers
                        , verifierReward = { amount = verifierReward, symbol = community.symbol }
                        , fileValidation = fileValidation
                        }
        )
        |> Form.with
            (Form.Radio.init
                { label = t "community.actions.form.verification_label"
                , id = "verification-type-radio"
                , optionToString = VerificationType.toString
                }
                |> Form.Radio.withOption VerificationType.Automatic
                    (span [ class "flex items-center space-x-2" ]
                        [ p []
                            [ b [] [ text <| t "community.actions.form.automatic" ]
                            , text <| t "community.actions.form.automatic_detail"
                            ]
                        , View.Components.tooltip
                            { message = t "community.actions.form.automatic_tooltip"
                            , iconClass = ""
                            , containerClass = ""
                            }
                        ]
                    )
                |> Form.Radio.withOption VerificationType.Claimable
                    (p []
                        [ b [] [ text <| t "community.actions.form.manual" ]
                        , text <| t "community.actions.form.manual_detail"
                        ]
                    )
                |> Form.Radio.withContainerAttrs [ class "my-6" ]
                |> Form.Radio.withLabelAttrs [ class "mb-6" ]
                |> Form.Radio.withDirection Form.Radio.Vertical
                |> Form.radio (VerificationType.fromString >> Maybe.withDefault VerificationType.Automatic)
                    { parser = Ok
                    , value = .verificationType
                    , update = \verificationType input -> { input | verificationType = verificationType }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    case values.verificationType of
                        VerificationType.Automatic ->
                            Form.succeed Three

                        VerificationType.Claimable ->
                            let
                                withOption minVotes =
                                    Form.Radio.withOption minVotes
                                        (span
                                            [ class "h-8 w-8 rounded-full border flex items-center justify-center cursor-pointer sibling-focus ring-orange-300 ring-opacity-50 transition-all"
                                            , classList
                                                [ ( "bg-orange-300 border-orange-300 text-white", values.minVotes == minVotes )
                                                , ( "hover:border-orange-300 hover:text-orange-500 border-gray-500", values.minVotes /= minVotes )
                                                ]
                                            ]
                                            [ text (minVotesToInt minVotes |> String.fromInt) ]
                                        )
                            in
                            Form.Radio.init
                                { label = t "community.actions.form.votes_label"
                                , id = "min-votes-radio"
                                , optionToString = minVotesToInt >> String.fromInt
                                }
                                |> withOption Three
                                |> withOption Five
                                |> withOption Seven
                                |> withOption Nine
                                |> Form.Radio.withContainerAttrs [ class "ml-8 mt-12 mb-6" ]
                                |> Form.Radio.withLabelAttrs [ class "mb-6" ]
                                |> Form.Radio.withHiddenRadioButton True
                                |> Form.radio
                                    (String.toInt
                                        >> Maybe.andThen minVotesFromInt
                                        >> Maybe.withDefault Three
                                    )
                                    { parser = Ok
                                    , value = .minVotes
                                    , update = \minVotes input -> { input | minVotes = minVotes }
                                    , externalError = always Nothing
                                    }
                )
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    case values.verificationType of
                        VerificationType.Automatic ->
                            Form.succeed []

                        VerificationType.Claimable ->
                            Form.UserPicker.init
                                { label =
                                    tr "community.actions.form.verifiers_label_count"
                                        [ ( "count", values.minVotes |> minVotesToInt |> String.fromInt ) ]
                                , currentUser = loggedIn.accountName
                                , profiles = community.members
                                }
                                |> Form.UserPicker.withContainerAttrs [ class "ml-8 sm:w-2/5" ]
                                |> Form.userPickerMultiple
                                    { parser =
                                        Form.Validate.succeed
                                            >> Form.Validate.lengthGreaterThanOrEqualTo (minVotesToInt values.minVotes)
                                            >> Form.Validate.validate loggedIn.shared.translators
                                    , value = .verifiers
                                    , update = \verifiers input -> { input | verifiers = verifiers }
                                    , externalError = always Nothing
                                    }
                                |> Form.withValidationStrategy Form.ValidateOnSubmit
                )
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    case values.verificationType of
                        VerificationType.Automatic ->
                            Form.succeed 0

                        VerificationType.Claimable ->
                            Form.Text.init
                                { label = t "community.actions.form.verifiers_reward_label"
                                , id = "verifier-reward-input"
                                }
                                |> Form.Text.withContainerAttrs [ class "ml-8 sm:w-2/5" ]
                                |> Form.Text.withCurrency community.symbol
                                |> Form.textField
                                    { parser =
                                        Form.Validate.succeed
                                            >> Form.Validate.maskedFloat loggedIn.shared.translators
                                            >> Form.Validate.validate loggedIn.shared.translators
                                    , value = .verifierReward
                                    , update = \verifierReward input -> { input | verifierReward = verifierReward }
                                    , externalError = always Nothing
                                    }
                )
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    case values.verificationType of
                        VerificationType.Automatic ->
                            Form.succeed NoFileValidation

                        VerificationType.Claimable ->
                            Form.mapValues
                                { value = .fileValidation
                                , update = \fileValidation input -> { input | fileValidation = fileValidation }
                                }
                                (fileValidationForm loggedIn.shared.translators)
                )
            )


fileValidationForm : Shared.Translators -> Form.Form msg FileValidationInput FileValidation
fileValidationForm { t } =
    Form.succeed (\_ fileValidation -> fileValidation)
        |> Form.with
            (Form.Checkbox.init
                { label =
                    div []
                        [ p [] [ b [] [ text <| t "community.actions.form.proof_validation" ] ]
                        , p [] [ text <| t "community.actions.form.proof_validation_hint" ]
                        ]
                , id = "file-validation-checkbox"
                }
                |> Form.Checkbox.withContainerAttrs [ class "flex ml-8 sm:w-2/5" ]
                |> Form.checkbox
                    { parser = Ok
                    , value = .useFileValidation
                    , update = \useFileValidation input -> { input | useFileValidation = useFileValidation }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.introspect
                (\{ useFileValidation } ->
                    if useFileValidation then
                        Form.succeed
                            (\useVerificationCode instructions ->
                                WithFileValidation
                                    { useVerificationCode = useVerificationCode
                                    , instructions = instructions
                                    }
                            )
                            |> Form.with
                                (Form.Checkbox.init
                                    { label =
                                        div []
                                            [ p [] [ b [] [ text <| t "community.actions.form.verification_code" ] ]
                                            , p [] [ text <| t "community.actions.form.verification_code_hint" ]
                                            ]
                                    , id = "verification-code-checkbox"
                                    }
                                    |> Form.Checkbox.withContainerAttrs [ class "flex ml-8 my-6 sm:w-2/5" ]
                                    |> Form.checkbox
                                        { parser = Ok
                                        , value = .useVerificationCode
                                        , update = \useVerificationCode input -> { input | useVerificationCode = useVerificationCode }
                                        , externalError = always Nothing
                                        }
                                )
                            |> Form.with
                                (Form.RichText.init { label = t "community.actions.form.verification_instructions" }
                                    |> Form.RichText.withEditorContainerAttrs [ class "ml-8 sm:w-2/5" ]
                                    |> Form.richText
                                        { parser = Ok
                                        , value = .instructions
                                        , update = \instructions input -> { input | instructions = instructions }
                                        , externalError = always Nothing
                                        }
                                )

                    else
                        Form.succeed NoFileValidation
                )
            )


minVotesToInt : MinVotes -> Int
minVotesToInt minVotes =
    case minVotes of
        Three ->
            3

        Five ->
            5

        Seven ->
            7

        Nine ->
            9


minVotesFromInt : Int -> Maybe MinVotes
minVotesFromInt minVotes =
    case minVotes of
        3 ->
            Just Three

        5 ->
            Just Five

        7 ->
            Just Seven

        9 ->
            Just Nine

        _ ->
            Nothing


verificationToString : Verification -> String
verificationToString verificationOutput =
    case verificationOutput of
        Automatic ->
            "automatic"

        Manual _ ->
            "claimable"



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        title =
            let
                action =
                    if model.actionId /= Nothing then
                        t "menu.edit"

                    else
                        t "menu.create"
            in
            action
                ++ " "
                ++ t "community.actions.title"

        content =
            case ( loggedIn.selectedCommunity, model.status ) of
                ( RemoteData.Loading, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.NotAsked, _ ) ->
                    Page.fullPageLoading shared

                ( _, Loading ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Success community, Authorized formModel maybeAction ) ->
                    let
                        form_ =
                            form loggedIn community
                    in
                    div [ class "bg-white" ]
                        [ Page.viewHeader loggedIn (t "community.actions.title")
                        , Form.view [ class "container mx-auto px-4 mt-6 mb-12" ]
                            shared.translators
                            (\submitButton ->
                                [ div [ class "mt-18 sm:flex sm:align-center" ]
                                    [ submitButton
                                        [ class "button button-primary w-full sm:w-48"
                                        , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                                        ]
                                        [ if Maybe.Extra.isJust model.actionId then
                                            text <| t "menu.save"

                                          else
                                            text <| t "menu.create"
                                        ]
                                    , case maybeAction of
                                        Just action ->
                                            button
                                                [ type_ "button"
                                                , class "button button-secondary w-full mt-4 sm:w-48 sm:mt-0 sm:ml-4"
                                                , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                                                , onClick
                                                    (Form.parse form_
                                                        formModel
                                                        { onError = GotFormMsg
                                                        , onSuccess = ClickedToggleCompleted action
                                                        }
                                                    )
                                                ]
                                                [ if action.isCompleted then
                                                    text <| t "community.actions.form.mark_not_completed"

                                                  else
                                                    text <| t "community.actions.form.mark_completed"
                                                ]

                                        Nothing ->
                                            text ""
                                    ]
                                ]
                            )
                            form_
                            formModel
                            { toMsg = GotFormMsg
                            , onSubmit = SubmittedForm maybeAction
                            }
                        ]

                ( RemoteData.Success _, Unauthorized ) ->
                    Page.fullPageNotFound (t "community.edit.unauthorized") ""

                ( RemoteData.Success _, NotFound ) ->
                    Page.fullPageNotFound (t "community.actions.form.not_found") ""

                ( RemoteData.Failure e, _ ) ->
                    Page.fullPageGraphQLError (t "error.invalidSymbol") e
    in
    { title = title
    , content =
        case RemoteData.map .hasObjectives loggedIn.selectedCommunity of
            RemoteData.Success True ->
                content

            RemoteData.Success False ->
                Page.fullPageNotFound
                    (t "error.pageNotFound")
                    (t "community.objectives.disabled.description")

            RemoteData.Loading ->
                Page.fullPageLoading shared

            RemoteData.NotAsked ->
                Page.fullPageLoading shared

            RemoteData.Failure e ->
                Page.fullPageGraphQLError (t "community.error_loading") e
    }



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityFieldLoaded community (Community.ObjectivesValue objectives) ->
            Just (CompletedLoadObjectives community objectives)

        _ ->
            Nothing


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    let
        decodeCompletedSavingAction =
            Json.decodeValue
                (Json.oneOf
                    [ Json.field "transactionId" Json.string
                        |> Json.map Ok
                    , Json.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << CompletedSavingAction)
                |> Result.withDefault Nothing
    in
    case addr of
        "SubmittedForm" :: _ ->
            decodeCompletedSavingAction

        "ClickedToggleCompleted" :: _ ->
            decodeCompletedSavingAction

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadObjectives _ _ ->
            [ "CompletedLoadObjectives" ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        SubmittedForm _ _ ->
            [ "SubmittedForm" ]

        ClickedToggleCompleted _ _ ->
            [ "ClickedToggleCompleted" ]

        CompletedSavingAction r ->
            [ "CompletedSavingAction", UR.resultToString r ]
