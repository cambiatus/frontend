module Page.Community.ActionEditor exposing
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
import Cambiatus.Scalar exposing (DateTime(..))
import Community
import DataValidator
    exposing
        ( Validator
        , getInput
        , greaterThan
        , greaterThanOrEqual
        , hasErrors
        , isOdd
        , lengthGreaterThanOrEqual
        , listErrors
        , newValidator
        , updateInput
        , validate
        )
import Date
import DatePicker
import Dict
import Eos
import Eos.Account as Eos
import Form
import Form.Checkbox
import Form.DatePicker
import Form.Radio
import Form.RichText
import Form.Text
import Form.Toggle
import Form.UserPicker
import Html exposing (Html, b, button, div, img, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, id, src, tabindex)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import I18Next
import Icons
import Json.Decode as Json exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Log
import Markdown exposing (Markdown)
import Mask
import Maybe.Extra
import Page
import Profile
import Profile.Summary
import RemoteData
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared exposing (Shared)
import Simple.Fuzzy
import Task
import Time
import Time.Extra
import UpdateResult as UR
import Utils
import View.Components
import View.Feedback as Feedback
import View.Form
import View.Form.Checkbox as Checkbox
import View.Form.Input as Input
import View.Form.Radio as Radio
import View.Form.Toggle as Toggle
import View.MarkdownEditor as MarkdownEditor
import View.Modal as Modal



-- INIT


type alias ObjectiveId =
    Int


type alias ActionId =
    Int


init : LoggedIn.Model -> ObjectiveId -> Maybe ActionId -> UpdateResult
init loggedIn objectiveId actionId =
    { status = Loading
    , objectiveId = objectiveId
    , actionId = actionId
    , form = initForm loggedIn.shared
    , multiSelectState = Select.newState ""
    , showAutomaticActionTooltip = False
    , status2 = Loading2
    }
        |> UR.init
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.ObjectivesField)



-- MODEL


type alias Model =
    { status : Status
    , objectiveId : ObjectiveId
    , actionId : Maybe ActionId
    , form : Form
    , multiSelectState : Select.State
    , showAutomaticActionTooltip : Bool
    , status2 : Status2
    }


type Status
    = Authorized
    | Loading
    | NotFound
    | Unauthorized


type Status2
    = Authorized2 FormInput
    | Loading2
    | NotFound2
    | Unauthorized2


type ActionValidation
    = NoValidation
    | Validations (Maybe Date.Date) (Maybe (Validator String)) -- Date validation, usage validate


type Verification
    = Automatic
    | Manual
        { verifiersValidator : Validator (List Profile.Minimal)
        , verifierRewardValidator : Validator String
        , minVotesValidator : Validator Int
        , photoProof : PhotoProof
        , profileSummaries : List Profile.Summary.Model
        }


type PhotoProof
    = Enabled ProofNumberPresence
    | Disabled


type ProofNumberPresence
    = WithProofNumber
    | WithoutProofNumber


type SaveStatus
    = NotAsked
    | Saving
    | Failed


type alias Form =
    { description : MarkdownEditor.Model
    , descriptionError : Maybe ( String, I18Next.Replacements )
    , reward : Validator String
    , validation : ActionValidation
    , verification : Verification
    , usagesLeft : Maybe (Validator String) -- Only available on edit
    , isCompleted : Bool
    , deadlinePicker : DatePicker.DatePicker
    , deadlineError : Maybe String
    , saveStatus : SaveStatus
    , instructions : MarkdownEditor.Model
    , instructionsError : Maybe ( String, I18Next.Replacements )
    }


type alias FormInput =
    { description : Form.RichText.Model
    , reward : String
    , useDateValidation : Bool
    , expirationDate : Form.DatePicker.Model
    , useUsagesValidation : Bool
    , maxUsages : String
    , verificationInput : VerificationInput
    }


initFormInput : Shared -> Maybe Action -> FormInput
initFormInput shared maybeAction =
    { description =
        maybeAction
            |> Maybe.map .description
            |> Form.RichText.initModel "description-editor"
    , reward =
        maybeAction
            |> Maybe.map (.reward >> String.fromFloat)
            |> Maybe.withDefault ""
    , useDateValidation =
        maybeAction
            |> Maybe.map (.deadline >> Maybe.Extra.isJust)
            |> Maybe.withDefault False
    , expirationDate =
        maybeAction
            |> Maybe.map (.deadline >> Utils.fromMaybeDateTime)
            |> Maybe.withDefault shared.now
            |> Date.fromPosix shared.timezone
            |> Form.DatePicker.initModel
    , useUsagesValidation =
        maybeAction
            |> Maybe.map .usages
            |> Maybe.withDefault 0
            |> (\usages -> usages > 0)
    , maxUsages = ""
    , verificationInput =
        { verificationType =
            maybeAction
                |> Maybe.map .verificationType
                |> Maybe.withDefault VerificationType.Automatic
        , minVotes =
            maybeAction
                |> Maybe.andThen (.verifications >> minVotesFromInt)
                |> Maybe.withDefault Three

        -- TODO - Initialize verifiers
        , verifiers = Form.UserPicker.initMultiple { id = "verifiers-picker" }
        , verifierReward =
            maybeAction
                |> Maybe.map (.verifierReward >> String.fromFloat)
                |> Maybe.withDefault ""
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


type alias FormOutput =
    { description : Markdown
    , reward : Float
    , expirationDate : Maybe Date.Date
    , maxUsages : Maybe Int
    , verificationOutput : VerificationOutput
    }


createForm : LoggedIn.Model -> Form.Form FormInput FormOutput
createForm loggedIn =
    Form.succeed
        (\description reward useDateValidation expirationDate useMaxUsages maxUsages verification ->
            { description = description
            , reward = reward
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
            , verificationOutput = verification
            }
        )
        |> Form.with
            (Form.RichText.init { label = "Description (TODO I18N)" }
                |> Form.richText
                    { parser = Ok
                    , value = .description
                    , update = \description input -> { input | description = description }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = "Claimants reward (TODO I18N)", id = "claimant-reward-input" }
                |> Form.textField
                    { -- TODO - Use symbol
                      parser = String.toFloat >> Result.fromMaybe "Needs to be an int"
                    , value = .reward
                    , update = \reward input -> { input | reward = reward }
                    , externalError = always Nothing
                    }
            )
        -- TODO - Check this toggle
        |> Form.withNoOutput
            (Form.Toggle.init
                { label = Html.text "Expiration on or off"
                , id = "expiration-toggle"
                }
                |> Form.toggle
                    { parser = Ok
                    , value = \input -> input.useDateValidation || input.useUsagesValidation
                    , update = \_ input -> input
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Checkbox.init
                { label = Html.text "Validity by date (TODO I18N)"
                , id = "date-validity-checkbox"
                }
                |> Form.checkbox
                    { parser = Ok
                    , value = .useDateValidation
                    , update = \useDateValidation input -> { input | useDateValidation = useDateValidation }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.DatePicker.init { label = "Pick a date (TODO I18N)", id = "date-validation-picker" }
                |> Form.datePicker
                    { parser = Result.fromMaybe "Mandatory"
                    , value = .expirationDate
                    , update = \expirationDate input -> { input | expirationDate = expirationDate }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Checkbox.init
                { label = Html.text "Validity by usages (TODO I18N)"
                , id = "usages-validity-checkbox"
                }
                |> Form.checkbox
                    { parser = Ok
                    , value = .useUsagesValidation
                    , update = \useUsagesValidation input -> { input | useUsagesValidation = useUsagesValidation }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                { label = "Max usages (TODO I18N)"
                , id = "usages-validity-input"
                }
                |> Form.textField
                    { parser = String.toInt >> Result.fromMaybe "Invalid int"
                    , value = .maxUsages
                    , update = \maxUsages input -> { input | maxUsages = maxUsages }
                    , externalError = always Nothing
                    }
            )
        |> Form.withNesting
            { value = .verificationInput
            , update = \child parent -> { parent | verificationInput = child }
            }
            (verificationForm loggedIn)


type alias VerificationInput =
    { verificationType : VerificationType.VerificationType
    , minVotes : MinVotes
    , verifiers : Form.UserPicker.MultiplePickerModel
    , verifierReward : String
    , fileValidation : FileValidationInput
    }


type VerificationOutput
    = AutomaticFormOutput
    | ManualFormOutput
        { minVotes : MinVotes
        , verifiers : List Profile.Minimal
        , verifierReward : Float
        , fileValidation : FileValidation
        }


type MinVotes
    = Three
    | Five
    | Seven
    | Nine


verificationForm : LoggedIn.Model -> Form.Form VerificationInput VerificationOutput
verificationForm loggedIn =
    Form.succeed
        (\verificationType minVotes verifiers verifierReward fileValidation ->
            case verificationType of
                VerificationType.Automatic ->
                    AutomaticFormOutput

                VerificationType.Claimable ->
                    ManualFormOutput
                        { minVotes = minVotes
                        , verifiers = verifiers
                        , verifierReward = verifierReward
                        , fileValidation = fileValidation
                        }
        )
        |> Form.with
            (Form.Radio.init
                { label = "Verification type (TODO I18N)"
                , id = "verification-type-radio"
                , optionToString = VerificationType.toString
                }
                |> Form.radio (VerificationType.fromString >> Maybe.withDefault VerificationType.Automatic)
                    { parser = Ok
                    , value = .verificationType
                    , update = \verificationType input -> { input | verificationType = verificationType }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Radio.init
                { label = "Minimum number of votes"
                , id = "min-votes-radio"
                , optionToString = minVotesToInt >> String.fromInt
                }
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
        |> Form.with
            (Form.UserPicker.init
                { label = "Pick validators"
                , currentUser = loggedIn.accountName
                , profiles = []
                }
                |> Form.userPickerMultiple
                    { parser = Ok
                    , value = .verifiers
                    , update = \verifiers input -> { input | verifiers = verifiers }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                { label = "Verifier reward"
                , id = "verifier-reward-input"
                }
                |> Form.textField
                    { parser = String.toFloat >> Result.fromMaybe "Invalid float"
                    , value = .verifierReward
                    , update = \verifierReward input -> { input | verifierReward = verifierReward }
                    , externalError = always Nothing
                    }
            )
        |> Form.withNesting
            { value = .fileValidation
            , update = \fileValidation input -> { input | fileValidation = fileValidation }
            }
            fileValidationForm


type alias FileValidationInput =
    { useFileValidation : Bool
    , useVerificationCode : Bool
    , instructions : Form.RichText.Model
    }


type FileValidation
    = NoFileValidation
    | WithFileValidation { useVerificationCode : Bool, instructions : Markdown }


fileValidationForm : Form.Form FileValidationInput FileValidation
fileValidationForm =
    Form.succeed (\_ fileValidationOutput -> fileValidationOutput)
        |> Form.with
            (Form.Checkbox.init
                { label = Html.text "Use PDF or photo"
                , id = "file-validation-checkbox"
                }
                |> Form.checkbox
                    { parser = Ok
                    , value = .useFileValidation
                    , update = \useFileValidation input -> { input | useFileValidation = useFileValidation }
                    , externalError = always Nothing
                    }
            )
        |> Form.withConditional
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
                                { label = Html.text "Use verificatino code"
                                , id = "verification-code-checkbox"
                                }
                                |> Form.checkbox
                                    { parser = Ok
                                    , value = .useVerificationCode
                                    , update = \useVerificationCode input -> { input | useVerificationCode = useVerificationCode }
                                    , externalError = always Nothing
                                    }
                            )
                        |> Form.with
                            (Form.RichText.init { label = "Instructions" }
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


initForm : Shared -> Form
initForm shared =
    { description = MarkdownEditor.init "action-description"
    , descriptionError = Nothing
    , reward = defaultReward shared.translators
    , validation = NoValidation
    , verification = Automatic
    , usagesLeft = Nothing
    , isCompleted = False
    , deadlinePicker = DatePicker.initFromDate (Date.fromPosix shared.timezone shared.now)
    , deadlineError = Nothing
    , saveStatus = NotAsked
    , instructions = MarkdownEditor.init "photo-proof-instructions"
    , instructionsError = Nothing
    }


editForm : LoggedIn.Model -> Form -> Action -> Form
editForm { shared } form action =
    let
        maybeDeadline : Maybe Date.Date
        maybeDeadline =
            action.deadline
                |> Maybe.map
                    (Utils.fromDateTime
                        >> Date.fromPosix shared.timezone
                    )

        usagesValidator : Maybe (Validator String)
        usagesValidator =
            if action.usages > 0 then
                defaultUsagesValidator
                    |> updateInput (String.fromInt action.usages)
                    |> Just

            else
                Nothing

        validation : ActionValidation
        validation =
            if action.usages > 0 || action.deadline /= Nothing then
                Validations maybeDeadline usagesValidator

            else
                NoValidation

        verifiers =
            if VerificationType.toString action.verificationType == "AUTOMATIC" then
                []

            else
                action.validators

        verification : Verification
        verification =
            if VerificationType.toString action.verificationType == "AUTOMATIC" then
                Automatic

            else
                let
                    minVotesValidator =
                        defaultMinVotes
                            |> updateInput action.verifications

                    verifiersValidator =
                        defaultVerifiersValidator verifiers (getInput minVotesValidator)
                            |> updateInput verifiers

                    verifierRewardValidator =
                        defaultVerificationReward shared.translators
                            |> updateInput (String.fromFloat action.verifierReward)

                    photoProof =
                        case ( action.hasProofPhoto, action.hasProofCode ) of
                            ( True, True ) ->
                                Enabled WithProofNumber

                            ( True, False ) ->
                                Enabled WithoutProofNumber

                            ( _, _ ) ->
                                Disabled

                    profileSummaries =
                        getInput verifiersValidator
                            |> List.length
                            |> Profile.Summary.initMany False
                in
                Manual
                    { verifiersValidator = verifiersValidator
                    , verifierRewardValidator = verifierRewardValidator
                    , minVotesValidator = minVotesValidator
                    , photoProof = photoProof
                    , profileSummaries = profileSummaries
                    }
    in
    { form
        | -- description = MarkdownEditor.setContents action.description form.description
          reward = updateInput (String.fromFloat action.reward) form.reward
        , validation = validation
        , verification = verification
        , usagesLeft = Just (updateInput (String.fromInt action.usagesLeft) defaultUsagesLeftValidator)
        , isCompleted = action.isCompleted
        , instructions =
            case action.photoProofInstructions of
                Just instructions_ ->
                    MarkdownEditor.setContents instructions_ form.instructions

                Nothing ->
                    form.instructions
    }


defaultReward : Shared.Translators -> Validator String
defaultReward _ =
    []
        |> newValidator "" Just True


defaultUsagesValidator : Validator String
defaultUsagesValidator =
    []
        |> greaterThan 0
        |> newValidator "" (\s -> Just s) True


defaultVerifiersValidator : List Profile.Minimal -> Int -> Validator (List Profile.Minimal)
defaultVerifiersValidator verifiers minVerifiersQty =
    let
        limit =
            if minVerifiersQty < minVotesLimit then
                minVotesLimit

            else
                minVerifiersQty
    in
    []
        |> lengthGreaterThanOrEqual limit
        |> newValidator verifiers (\s -> Just (String.fromInt (List.length s))) True


defaultUsagesLeftValidator : Validator String
defaultUsagesLeftValidator =
    []
        |> greaterThanOrEqual 0
        |> newValidator "" (\s -> Just s) True


defaultVerificationReward : Shared.Translators -> Validator String
defaultVerificationReward translators =
    []
        |> greaterThanOrEqual 0
        |> newValidator "0"
            (Mask.removeFloat (Shared.decimalSeparators translators)
                >> Just
            )
            True


minVotesLimit : Int
minVotesLimit =
    3


defaultMinVotes : Validator Int
defaultMinVotes =
    []
        |> greaterThanOrEqual (toFloat minVotesLimit)
        |> isOdd
        |> newValidator minVotesLimit (\s -> Just (String.fromInt s)) True


validateForm : Time.Zone -> Time.Posix -> Form -> Form
validateForm timezone now form =
    let
        ( validation, dateError ) =
            case form.validation of
                NoValidation ->
                    ( NoValidation, Nothing )

                Validations (Just dateValidation) (Just usageValidation) ->
                    if Date.diff Date.Days (Date.fromPosix timezone now) dateValidation < 0 then
                        ( Validations (Just dateValidation) (Just (validate usageValidation)), Just "error.validator.date.invalid" )

                    else
                        ( Validations (Just dateValidation) (Just (validate usageValidation)), Nothing )

                Validations (Just dateValidation) Nothing ->
                    if Date.diff Date.Days (Date.fromPosix timezone now) dateValidation < 0 then
                        ( Validations (Just dateValidation) Nothing, Just "error.validator.date.invalid" )

                    else
                        ( Validations (Just dateValidation) Nothing, Nothing )

                Validations Nothing (Just usageValidation) ->
                    ( Validations Nothing (Just (validate usageValidation)), Nothing )

                Validations Nothing Nothing ->
                    ( NoValidation, Nothing )

        verification =
            case form.verification of
                Automatic ->
                    Automatic

                Manual m ->
                    Manual
                        { m
                            | verifiersValidator = validate m.verifiersValidator
                            , verifierRewardValidator = validate m.verifierRewardValidator
                            , minVotesValidator = validate m.minVotesValidator
                        }

        descriptionError =
            if String.length form.description.contents < 10 then
                Just
                    ( "error.validator.text.longer_than"
                    , [ ( "base", String.fromInt 10 ) ]
                    )

            else
                Nothing

        instructionsError =
            if String.length form.instructions.contents < 10 then
                Just
                    ( "error.validator.text.longer_than"
                    , [ ( "base", String.fromInt 10 ) ]
                    )

            else
                Nothing
    in
    { form
        | descriptionError = descriptionError
        , reward = validate form.reward
        , validation = validation
        , deadlineError = dateError
        , verification = verification
        , instructionsError = instructionsError
    }


isFormValid : Form -> Bool
isFormValid form =
    let
        hasDeadlineError =
            case form.deadlineError of
                Just _ ->
                    True

                Nothing ->
                    False

        verificationHasErrors =
            case form.verification of
                Manual m ->
                    hasErrors m.minVotesValidator
                        || hasErrors m.verifiersValidator
                        || hasErrors m.verifierRewardValidator
                        || hasDeadlineError

                Automatic ->
                    -- Automatic verification never has validation errors
                    False

        hasDescriptionErrors =
            String.length form.description.contents < 10
    in
    hasDescriptionErrors
        || hasErrors form.reward
        || verificationHasErrors
        |> not


hasDateValidation : ActionValidation -> Bool
hasDateValidation validation =
    case validation of
        NoValidation ->
            False

        Validations maybeDate _ ->
            case maybeDate of
                Just _ ->
                    True

                Nothing ->
                    False


hasUnitValidation : ActionValidation -> Bool
hasUnitValidation validation =
    case validation of
        NoValidation ->
            False

        Validations _ maybeUnit ->
            case maybeUnit of
                Just _ ->
                    True

                Nothing ->
                    False



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = NoOp
    | CompletedLoadObjectives Community.Model (List Community.Objective)
    | ClosedAuthModal
    | OnSelectVerifier Profile.Minimal
    | OnRemoveVerifier Profile.Minimal
    | SelectMsg (Select.Msg Profile.Minimal)
    | EnteredReward String
    | ClickedCalendar
    | GotDatePickerMsg DatePicker.Msg
    | EnteredUsages String
    | EnteredUsagesLeft String
    | EnteredVerifierReward String
    | EnteredMinVotes Int
    | ToggleValidity
    | ToggleDeadline Bool
    | TogglePhotoProof Bool
    | TogglePhotoProofNumber Bool
    | ToggleUsages Bool
    | MarkAsCompleted
    | SetVerification Verification
    | ValidateForm
    | SaveAction Int -- Send the date
    | GotSaveAction (Result Value String)
    | GotProfileSummaryMsg Int Profile.Summary.Msg
    | GotDescriptionEditorMsg MarkdownEditor.Msg
    | GotInstructionsEditorMsg MarkdownEditor.Msg
    | OpenedAutomaticActionTooltip
    | ClosedAutomaticActionTooltip



---- ACTION CREATE


type alias CreateActionAction =
    { actionId : ActionId
    , objectiveId : ObjectiveId
    , description : String
    , reward : Eos.Asset
    , verifierReward : Eos.Asset
    , deadline : Int
    , usages : String
    , usagesLeft : String
    , verifications : Int
    , verificationType : String
    , validatorsStr : String
    , isCompleted : Int
    , creator : Eos.Name
    , hasProofPhoto : Bool
    , hasProofCode : Bool
    , photoProofInstructions : String
    }


encodeCreateActionAction : CreateActionAction -> Value
encodeCreateActionAction c =
    Encode.object
        [ ( "action_id", Encode.int c.actionId )
        , ( "objective_id", Encode.int c.objectiveId )
        , ( "description", Encode.string c.description )
        , ( "reward", Eos.encodeAsset c.reward )
        , ( "verifier_reward", Eos.encodeAsset c.verifierReward )
        , ( "deadline", Encode.int c.deadline )
        , ( "usages", Encode.string c.usages )
        , ( "usages_left", Encode.string c.usagesLeft )
        , ( "verifications", Encode.string (String.fromInt c.verifications) )
        , ( "verification_type", Encode.string c.verificationType )
        , ( "validators_str", Encode.string c.validatorsStr )
        , ( "is_completed", Encode.int c.isCompleted )
        , ( "creator", Eos.encodeName c.creator )
        , ( "has_proof_photo", Eos.encodeEosBool <| Eos.boolToEosBool c.hasProofPhoto )
        , ( "has_proof_code", Eos.encodeEosBool <| Eos.boolToEosBool c.hasProofCode )
        , ( "photo_proof_instructions", Encode.string c.photoProofInstructions )
        ]


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared } as loggedIn) =
    let
        { t } =
            shared.translators

        oldForm =
            model.form
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
                                { model
                                    | status = Authorized
                                    , form = editForm loggedIn model.form action
                                }
                                    |> UR.init

                            Nothing ->
                                { model | status = NotFound }
                                    |> UR.init

                    ( Just _, Nothing ) ->
                        { model
                            | status = Authorized
                            , form = initForm shared
                        }
                            |> UR.init

                    ( Nothing, _ ) ->
                        { model | status = NotFound }
                            |> UR.init

            else
                { model | status = Unauthorized }
                    |> UR.init

        ClosedAuthModal ->
            { model | form = { oldForm | saveStatus = NotAsked } }
                |> UR.init

        OnSelectVerifier profile ->
            let
                verification =
                    case model.form.verification of
                        Automatic ->
                            model.form.verification

                        Manual m ->
                            let
                                newVerifiers : List Profile.Minimal
                                newVerifiers =
                                    profile :: getInput m.verifiersValidator
                            in
                            Manual
                                { m
                                    | verifiersValidator = updateInput newVerifiers m.verifiersValidator
                                    , profileSummaries =
                                        List.length newVerifiers
                                            |> Profile.Summary.initMany False
                                }
            in
            { model | form = { oldForm | verification = verification } }
                |> UR.init

        OnRemoveVerifier profile ->
            let
                verification =
                    case model.form.verification of
                        Automatic ->
                            model.form.verification

                        Manual m ->
                            let
                                newVerifiers : List Profile.Minimal
                                newVerifiers =
                                    List.filter
                                        (\currVerifier -> currVerifier.account /= profile.account)
                                        (getInput m.verifiersValidator)
                            in
                            Manual
                                { m
                                    | verifiersValidator = updateInput newVerifiers m.verifiersValidator
                                    , profileSummaries =
                                        List.length newVerifiers
                                            |> Profile.Summary.initMany False
                                }
            in
            { model | form = { oldForm | verification = verification } }
                |> UR.init

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration loggedIn.shared False) subMsg model.multiSelectState
            in
            { model | multiSelectState = updated }
                |> UR.init
                |> UR.addCmd cmd

        EnteredReward val ->
            { model | form = { oldForm | reward = updateInput val model.form.reward } }
                |> UR.init

        EnteredUsages val ->
            case model.form.validation of
                NoValidation ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried setting max usages for action, but action's validation has no date"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.ActionEditor", function = "update" }
                            [ { name = "Action"
                              , extras =
                                    Dict.fromList
                                        [ ( "actionId"
                                          , case model.actionId of
                                                Nothing ->
                                                    Encode.null

                                                Just id ->
                                                    Encode.int id
                                          )
                                        , ( "tried", Encode.string val )
                                        ]
                              }
                            , Log.contextFromCommunity loggedIn.selectedCommunity
                            ]

                Validations maybeDate maybeUsage ->
                    case maybeUsage of
                        Just usageValidation ->
                            { model
                                | form =
                                    { oldForm
                                        | validation = Validations maybeDate (Just (updateInput val usageValidation))
                                    }
                            }
                                |> UR.init

                        Nothing ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Tried setting max usages for action, but action's validation has no max usages"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Community.ActionEditor", function = "update" }
                                    [ { name = "Action"
                                      , extras =
                                            Dict.fromList
                                                [ ( "actionId"
                                                  , case model.actionId of
                                                        Nothing ->
                                                            Encode.null

                                                        Just id ->
                                                            Encode.int id
                                                  )
                                                , ( "tried", Encode.string val )
                                                ]
                                      }
                                    , Log.contextFromCommunity loggedIn.selectedCommunity
                                    ]

        EnteredUsagesLeft val ->
            case model.form.usagesLeft of
                Just validator ->
                    { model | form = { oldForm | usagesLeft = Just (updateInput val validator) } }
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init

        EnteredVerifierReward val ->
            let
                verification =
                    case model.form.verification of
                        Automatic ->
                            model.form.verification

                        Manual m ->
                            Manual
                                { m
                                    | verifierRewardValidator = updateInput val m.verifierRewardValidator
                                }
            in
            { model | form = { oldForm | verification = verification } }
                |> UR.init

        EnteredMinVotes val ->
            let
                verification =
                    case model.form.verification of
                        Automatic ->
                            model.form.verification

                        Manual m ->
                            let
                                newMinVotes =
                                    updateInput val m.minVotesValidator

                                newVerifiers =
                                    -- Update min. verifiers quantity
                                    defaultVerifiersValidator (getInput m.verifiersValidator) val
                            in
                            Manual
                                { m
                                    | verifiersValidator = newVerifiers
                                    , minVotesValidator = newMinVotes
                                }
            in
            { model | form = { oldForm | verification = verification } }
                |> UR.init

        ValidateForm ->
            let
                newModel =
                    { model | form = validateForm shared.timezone shared.now model.form }
            in
            if isFormValid newModel.form then
                let
                    millis =
                        case newModel.form.validation of
                            NoValidation ->
                                0

                            Validations maybeDate _ ->
                                maybeDate
                                    |> Maybe.map
                                        (\date ->
                                            Time.Extra.partsToPosix shared.timezone
                                                { year = Date.year date
                                                , month = Date.month date
                                                , day = Date.day date
                                                , hour = 23
                                                , minute = 59
                                                , second = 59
                                                , millisecond = 0
                                                }
                                                |> Time.posixToMillis
                                        )
                                    |> Maybe.withDefault 0
                in
                update (SaveAction millis) model loggedIn

            else
                newModel
                    |> UR.init

        ClickedCalendar ->
            model
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.focus "validity-date-input"
                        |> Task.attempt (\_ -> NoOp)
                    )

        GotDatePickerMsg subMsg ->
            case model.form.validation of
                NoValidation ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried changing action's deadline, but action has no validation"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.ActionEditor", function = "update" }
                            []

                Validations maybeDate usageValidator ->
                    case maybeDate of
                        Just oldDate ->
                            let
                                ( datePicker, datePickerEvent ) =
                                    DatePicker.update (datePickerSettings shared)
                                        subMsg
                                        model.form.deadlinePicker

                                newDate =
                                    case datePickerEvent of
                                        DatePicker.Picked pickedDate ->
                                            pickedDate

                                        _ ->
                                            oldDate
                            in
                            { model
                                | form =
                                    { oldForm
                                        | deadlinePicker = datePicker
                                        , validation = Validations (Just newDate) usageValidator
                                        , deadlineError =
                                            if Date.diff Date.Days (Date.fromPosix shared.timezone shared.now) newDate < 0 then
                                                Just "error.validator.date.invalid"

                                            else
                                                Nothing
                                    }
                            }
                                |> UR.init

                        Nothing ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Changed action's deadline, but action doesn't have a validation date"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Community.ActionEditor", function = "update" }
                                    []

        ToggleValidity ->
            model
                |> UR.init

        TogglePhotoProof isPhotoProofEnabled ->
            let
                verification =
                    case model.form.verification of
                        Automatic ->
                            model.form.verification

                        Manual m ->
                            let
                                newPhotoProof =
                                    if isPhotoProofEnabled then
                                        Enabled WithoutProofNumber

                                    else
                                        Disabled
                            in
                            Manual { m | photoProof = newPhotoProof }
            in
            { model | form = { oldForm | verification = verification } }
                |> UR.init

        TogglePhotoProofNumber isProofNumberEnabled ->
            let
                verification =
                    case model.form.verification of
                        Automatic ->
                            model.form.verification

                        Manual m ->
                            let
                                newPhotoProof =
                                    if isProofNumberEnabled then
                                        Enabled WithProofNumber

                                    else
                                        Enabled WithoutProofNumber
                            in
                            Manual { m | photoProof = newPhotoProof }
            in
            { model | form = { oldForm | verification = verification } }
                |> UR.init

        ToggleDeadline bool ->
            let
                deadlineValidation =
                    if bool then
                        Just (Date.fromPosix shared.timezone shared.now)

                    else
                        Nothing

                usagesValidation =
                    case model.form.validation of
                        NoValidation ->
                            Nothing

                        Validations _ maybeUsages ->
                            maybeUsages
            in
            { model
                | form =
                    { oldForm
                        | validation =
                            if deadlineValidation /= Nothing || usagesValidation /= Nothing then
                                Validations deadlineValidation usagesValidation

                            else
                                NoValidation
                    }
            }
                |> UR.init

        ToggleUsages bool ->
            let
                usagesValidation =
                    if bool then
                        Just defaultUsagesValidator

                    else
                        Nothing

                deadlineValidation =
                    case model.form.validation of
                        NoValidation ->
                            Nothing

                        Validations maybeDate _ ->
                            maybeDate
            in
            { model
                | form =
                    { oldForm
                        | validation =
                            if deadlineValidation /= Nothing || usagesValidation /= Nothing then
                                Validations deadlineValidation usagesValidation

                            else
                                NoValidation
                    }
            }
                |> UR.init

        SetVerification verification ->
            { model | form = { oldForm | verification = verification } }
                |> UR.init

        MarkAsCompleted ->
            let
                newModel =
                    { model | form = { oldForm | isCompleted = True } }
            in
            update ValidateForm newModel loggedIn

        SaveAction isoDate ->
            let
                newModel =
                    { model | form = { oldForm | saveStatus = Saving } }
            in
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    upsertAction loggedIn community newModel isoDate
                        |> LoggedIn.withAuthentication loggedIn
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                _ ->
                    UR.init newModel

        GotSaveAction (Ok _) ->
            model
                |> UR.init
                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey Route.Objectives)
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

        GotSaveAction (Err val) ->
            let
                newModel =
                    { model | form = { oldForm | saveStatus = Failed } }
            in
            newModel
                |> UR.init
                |> UR.logJsonValue msg
                    (Just loggedIn.accountName)
                    "Got an error when saving action"
                    { moduleName = "Page.Community.ActionEditor", function = "update" }
                    []
                    val
                |> UR.addExt (ShowFeedback Feedback.Failure (t "error.unknown"))

        GotProfileSummaryMsg index subMsg ->
            case model.form.verification of
                Manual ({ profileSummaries } as verification) ->
                    let
                        modelForm =
                            model.form

                        newProfileSummaries =
                            List.updateAt index (Profile.Summary.update subMsg) profileSummaries
                    in
                    { model | form = { modelForm | verification = Manual { verification | profileSummaries = newProfileSummaries } } }
                        |> UR.init

                Automatic ->
                    UR.init model

        GotDescriptionEditorMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    MarkdownEditor.update subMsg model.form.description
            in
            { model | form = { oldForm | description = subModel } }
                |> UR.init
                |> UR.addCmd (Cmd.map GotDescriptionEditorMsg subCmd)

        GotInstructionsEditorMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    MarkdownEditor.update subMsg model.form.instructions
            in
            { model | form = { oldForm | instructions = subModel } }
                |> UR.init
                |> UR.addCmd (Cmd.map GotInstructionsEditorMsg subCmd)

        OpenedAutomaticActionTooltip ->
            { model | showAutomaticActionTooltip = True }
                |> UR.init

        ClosedAutomaticActionTooltip ->
            { model | showAutomaticActionTooltip = False }
                |> UR.init


datePickerSettings : Shared -> DatePicker.Settings
datePickerSettings shared =
    let
        defaultSettings =
            DatePicker.defaultSettings
    in
    { defaultSettings
        | changeYear = DatePicker.off
        , placeholder = shared.translators.t "payment_history.pick_date"
        , inputClassList = [ ( "input w-full", True ) ]
        , dateFormatter = Date.format "E, d MMM y"
        , inputId = Just "validity-date-input"
    }


upsertAction : LoggedIn.Model -> Community.Model -> Model -> Int -> UpdateResult
upsertAction loggedIn community model isoDate =
    let
        verifierReward =
            case model.form.verification of
                Automatic ->
                    Eos.Asset 0.0 community.symbol

                Manual { verifierRewardValidator } ->
                    Eos.Asset
                        (getInput verifierRewardValidator
                            |> Mask.removeFloat (Shared.decimalSeparators loggedIn.shared.translators)
                            |> String.toFloat
                            |> Maybe.withDefault 0.0
                        )
                        community.symbol

        usages =
            case model.form.validation of
                Validations _ (Just usageValidator) ->
                    getInput usageValidator

                _ ->
                    "0"

        usagesLeft =
            case model.form.usagesLeft of
                Just u ->
                    getInput u

                Nothing ->
                    usages

        minVotes =
            case model.form.verification of
                Automatic ->
                    0

                Manual { minVotesValidator } ->
                    getInput minVotesValidator

        validators =
            case model.form.verification of
                Automatic ->
                    []

                Manual { verifiersValidator } ->
                    getInput verifiersValidator

        validatorsStr =
            validators
                |> List.map (\v -> Eos.nameToString v.account)
                |> String.join "-"

        verificationType =
            case model.form.verification of
                Automatic ->
                    "automatic"

                Manual _ ->
                    "claimable"

        isCompleted =
            if model.form.isCompleted then
                1

            else
                0

        ( hasProofPhoto, hasProofCode ) =
            case model.form.verification of
                Manual { photoProof } ->
                    case photoProof of
                        Enabled WithProofNumber ->
                            ( True, True )

                        Enabled WithoutProofNumber ->
                            ( True, False )

                        Disabled ->
                            ( False, False )

                _ ->
                    ( False, False )

        instructions =
            if hasProofPhoto then
                model.form.instructions.contents

            else
                ""
    in
    model
        |> UR.init
        |> UR.addPort
            { responseAddress = SaveAction isoDate
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
                            { actionId = model.actionId |> Maybe.withDefault 0
                            , objectiveId = model.objectiveId
                            , description = model.form.description.contents
                            , reward =
                                Eos.Asset
                                    (getInput model.form.reward
                                        |> Mask.removeFloat (Shared.decimalSeparators loggedIn.shared.translators)
                                        |> String.toFloat
                                        |> Maybe.withDefault 0.0
                                    )
                                    community.symbol
                            , verifierReward = verifierReward
                            , deadline = isoDate
                            , usages = usages
                            , usagesLeft = usagesLeft
                            , verifications = minVotes
                            , verificationType = verificationType
                            , validatorsStr = validatorsStr
                            , isCompleted = isCompleted
                            , creator = loggedIn.accountName
                            , hasProofPhoto = hasProofPhoto
                            , hasProofCode = hasProofCode
                            , photoProofInstructions = instructions
                            }
                                |> encodeCreateActionAction
                      }
                    ]
            }



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

                ( RemoteData.Success community, Authorized ) ->
                    div [ class "bg-white" ]
                        [ Page.viewHeader loggedIn (t "community.actions.title")
                        , viewForm loggedIn community model
                        ]

                ( RemoteData.Success _, Unauthorized ) ->
                    Page.fullPageNotFound "not authorized" ""

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


viewForm : LoggedIn.Model -> Community.Model -> Model -> Html Msg
viewForm ({ shared } as loggedIn) community model =
    let
        { t } =
            shared.translators
    in
    div [ class "container mx-auto" ]
        [ div [ class "py-6 px-4" ]
            [ viewLoading model
            , viewDescription loggedIn model.form
            , viewReward loggedIn community model.form
            , viewValidations loggedIn model
            , viewVerifications loggedIn model community
            , div [ class "sm:flex sm:align-center mt-18 mb-12" ]
                [ button
                    [ class "button button-primary w-full sm:w-48"
                    , onClick ValidateForm
                    ]
                    [ if model.actionId /= Nothing then
                        text (t "menu.save")

                      else
                        text (t "menu.create")
                    ]
                , case model.actionId of
                    Just _ ->
                        button [ class "button button-secondary w-full mt-4 sm:w-48 sm:mt-0 sm:ml-4", onClick MarkAsCompleted ]
                            [ text (t "community.actions.form.mark_completed") ]

                    Nothing ->
                        text ""
                ]
            ]
        ]


viewLoading : Model -> Html msg
viewLoading model =
    case model.form.saveStatus of
        Saving ->
            div [ class "modal container" ]
                [ div [ class "modal-bg" ] []
                , div [ class "full-spinner-container h-full" ]
                    [ div [ class "spinner spinner--delay" ] [] ]
                ]

        _ ->
            text ""


viewDescription : LoggedIn.Model -> Form -> Html Msg
viewDescription { shared } form =
    let
        { t, tr } =
            shared.translators
    in
    MarkdownEditor.view
        { translators = shared.translators
        , placeholder = Nothing
        , label = t "community.actions.form.description_label"
        , problem =
            form.descriptionError
                |> Maybe.map (\( key, replacements ) -> tr key replacements)
        , disabled = False
        }
        []
        form.description
        |> Html.map GotDescriptionEditorMsg


viewReward : LoggedIn.Model -> Community.Model -> Form -> Html Msg
viewReward { shared } community form =
    let
        { t } =
            shared.translators
    in
    Input.init
        { label = t "community.actions.form.reward_label"
        , id = "action_reward_field"
        , onInput = EnteredReward
        , disabled = False
        , value = getInput form.reward
        , placeholder = Just (Eos.formatSymbolAmount shared.translators community.symbol 0)
        , problems = Just (listErrors shared.translations form.reward)
        , translators = shared.translators
        }
        |> Input.withContainerAttrs [ class "w-full sm:w-2/5" ]
        |> Input.withCurrency community.symbol
        |> Input.toHtml


viewValidations : LoggedIn.Model -> Model -> Html Msg
viewValidations { shared } model =
    let
        { t } =
            shared.translators

        text_ s =
            text (t s)
    in
    div []
        [ div [ class "mb-6" ]
            [ View.Form.label [] "expiration-toggle" (t "community.actions.form.validity_label")
            , Toggle.init
                { label =
                    p [ class "text-green" ]
                        [ b []
                            [ if model.form.validation == NoValidation then
                                text_ "community.actions.form.validation_off"

                              else
                                text_ "community.actions.form.validation_on"
                            ]
                        , if model.form.validation == NoValidation then
                            text_ "community.actions.form.validation_detail"

                          else
                            text_ "community.actions.form.validation_on_detail"
                        ]
                , id = "expiration-toggle"
                , onToggle = \_ -> ToggleValidity
                , disabled = True
                , value = model.form.validation /= NoValidation
                }
                |> Toggle.withVariant Toggle.Simple
                |> Toggle.withAttrs [ class "mt-6" ]
                |> Toggle.toHtml shared.translators
            ]
        , div
            [ class "mb-6 sm:w-2/5" ]
            [ Checkbox.init
                { description =
                    p []
                        [ b [] [ text_ "community.actions.form.date_validity" ]
                        , text_ "community.actions.form.date_validity_details"
                        ]
                , id = "deadline_checkbox"
                , value = hasDateValidation model.form.validation
                , disabled = False
                , onCheck = ToggleDeadline
                }
                |> Checkbox.withContainerAttrs [ class "flex mb-3" ]
                |> Checkbox.toHtml
            , case model.form.validation of
                NoValidation ->
                    text ""

                Validations maybeDate _ ->
                    case maybeDate of
                        Just date ->
                            div []
                                [ span [ class "label" ]
                                    [ text_ "community.actions.form.date_label" ]
                                , div [ class "mb-10" ]
                                    [ div [ class "relative" ]
                                        [ DatePicker.view (Just date)
                                            (datePickerSettings shared)
                                            model.form.deadlinePicker
                                            |> Html.map GotDatePickerMsg
                                        , img
                                            [ class "absolute right-0 top-0 h-full cursor-pointer"
                                            , src "/icons/calendar.svg"
                                            , tabindex -1
                                            , onClick ClickedCalendar
                                            ]
                                            []
                                        ]
                                    , model.form.deadlineError
                                        |> Maybe.map
                                            (t
                                                >> List.singleton
                                                >> viewFieldErrors
                                            )
                                        |> Maybe.withDefault (text "")
                                    ]
                                ]

                        Nothing ->
                            text ""
            , Checkbox.init
                { description =
                    p []
                        [ b [] [ text_ "community.actions.form.quantity_validity" ]
                        , text_ "community.actions.form.quantity_validity_details"
                        ]
                , id = "quantity_checkbox"
                , value = hasUnitValidation model.form.validation
                , disabled = False
                , onCheck = ToggleUsages
                }
                |> Checkbox.withContainerAttrs [ class "flex" ]
                |> Checkbox.toHtml
            ]
        , case model.form.validation of
            NoValidation ->
                text ""

            Validations _ usagesValidation ->
                case usagesValidation of
                    Just validation ->
                        div []
                            [ Input.init
                                { label = t "community.actions.form.quantity_label"
                                , id = "quantity_input"
                                , onInput = EnteredUsages
                                , disabled = False
                                , value = getInput validation
                                , placeholder = Just (t "community.actions.form.usages_placeholder")
                                , problems = Just (listErrors shared.translations validation)
                                , translators = shared.translators
                                }
                                |> Input.withType Input.Number
                                |> Input.asNumeric
                                |> Input.withAttrs [ Html.Attributes.min "0" ]
                                |> Input.withContainerAttrs [ class "sm:w-2/5" ]
                                |> Input.toHtml
                            , case model.form.usagesLeft of
                                Just usagesLeftValidation ->
                                    Input.init
                                        { label = t "community.actions.form.usages_left_label"
                                        , id = "usages_left_input"
                                        , onInput = EnteredUsagesLeft
                                        , disabled = False
                                        , value = getInput usagesLeftValidation
                                        , placeholder = Nothing
                                        , problems = Just (listErrors shared.translations usagesLeftValidation)
                                        , translators = shared.translators
                                        }
                                        |> Input.withType Input.Number
                                        |> Input.asNumeric
                                        |> Input.withAttrs [ Html.Attributes.min "0" ]
                                        |> Input.withContainerAttrs [ class "sm:w-2/5" ]
                                        |> Input.toHtml

                                Nothing ->
                                    text ""
                            ]

                    Nothing ->
                        text ""
        ]


viewVerifications : LoggedIn.Model -> Model -> Community.Model -> Html Msg
viewVerifications ({ shared } as loggedIn) model community =
    let
        { t } =
            shared.translators

        text_ s =
            text (t s)

        verifiersValidator =
            defaultVerifiersValidator [] (getInput defaultMinVotes)

        profileSummaries =
            getInput verifiersValidator
                |> List.length
                |> Profile.Summary.initMany False

        automaticActionTooltipContainer children =
            div [ class "flex items-center" ]
                [ div
                    [ class "items-center hidden md:flex"
                    , onMouseEnter OpenedAutomaticActionTooltip
                    , onMouseLeave ClosedAutomaticActionTooltip
                    ]
                    children
                , div
                    [ class "items-center flex md:hidden" ]
                    children
                ]
    in
    div [ class "mb-10" ]
        [ Radio.init
            { label = "community.actions.form.verification_label"
            , name = "verification_radio"
            , optionToString =
                \option ->
                    case option of
                        Manual _ ->
                            "manual"

                        Automatic ->
                            "automatic"
            , activeOption = model.form.verification
            , onSelect = SetVerification
            , areOptionsEqual =
                \firstOption secondOption ->
                    case ( firstOption, secondOption ) of
                        ( Manual _, Manual _ ) ->
                            True

                        ( Automatic, Automatic ) ->
                            True

                        _ ->
                            False
            }
            |> Radio.withOption Automatic
                (\_ ->
                    div [ class "flex space-x-2" ]
                        [ span []
                            [ b [] [ text_ "community.actions.form.automatic" ]
                            , text_ "community.actions.form.automatic_detail"
                            ]
                        , automaticActionTooltipContainer
                            [ button
                                [ class "rounded-full focus:outline-none focus:ring focus:ring-green focus:ring-opacity-50"
                                , Utils.onClickNoBubble OpenedAutomaticActionTooltip
                                ]
                                [ Icons.question "h-5 md:h-4" ]
                            , if model.showAutomaticActionTooltip then
                                View.Components.dialogBubble
                                    { class_ = "bg-black text-white p-2 w-120 animate-fade-in opacity-0 hidden md:block"
                                    , relativeSelector = Nothing
                                    , scrollSelector = Nothing
                                    }
                                    [ text_ "community.actions.form.automatic_tooltip" ]

                              else
                                text ""
                            , div [ class "text-black md:hidden" ]
                                [ Modal.initWith
                                    { closeMsg = ClosedAutomaticActionTooltip
                                    , isVisible = model.showAutomaticActionTooltip
                                    }
                                    |> Modal.withBody [ text_ "community.actions.form.automatic_tooltip" ]
                                    |> Modal.withFooter
                                        [ button
                                            [ class "button button-primary w-full"
                                            , Utils.onClickNoBubble ClosedAutomaticActionTooltip
                                            ]
                                            [ text_ "Entendi" ]
                                        ]
                                    |> Modal.toHtml
                                ]
                            ]
                        ]
                )
            |> Radio.withOption
                (Manual
                    { verifiersValidator = verifiersValidator
                    , verifierRewardValidator = defaultVerificationReward shared.translators
                    , minVotesValidator = defaultMinVotes
                    , photoProof = Disabled
                    , profileSummaries = profileSummaries
                    }
                )
                (\_ ->
                    span []
                        [ b [] [ text_ "community.actions.form.manual" ]
                        , text_ "community.actions.form.manual_detail"
                        ]
                )
            |> Radio.withVertical True
            |> Radio.toHtml shared.translators
        , if model.form.verification /= Automatic then
            viewManualVerificationForm loggedIn model community

          else
            text ""
        ]


viewManualVerificationForm : LoggedIn.Model -> Model -> Community.Model -> Html Msg
viewManualVerificationForm ({ shared } as loggedIn) model community =
    let
        { t, tr } =
            shared.translators
    in
    case model.form.verification of
        Automatic ->
            text ""

        Manual { verifiersValidator, verifierRewardValidator, minVotesValidator, photoProof, profileSummaries } ->
            let
                isPhotoProofEnabled =
                    case photoProof of
                        Enabled _ ->
                            True

                        _ ->
                            False

                isProofNumberEnabled =
                    case photoProof of
                        Enabled WithProofNumber ->
                            True

                        _ ->
                            False

                minVotesOptions =
                    List.map
                        (\option ->
                            ( option
                            , \isActive ->
                                div
                                    [ class "rounded-full border w-8 h-8 flex items-center justify-center cursor-pointer"
                                    , classList
                                        [ ( "bg-orange-300 border-orange-300 text-white", isActive )
                                        , ( "hover:border-orange-300 hover:text-orange-500 border-gray-500", not isActive )
                                        ]
                                    ]
                                    [ text (String.fromInt option) ]
                            )
                        )
                        [ 3, 5, 7, 9 ]
            in
            div [ class "mt-6 ml-8 sm:w-2/5" ]
                [ Radio.init
                    { label = t "community.actions.form.votes_label"
                    , name = "number_of_votes"
                    , optionToString = String.fromInt
                    , activeOption = getInput minVotesValidator
                    , onSelect = EnteredMinVotes
                    , areOptionsEqual = (==)
                    }
                    |> Radio.withRowAttrs [ class "space-x-4" ]
                    |> Radio.withAttrs [ class "mb-6" ]
                    |> Radio.withOptions minVotesOptions
                    |> Radio.withVariant Radio.Simplified
                    |> Radio.toHtml shared.translators
                , span [ class "label" ]
                    [ text (tr "community.actions.form.verifiers_label_count" [ ( "count", getInput minVotesValidator |> String.fromInt ) ]) ]
                , div []
                    [ viewVerifierSelect loggedIn model False
                    , viewFieldErrors (listErrors shared.translations verifiersValidator)
                    , viewSelectedVerifiers loggedIn profileSummaries (getInput verifiersValidator)
                    ]
                , Input.init
                    { label = t "community.actions.form.verifiers_reward_label"
                    , id = "verifiers_reward_field"
                    , onInput = EnteredVerifierReward
                    , disabled = False
                    , value = getInput verifierRewardValidator
                    , placeholder = Just (Eos.formatSymbolAmount shared.translators community.symbol 0)
                    , problems = listErrors shared.translations verifierRewardValidator |> Just
                    , translators = shared.translators
                    }
                    |> Input.withCurrency community.symbol
                    |> Input.toHtml
                , div [ class "mt-8" ]
                    [ Checkbox.init
                        { description =
                            span []
                                [ b [ class "block" ] [ text (t "community.actions.form.proof_validation") ]
                                , text (t "community.actions.form.proof_validation_hint")
                                ]
                        , id = "photo_proof_checkbox"
                        , value = isPhotoProofEnabled
                        , disabled = False
                        , onCheck = TogglePhotoProof
                        }
                        |> Checkbox.withContainerAttrs [ class "flex" ]
                        |> Checkbox.toHtml
                    , if isPhotoProofEnabled then
                        div [ class "mt-6" ]
                            [ Checkbox.init
                                { description =
                                    span []
                                        [ b [ class "block" ] [ text (t "community.actions.form.verification_code") ]
                                        , text (t "community.actions.form.verification_code_hint")
                                        ]
                                , id = "verification_code_checkbox"
                                , value = isProofNumberEnabled
                                , disabled = False
                                , onCheck = TogglePhotoProofNumber
                                }
                                |> Checkbox.withContainerAttrs [ class "flex" ]
                                |> Checkbox.toHtml
                            , MarkdownEditor.view
                                { translators = shared.translators
                                , placeholder = Nothing
                                , label = t "community.actions.form.verification_instructions"
                                , problem =
                                    model.form.instructionsError
                                        |> Maybe.map
                                            (\( key, replacements ) ->
                                                tr key replacements
                                            )
                                , disabled = False
                                }
                                [ class "mt-6" ]
                                model.form.instructions
                                |> Html.map GotInstructionsEditorMsg
                            ]

                      else
                        text ""
                    ]
                ]


viewSelectedVerifiers : LoggedIn.Model -> List Profile.Summary.Model -> List Profile.Minimal -> Html Msg
viewSelectedVerifiers ({ shared } as loggedIn) profileSummaries selectedVerifiers =
    div [ class "flex flex-row mt-3 mb-6 flex-wrap" ]
        (List.map3
            (\profileSummary index verifier ->
                div
                    [ class "flex justify-between flex-col m-3 items-center" ]
                    [ Profile.Summary.view shared loggedIn.accountName verifier profileSummary
                        |> Html.map (GotProfileSummaryMsg index)
                    , div
                        [ onClick (OnRemoveVerifier verifier)
                        , class "h-6 w-6 flex items-center mt-4"
                        ]
                        [ Icons.trash "" ]
                    ]
            )
            profileSummaries
            (List.range 0 (List.length selectedVerifiers))
            selectedVerifiers
        )


viewFieldErrors : List String -> Html msg
viewFieldErrors errors =
    ul []
        (List.map
            (\e ->
                li [ class "form-error" ] [ text e ]
            )
            errors
        )



-- Configure Select


filter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
filter minChars toLabel query items =
    if String.length query < minChars then
        Nothing

    else
        items
            |> Simple.Fuzzy.filter toLabel query
            |> Just


selectConfiguration : Shared -> Bool -> Select.Config Msg Profile.Minimal
selectConfiguration shared isDisabled =
    Profile.selectConfig
        (Select.newConfig
            { onSelect = OnSelectVerifier
            , toLabel = \p -> Eos.nameToString p.account
            , filter = filter 2 (\p -> Eos.nameToString p.account)
            , onFocusItem = NoOp
            }
            |> Select.withMultiSelection True
        )
        shared
        isDisabled


viewVerifierSelect : LoggedIn.Model -> Model -> Bool -> Html Msg
viewVerifierSelect loggedIn model isDisabled =
    let
        users =
            case ( loggedIn.selectedCommunity, model.status ) of
                ( RemoteData.Success community, Authorized ) ->
                    community.members

                _ ->
                    []
    in
    case model.form.verification of
        Automatic ->
            text ""

        Manual { verifiersValidator } ->
            div []
                [ Html.map SelectMsg
                    (Select.view (selectConfiguration loggedIn.shared isDisabled)
                        model.multiSelectState
                        users
                        (getInput verifiersValidator)
                    )
                ]



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
    case addr of
        "SaveAction" :: _ ->
            Json.decodeValue
                (Json.oneOf
                    [ Json.field "transactionId" Json.string
                        |> Json.map Ok
                    , Json.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotSaveAction)
                |> Result.withDefault Nothing

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

        OnSelectVerifier _ ->
            [ "OnSelectVerifier" ]

        OnRemoveVerifier _ ->
            [ "OnRemoveVerifier" ]

        EnteredReward _ ->
            [ "EnteredReward" ]

        EnteredMinVotes _ ->
            [ "EnteredMinVotes" ]

        EnteredUsages _ ->
            [ "EnteredUsages" ]

        EnteredUsagesLeft _ ->
            [ "EnteredUsagesLeft" ]

        ClickedCalendar ->
            [ "ClickedCalendar" ]

        GotDatePickerMsg _ ->
            [ "GotDatePickerMsg" ]

        SelectMsg _ ->
            [ "SelectMsg" ]

        ToggleValidity ->
            [ "ToggleValidity" ]

        ToggleDeadline _ ->
            [ "ToggleDeadline" ]

        ToggleUsages _ ->
            [ "ToggleUsages" ]

        TogglePhotoProof _ ->
            [ "TogglePhotoProof" ]

        TogglePhotoProofNumber _ ->
            [ "TogglePhotoProofNumber" ]

        EnteredVerifierReward _ ->
            [ "EnteredVerifierReward" ]

        SetVerification _ ->
            [ "SetVerification" ]

        MarkAsCompleted ->
            [ "MarkAsCompleted" ]

        ValidateForm ->
            [ "ValidateForm" ]

        SaveAction _ ->
            [ "SaveAction" ]

        GotSaveAction _ ->
            [ "GotSaveAction" ]

        GotProfileSummaryMsg _ subMsg ->
            "GotProfileSummaryMsg" :: Profile.Summary.msgToString subMsg

        GotDescriptionEditorMsg subMsg ->
            "GotDescriptionEditorMsg" :: MarkdownEditor.msgToString subMsg

        GotInstructionsEditorMsg subMsg ->
            "GotInstructionsEditorMsg" :: MarkdownEditor.msgToString subMsg

        OpenedAutomaticActionTooltip ->
            [ "OpenedAutomaticActionTooltip" ]

        ClosedAutomaticActionTooltip ->
            [ "ClosedAutomaticActionTooltip" ]
