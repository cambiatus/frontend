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
import Cambiatus.Enum.VerificationType as VerificationType
import Cambiatus.Scalar exposing (DateTime(..))
import Community
import DataValidator
    exposing
        ( Validator
        , addConstraints
        , getInput
        , greaterThan
        , greaterThanOrEqual
        , hasErrors
        , isOdd
        , lengthGreaterThanOrEqual
        , listErrors
        , longerThan
        , newValidator
        , shorterThan
        , updateInput
        , validate
        )
import Eos
import Eos.Account as Eos
import Html exposing (Html, b, button, div, label, p, span, text, textarea)
import Html.Attributes exposing (class, classList, placeholder, rows, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Decode as Json exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import MaskedInput.Text as MaskedDate
import Page
import Profile
import Profile.Summary
import RemoteData
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import Strftime
import Time
import UpdateResult as UR
import Utils
import View.Feedback as Feedback
import View.Form
import View.Form.Checkbox as Checkbox
import View.Form.Input as Input
import View.Form.InputCounter
import View.Form.Radio as Radio
import View.Form.Toggle as Toggle



-- INIT


type alias ObjectiveId =
    Int


type alias ActionId =
    Int


init : LoggedIn.Model -> ObjectiveId -> Maybe ActionId -> ( Model, Cmd Msg )
init loggedIn objectiveId actionId =
    ( { status = NotFound
      , objectiveId = objectiveId
      , actionId = actionId
      , form = initForm
      , multiSelectState = Select.newState ""
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- MODEL


type alias Model =
    { status : Status
    , objectiveId : ObjectiveId
    , actionId : Maybe ActionId
    , form : Form
    , multiSelectState : Select.State
    }


type Status
    = Authorized
    | NotFound
    | Unauthorized


type ActionValidation
    = NoValidation
    | Validations (Maybe (Validator String)) (Maybe (Validator String)) -- Date validation, usage validate


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
    { description : Validator String
    , reward : Validator String
    , validation : ActionValidation
    , verification : Verification
    , usagesLeft : Maybe (Validator String) -- Only available on edit
    , isCompleted : Bool
    , deadlineState : MaskedDate.State
    , saveStatus : SaveStatus
    , instructions : Validator String
    }


initForm : Form
initForm =
    { description = defaultDescription
    , reward = defaultReward
    , validation = NoValidation
    , verification = Automatic
    , usagesLeft = Nothing
    , isCompleted = False
    , deadlineState = MaskedDate.initialState
    , saveStatus = NotAsked
    , instructions = defaultInstructions
    }


editForm : Form -> Action -> Form
editForm form action =
    let
        dateValidator : Maybe (Validator String)
        dateValidator =
            action.deadline
                |> Maybe.andThen
                    (\d ->
                        defaultDateValidator
                            |> updateInput
                                (Just d |> Utils.posixDateTime |> Strftime.format "%m%d%Y" Time.utc)
                            |> Just
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
                Validations dateValidator usagesValidator

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
                        defaultVerificationReward
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

        instructions =
            case action.photoProofInstructions of
                Just i ->
                    updateInput i form.instructions

                Nothing ->
                    form.instructions
    in
    { form
        | description = updateInput action.description form.description
        , reward = updateInput (String.fromFloat action.reward) form.reward
        , validation = validation
        , verification = verification
        , usagesLeft = Just (updateInput (String.fromInt action.usagesLeft) defaultUsagesLeftValidator)
        , isCompleted = action.isCompleted
        , instructions = instructions
    }


defaultDescription : Validator String
defaultDescription =
    []
        |> longerThan 10
        |> shorterThan 256
        |> newValidator "" (\v -> Just v) True


defaultInstructions : Validator String
defaultInstructions =
    []
        |> longerThan 10
        |> shorterThan 256
        |> newValidator "" (\v -> Just v) True


defaultReward : Validator String
defaultReward =
    []
        |> greaterThanOrEqual 1.0
        |> newValidator "" (\s -> Just s) True


defaultDateValidator : Validator String
defaultDateValidator =
    newValidator "" (\s -> Just s) True []


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


defaultVerificationReward : Validator String
defaultVerificationReward =
    []
        |> greaterThanOrEqual 0
        |> newValidator "0" (\s -> Just s) True


minVotesLimit : Int
minVotesLimit =
    3


defaultMinVotes : Validator Int
defaultMinVotes =
    []
        |> greaterThanOrEqual (toFloat minVotesLimit)
        |> isOdd
        |> newValidator minVotesLimit (\s -> Just (String.fromInt s)) True


validateForm : Form -> Form
validateForm form =
    let
        validation =
            case form.validation of
                NoValidation ->
                    NoValidation

                Validations (Just dateValidation) (Just usageValidation) ->
                    Validations (Just (validate dateValidation)) (Just (validate usageValidation))

                Validations (Just dateValidation) Nothing ->
                    Validations (Just (validate dateValidation)) Nothing

                Validations Nothing (Just usageValidation) ->
                    Validations Nothing (Just (validate usageValidation))

                Validations Nothing Nothing ->
                    NoValidation

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
    in
    { form
        | description = validate form.description
        , reward = validate form.reward
        , validation = validation
        , verification = verification
        , instructions = validate form.instructions
    }


isFormValid : Form -> Bool
isFormValid form =
    let
        verificationHasErrors =
            case form.verification of
                Manual m ->
                    hasErrors m.minVotesValidator
                        || hasErrors m.verifiersValidator
                        || hasErrors m.verifierRewardValidator

                Automatic ->
                    -- Automatic verification never has validation errors
                    False
    in
    hasErrors form.description
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


getDateValidation : ActionValidation -> Maybe (Validator String)
getDateValidation validation =
    case validation of
        NoValidation ->
            Nothing

        Validations maybeDate _ ->
            maybeDate


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
    = CompletedLoadCommunity Community.Model
    | OnSelectVerifier (Maybe Profile.Minimal)
    | OnRemoveVerifier Profile.Minimal
    | SelectMsg (Select.Msg Profile.Minimal)
    | EnteredDescription String
    | EnteredInstructions String
    | EnteredReward String
    | EnteredDeadline String
    | DeadlineChanged MaskedDate.State
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
    | ValidateDeadline
    | GotValidDate (Result Value String)
    | GotInvalidDate
    | SaveAction Int -- Send the date
    | GotSaveAction (Result Value String)
    | GotProfileSummaryMsg Int Profile.Summary.Msg



---- ACTION CREATE


type alias CreateActionAction =
    { actionId : ActionId
    , objectiveId : ObjectiveId
    , description : String
    , reward : String
    , verifierReward : String
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
        , ( "reward", Encode.string c.reward )
        , ( "verifier_reward", Encode.string c.verifierReward )
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
        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                -- Check the action belongs to the objective
                let
                    maybeObjective =
                        List.filterMap
                            (\o ->
                                if o.id == model.objectiveId then
                                    Just o

                                else
                                    Nothing
                            )
                            community.objectives
                            |> List.head
                in
                case ( maybeObjective, model.actionId ) of
                    ( Just objective, Just actionId ) ->
                        -- Edit form
                        let
                            maybeAction =
                                List.filterMap
                                    (\a ->
                                        if a.id == actionId then
                                            Just a

                                        else
                                            Nothing
                                    )
                                    objective.actions
                                    |> List.head
                        in
                        case maybeAction of
                            Just action ->
                                { model
                                    | status = Authorized
                                    , form = editForm model.form action
                                }
                                    |> UR.init

                            Nothing ->
                                { model | status = NotFound }
                                    |> UR.init

                    ( Just _, Nothing ) ->
                        -- New form
                        { model
                            | status = Authorized
                            , form = initForm
                        }
                            |> UR.init

                    ( Nothing, _ ) ->
                        { model | status = NotFound }
                            |> UR.init

            else
                { model | status = Unauthorized }
                    |> UR.init

        OnSelectVerifier maybeProfile ->
            let
                verification =
                    case model.form.verification of
                        Automatic ->
                            model.form.verification

                        Manual m ->
                            let
                                newVerifiers : List Profile.Minimal
                                newVerifiers =
                                    maybeProfile
                                        |> Maybe.map (List.singleton >> List.append (getInput m.verifiersValidator))
                                        |> Maybe.withDefault (getInput m.verifiersValidator)
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

        EnteredDescription val ->
            let
                limitedDescription =
                    if String.length val < 255 then
                        val

                    else
                        String.slice 0 255 val
            in
            { model
                | form =
                    { oldForm
                        | description = updateInput limitedDescription model.form.description
                    }
            }
                |> UR.init

        EnteredInstructions val ->
            let
                limitedInstructions =
                    if String.length val < 255 then
                        val

                    else
                        String.slice 0 255 val
            in
            { model
                | form =
                    { oldForm
                        | instructions = updateInput limitedInstructions model.form.instructions
                    }
            }
                |> UR.init

        EnteredReward val ->
            { model | form = { oldForm | reward = updateInput val model.form.reward } }
                |> UR.init

        EnteredDeadline val ->
            case model.form.validation of
                NoValidation ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

                Validations maybeDate usageValidation ->
                    case maybeDate of
                        Just dateValidation ->
                            { model
                                | form =
                                    { oldForm
                                        | validation = Validations (Just (updateInput val dateValidation)) usageValidation
                                    }
                            }
                                |> UR.init

                        Nothing ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg []

        EnteredUsages val ->
            case model.form.validation of
                NoValidation ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

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
                                |> UR.logImpossible msg []

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
                    { model | form = validateForm model.form }
            in
            if isFormValid newModel.form then
                case getDateValidation newModel.form.validation of
                    Just _ ->
                        update ValidateDeadline model loggedIn

                    Nothing ->
                        update (SaveAction 0) model loggedIn

            else
                newModel
                    |> UR.init

        ValidateDeadline ->
            case model.form.validation of
                NoValidation ->
                    model
                        |> UR.init

                Validations maybeDate _ ->
                    case maybeDate of
                        Just dateValidation ->
                            model
                                |> UR.init
                                |> UR.addPort
                                    { responseAddress = ValidateDeadline
                                    , responseData = Encode.null
                                    , data =
                                        Encode.object
                                            [ ( "name", Encode.string "validateDeadline" )
                                            , ( "deadline"
                                              , Encode.string
                                                    (String.join "/"
                                                        [ String.slice 0 2 (getInput dateValidation) -- month
                                                        , String.slice 2 4 (getInput dateValidation) -- day
                                                        , String.slice 4 8 (getInput dateValidation) -- year
                                                        ]
                                                    )
                                              )
                                            ]
                                    }

                        Nothing ->
                            model
                                |> UR.init

        DeadlineChanged state ->
            case model.form.validation of
                NoValidation ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

                Validations maybeDate _ ->
                    case maybeDate of
                        Just _ ->
                            { model
                                | form =
                                    { oldForm
                                        | deadlineState = state
                                    }
                            }
                                |> UR.init

                        Nothing ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg []

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
                        Just defaultDateValidator

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

        GotInvalidDate ->
            let
                newValidation =
                    case model.form.validation of
                        NoValidation ->
                            NoValidation

                        Validations (Just dateValidation) usageValidation ->
                            Validations
                                (Just
                                    (addConstraints
                                        [ { test = \_ -> False
                                          , defaultError = \_ -> t "error.validator.date.invalid"
                                          }
                                        ]
                                        (updateInput (getInput dateValidation) defaultDateValidator)
                                    )
                                )
                                usageValidation

                        Validations dateValidation usageValidation ->
                            Validations dateValidation usageValidation

                newForm =
                    { oldForm | validation = newValidation }
            in
            { model | form = validateForm newForm }
                |> UR.init

        GotValidDate isoDate ->
            case isoDate of
                Ok date ->
                    let
                        dateInt =
                            if String.length date == 0 then
                                0

                            else
                                Just (DateTime date)
                                    |> Utils.posixDateTime
                                    |> Time.posixToMillis
                    in
                    update (SaveAction dateInt) model loggedIn

                Err _ ->
                    update GotInvalidDate model loggedIn

        SaveAction isoDate ->
            let
                newModel =
                    { model | form = { oldForm | saveStatus = Saving } }
            in
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    if LoggedIn.hasPrivateKey loggedIn then
                        upsertAction loggedIn community newModel isoDate

                    else
                        newModel
                            |> UR.init
                            |> UR.addExt
                                (Just (SaveAction isoDate)
                                    |> RequiredAuthentication
                                )

                _ ->
                    UR.init newModel

        GotSaveAction (Ok _) ->
            model
                |> UR.init
                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey Route.Objectives)
                |> UR.addExt (ShowFeedback Feedback.Success (t "community.actions.save_success"))
                -- TODO - This only works sometimes
                |> UR.addExt (LoggedIn.ReloadResource LoggedIn.CommunityResource)

        GotSaveAction (Err val) ->
            let
                newModel =
                    { model | form = { oldForm | saveStatus = Failed } }
            in
            newModel
                |> UR.init
                |> UR.logDebugValue msg val
                |> UR.logImpossible msg []
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


upsertAction : LoggedIn.Model -> Community.Model -> Model -> Int -> UpdateResult
upsertAction loggedIn community model isoDate =
    let
        verifierReward =
            case model.form.verification of
                Automatic ->
                    Eos.Asset 0.0 community.symbol |> Eos.assetToString

                Manual { verifierRewardValidator } ->
                    Eos.Asset (getInput verifierRewardValidator |> String.toFloat |> Maybe.withDefault 0.0) community.symbol
                        |> Eos.assetToString

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
                getInput model.form.instructions

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
                            , description = getInput model.form.description
                            , reward = Eos.Asset (getInput model.form.reward |> String.toFloat |> Maybe.withDefault 0.0) community.symbol |> Eos.assetToString
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
        { t } =
            shared.translators

        text_ s =
            text (t s)
    in
    div [ class "mb-10" ]
        [ span [ class "input-label" ]
            [ text_ "community.actions.form.description_label" ]
        , textarea
            [ class "input textarea-input w-full"
            , classList [ ( "border-red", hasErrors form.description ) ]
            , rows 5
            , onInput EnteredDescription
            , value (getInput form.description)
            ]
            []
        , View.Form.InputCounter.view shared.translators.tr 256 (getInput form.description)
        , viewFieldErrors (listErrors shared.translations form.description)
        ]


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
        , placeholder = Just (Eos.formatSymbolAmount community.symbol 0)
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

        dateOptions =
            MaskedDate.defaultOptions EnteredDeadline DeadlineChanged
    in
    div []
        [ div [ class "mb-6" ]
            [ View.Form.label "expiration-toggle" (t "community.actions.form.validity_label")
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
                |> Checkbox.withContainerAttrs [ class "flex text-body mb-3" ]
                |> Checkbox.toHtml
            , case model.form.validation of
                NoValidation ->
                    text ""

                Validations dateValidation _ ->
                    case dateValidation of
                        Just validation ->
                            div []
                                [ span [ class "input-label" ]
                                    [ text_ "community.actions.form.date_label" ]
                                , div [ class "mb-10" ]
                                    [ MaskedDate.input
                                        { dateOptions
                                            | pattern = "##/##/####"
                                            , inputCharacter = '#'
                                        }
                                        [ class "input w-full"
                                        , classList [ ( "border-red", hasErrors validation ) ]
                                        , placeholder "mm/dd/yyyy"
                                        ]
                                        model.form.deadlineState
                                        (getInput validation)
                                    , viewFieldErrors (listErrors shared.translations validation)
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
                |> Checkbox.withContainerAttrs [ class "flex text-body" ]
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
                    span []
                        [ b [] [ text_ "community.actions.form.automatic" ]
                        , text_ "community.actions.form.automatic_detail"
                        ]
                )
            |> Radio.withOption
                (Manual
                    { verifiersValidator = verifiersValidator
                    , verifierRewardValidator = defaultVerificationReward
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
                , span [ class "input-label" ]
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
                    , placeholder = Just (Eos.formatSymbolAmount community.symbol 0)
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
                        |> Checkbox.withContainerAttrs [ class "flex text-body" ]
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
                                |> Checkbox.withContainerAttrs [ class "flex text-body" ]
                                |> Checkbox.toHtml
                            , div [ class "mt-6" ]
                                [ label [ class "input-label" ]
                                    [ text (t "community.actions.form.verification_instructions") ]
                                , textarea
                                    [ class "input textarea-input w-full"
                                    , classList [ ( "border-red", hasErrors model.form.instructions ) ]
                                    , rows 5
                                    , onInput EnteredInstructions
                                    , value (getInput model.form.instructions)
                                    ]
                                    []
                                , View.Form.InputCounter.view shared.translators.tr 256 (getInput model.form.instructions)
                                , viewFieldErrors (listErrors shared.translations model.form.instructions)
                                ]
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
    div []
        (List.map
            (\e ->
                span [ class "form-error" ] [ text e ]
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
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ValidateDeadline" :: _ ->
            Json.decodeValue
                (Json.oneOf
                    [ Json.field "date" Json.string
                        |> Json.map Ok
                    , Json.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotValidDate)
                |> Result.withDefault (Just GotInvalidDate)

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
        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        OnSelectVerifier _ ->
            [ "OnSelectVerifier" ]

        OnRemoveVerifier _ ->
            [ "OnRemoveVerifier" ]

        EnteredDescription _ ->
            [ "EnteredDescription" ]

        EnteredInstructions _ ->
            [ "EnteredInstructions" ]

        EnteredReward _ ->
            [ "EnteredReward" ]

        EnteredDeadline _ ->
            [ "EnteredDeadline" ]

        EnteredMinVotes _ ->
            [ "EnteredMinVotes" ]

        EnteredUsages _ ->
            [ "EnteredUsages" ]

        EnteredUsagesLeft _ ->
            [ "EnteredUsagesLeft" ]

        DeadlineChanged _ ->
            [ "DeadlineChanged" ]

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

        ValidateDeadline ->
            [ "ValidateDeadline" ]

        SaveAction _ ->
            [ "SaveAction" ]

        GotValidDate _ ->
            [ "GotValidDate" ]

        GotInvalidDate ->
            [ "GotInvalidDate" ]

        GotSaveAction _ ->
            [ "GotSaveAction" ]

        GotProfileSummaryMsg _ subMsg ->
            "GotProfileSummaryMsg" :: Profile.Summary.msgToString subMsg
