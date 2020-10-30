module Page.Community.ActionEditor exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , update
    , view
    )

import Api.Graphql
import Cambiatus.Enum.VerificationType as VerificationType
import Cambiatus.Scalar exposing (DateTime(..))
import Community exposing (Model)
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
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, b, button, div, input, label, p, span, text, textarea)
import Html.Attributes exposing (checked, class, classList, for, id, name, placeholder, rows, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Icons
import Json.Decode as Json exposing (Value)
import Json.Encode as Encode
import MaskedInput.Text as MaskedDate
import Page
import Profile exposing (Profile)
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import Strftime
import Task
import Time
import UpdateResult as UR
import Utils
import View.Form.InputCounter



-- INIT


type alias ObjectiveId =
    Int


type alias ActionId =
    Int


init : LoggedIn.Model -> Symbol -> ObjectiveId -> Maybe ActionId -> ( Model, Cmd Msg )
init loggedIn symbol objectiveId actionId =
    ( { status = Loading
      , communityId = symbol
      , objectiveId = objectiveId
      , actionId = actionId
      , form = initForm
      , multiSelectState = Select.newState ""
      }
    , Api.Graphql.query loggedIn.shared
        (Community.communityQuery symbol)
        CompletedCommunityLoad
    )



-- MODEL


type alias Model =
    { status : Status
    , communityId : Symbol
    , objectiveId : ObjectiveId
    , actionId : Maybe ActionId
    , form : Form
    , multiSelectState : Select.State
    }


type Status
    = Loading
    | Loaded Community.Model
      -- Errors
    | LoadFailed (Graphql.Http.Error (Maybe Community.Model))
    | NotFound
    | Unauthorized


type ActionValidation
    = NoValidation
    | Validations (Maybe (Validator String)) (Maybe (Validator String)) -- Date validation, usage validate


type Verification
    = Automatic
      -- users list, verification reward, min votes, photo proof
    | Manual
        { verifiers : Validator (List Profile)
        , verificationReward : Validator String
        , minVotes : Validator String
        , photoProof : Maybe PhotoProof
        }


type PhotoProof
    = Enabled ProofNumberToggle
    | Disabled


type ProofNumberToggle
    = WithProofNumber
    | WithoutProofNumber


type SaveStatus
    = NotAsked
    | Saving
    | Failed String


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


editForm : Form -> Community.Action -> Form
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

        verificators : List Profile
        verificators =
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
                    newVerifications =
                        defaultMinVotes |> updateInput (String.fromInt action.verifications)

                    photoProof =
                        case ( action.hasProofPhoto, action.hasProofCode ) of
                            ( Just True, Just True ) ->
                                Just (Enabled WithProofNumber)

                            ( Just True, _ ) ->
                                Just (Enabled WithoutProofNumber)

                            ( Just False, _ ) ->
                                Just Disabled

                            _ ->
                                Nothing
                in
                Manual
                    { verifiers = defaultVerifiersValidator verificators (getInput newVerifications) |> updateInput verificators
                    , verificationReward = defaultVerificationReward |> updateInput (String.fromFloat action.verificationReward)
                    , minVotes = newVerifications
                    , photoProof = photoProof
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


defaultVerifiersValidator : List Profile -> String -> Validator (List Profile)
defaultVerifiersValidator verifiers minVerifiersQty =
    let
        limit =
            case String.toInt minVerifiersQty of
                Just m ->
                    if m < minVotesLimit then
                        minVotesLimit

                    else
                        m

                Nothing ->
                    minVotesLimit
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


defaultMinVotes : Validator String
defaultMinVotes =
    []
        |> greaterThanOrEqual (toFloat minVotesLimit)
        |> isOdd
        |> newValidator (String.fromInt minVotesLimit) (\s -> Just s) True


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

                Manual { verifiers, verificationReward, minVotes, photoProof } ->
                    Manual
                        { verifiers = validate verifiers
                        , verificationReward = validate verificationReward
                        , minVotes = validate minVotes
                        , photoProof = photoProof
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
                Manual { verifiers, verificationReward, minVotes } ->
                    hasErrors minVotes
                        || hasErrors verifiers
                        || hasErrors verificationReward

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
    = CompletedCommunityLoad (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | OnSelectVerifier (Maybe Profile)
    | OnRemoveVerifier Profile
    | SelectMsg (Select.Msg Profile)
    | EnteredDescription String
    | EnteredInstructions String
    | EnteredReward String
    | EnteredDeadline String
    | DeadlineChanged MaskedDate.State
    | EnteredUsages String
    | EnteredUsagesLeft String
    | EnteredVerifierReward String
    | EnteredMinVotes String
    | ToggleValidity Bool
    | ToggleDeadline Bool
    | TogglePhotoProof Bool
    | TogglePhotoProofNumber Bool
    | ToggleUsages Bool
    | MarkAsCompleted
    | SetVerification String
    | ValidateForm
    | ValidateDeadline
    | GotValidDate (Result Value String)
    | GotInvalidDate
    | SaveAction Int -- Send the date
    | GotSaveAction (Result Value String)
    | PressedEnter Bool



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
    , verifications : String
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
        , ( "verifications", Encode.string c.verifications )
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
    in
    case msg of
        CompletedCommunityLoad (Err err) ->
            { model | status = LoadFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedCommunityLoad (Ok c) ->
            case c of
                Just community ->
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
                        case maybeObjective of
                            Just objective ->
                                case model.actionId of
                                    Just actionId ->
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
                                                    | status = Loaded community
                                                    , form = editForm model.form action
                                                }
                                                    |> UR.init

                                            Nothing ->
                                                { model | status = NotFound }
                                                    |> UR.init

                                    Nothing ->
                                        -- New form
                                        { model
                                            | status = Loaded community
                                            , form = initForm
                                        }
                                            |> UR.init

                            Nothing ->
                                { model | status = NotFound }
                                    |> UR.init

                    else
                        { model | status = Unauthorized }
                            |> UR.init

                Nothing ->
                    { model | status = NotFound }
                        |> UR.init
                        |> UR.logImpossible msg []

        OnSelectVerifier maybeProfile ->
            let
                oldForm =
                    model.form
            in
            case model.form.verification of
                Automatic ->
                    model
                        |> UR.init

                Manual { verifiers, verificationReward, minVotes, photoProof } ->
                    { model
                        | form =
                            { oldForm
                                | verification =
                                    let
                                        newVerifiers =
                                            maybeProfile
                                                |> Maybe.map (List.singleton >> List.append (getInput verifiers))
                                                |> Maybe.withDefault (getInput verifiers)
                                    in
                                    Manual
                                        { verifiers = updateInput newVerifiers verifiers
                                        , verificationReward = verificationReward
                                        , minVotes = minVotes
                                        , photoProof = photoProof
                                        }
                            }
                    }
                        |> UR.init

        OnRemoveVerifier profile ->
            let
                oldForm =
                    model.form

                verification =
                    case model.form.verification of
                        Automatic ->
                            model.form.verification

                        Manual { verifiers, verificationReward, minVotes, photoProof } ->
                            let
                                newVerifiers =
                                    List.filter
                                        (\currVerifier -> currVerifier.account /= profile.account)
                                        (getInput verifiers)
                            in
                            Manual
                                { verifiers = updateInput newVerifiers verifiers
                                , verificationReward = verificationReward
                                , minVotes = minVotes
                                , photoProof = photoProof
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
                oldForm =
                    model.form

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
                oldForm =
                    model.form

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
            let
                oldForm =
                    model.form
            in
            { model | form = { oldForm | reward = updateInput val model.form.reward } }
                |> UR.init

        EnteredDeadline val ->
            let
                oldForm =
                    model.form
            in
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
            let
                oldForm =
                    model.form
            in
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
            let
                oldForm =
                    model.form
            in
            case model.form.usagesLeft of
                Just validator ->
                    { model | form = { oldForm | usagesLeft = Just (updateInput val validator) } }
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init

        EnteredVerifierReward val ->
            let
                oldForm =
                    model.form
            in
            case model.form.verification of
                Automatic ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

                Manual { verifiers, verificationReward, minVotes, photoProof } ->
                    { model
                        | form =
                            { oldForm
                                | verification =
                                    Manual
                                        { verifiers = verifiers
                                        , verificationReward = updateInput val verificationReward
                                        , minVotes = minVotes
                                        , photoProof = photoProof
                                        }
                            }
                    }
                        |> UR.init

        EnteredMinVotes val ->
            let
                oldForm =
                    model.form
            in
            case model.form.verification of
                Automatic ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

                Manual { verifiers, verificationReward, minVotes, photoProof } ->
                    let
                        newMinVotes =
                            updateInput val minVotes

                        newVerifiers =
                            -- Update min. verifiers quantity
                            defaultVerifiersValidator (getInput verifiers) val
                    in
                    { model
                        | form =
                            { oldForm
                                | verification =
                                    Manual
                                        { verifiers = newVerifiers
                                        , verificationReward = verificationReward
                                        , minVotes = newMinVotes
                                        , photoProof = photoProof
                                        }
                            }
                    }
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
            let
                oldForm =
                    model.form
            in
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

        ToggleValidity _ ->
            model
                |> UR.init

        TogglePhotoProof isPhotoProofEnabled ->
            let
                oldForm =
                    model.form

                newPhotoProofState =
                    if isPhotoProofEnabled then
                        Just (Enabled WithoutProofNumber)

                    else
                        Nothing
            in
            case model.form.verification of
                Automatic ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

                Manual { verifiers, verificationReward, minVotes } ->
                    { model
                        | form =
                            { oldForm
                                | verification =
                                    Manual
                                        { verifiers = verifiers
                                        , verificationReward = verificationReward
                                        , minVotes = minVotes
                                        , photoProof = newPhotoProofState
                                        }
                            }
                    }
                        |> UR.init

        TogglePhotoProofNumber isProofNumberEnabled ->
            let
                oldForm =
                    model.form
            in
            case model.form.verification of
                Automatic ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

                Manual { verifiers, verificationReward, minVotes } ->
                    let
                        newPhotoValidationState =
                            if isProofNumberEnabled then
                                Just (Enabled WithProofNumber)

                            else
                                Just (Enabled WithoutProofNumber)
                    in
                    { model
                        | form =
                            { oldForm
                                | verification =
                                    Manual
                                        { verifiers = verifiers
                                        , verificationReward = verificationReward
                                        , minVotes = minVotes
                                        , photoProof = newPhotoValidationState
                                        }
                            }
                    }
                        |> UR.init

        ToggleDeadline bool ->
            let
                oldForm =
                    model.form

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
                oldForm =
                    model.form

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

        SetVerification val ->
            let
                oldForm =
                    model.form
            in
            { model
                | form =
                    { oldForm
                        | verification =
                            if val == "automatic" then
                                Automatic

                            else
                                Manual
                                    { verifiers = defaultVerifiersValidator [] (getInput defaultMinVotes)
                                    , verificationReward = defaultVerificationReward
                                    , minVotes = defaultMinVotes
                                    , photoProof = Nothing
                                    }
                    }
            }
                |> UR.init

        MarkAsCompleted ->
            let
                oldForm =
                    model.form

                newModel =
                    { model | form = { oldForm | isCompleted = True } }
            in
            update ValidateForm newModel loggedIn

        GotInvalidDate ->
            let
                oldForm =
                    model.form

                newForm =
                    { oldForm
                        | validation =
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
                    }
            in
            { model
                | form = validateForm newForm
            }
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
                oldForm =
                    model.form

                newModel =
                    { model | form = { oldForm | saveStatus = Saving } }
            in
            if LoggedIn.isAuth loggedIn then
                upsertAction loggedIn newModel isoDate

            else
                newModel
                    |> UR.init
                    |> UR.addExt
                        (Just (SaveAction isoDate)
                            |> RequiredAuthentication
                        )

        GotSaveAction (Ok _) ->
            model
                |> UR.init
                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey (Route.Objectives model.communityId))
                |> UR.addExt (ShowFeedback Success (t "community.actions.create_success"))

        GotSaveAction (Err val) ->
            let
                oldForm =
                    model.form

                newModel =
                    { model | form = { oldForm | saveStatus = Failed (t "error.unknown") } }
            in
            newModel
                |> UR.init
                |> UR.logDebugValue msg val
                |> UR.logImpossible msg []
                |> UR.addExt (ShowFeedback Failure (t "error.unknown"))

        PressedEnter val ->
            if val then
                UR.init model
                    |> UR.addCmd
                        (Task.succeed ValidateDeadline
                            |> Task.perform identity
                        )

            else
                UR.init model


upsertAction : LoggedIn.Model -> Model -> Int -> UpdateResult
upsertAction loggedIn model isoDate =
    let
        verifierReward =
            case model.form.verification of
                Automatic ->
                    Eos.Asset 0.0 model.communityId |> Eos.assetToString

                Manual { verificationReward } ->
                    Eos.Asset (getInput verificationReward |> String.toFloat |> Maybe.withDefault 0.0) model.communityId
                        |> Eos.assetToString

        usages =
            case model.form.validation of
                NoValidation ->
                    "0"

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

        minVotes_ =
            case model.form.verification of
                Automatic ->
                    "0"

                Manual { minVotes } ->
                    getInput minVotes

        validators =
            case model.form.verification of
                Automatic ->
                    []

                Manual { verifiers } ->
                    getInput verifiers

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

        hasProofPhoto =
            case model.form.verification of
                Manual { photoProof } ->
                    case photoProof of
                        Just (Enabled _) ->
                            True

                        _ ->
                            False

                _ ->
                    False

        hasProofCode =
            case model.form.verification of
                Manual { photoProof } ->
                    case photoProof of
                        Just (Enabled WithProofNumber) ->
                            True

                        _ ->
                            False

                _ ->
                    False

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
                            , reward = Eos.Asset (getInput model.form.reward |> String.toFloat |> Maybe.withDefault 0.0) model.communityId |> Eos.assetToString
                            , verifierReward = verifierReward
                            , deadline = isoDate
                            , usages = usages
                            , usagesLeft = usagesLeft
                            , verifications = minVotes_
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
            case model.status of
                Loading ->
                    Page.fullPageLoading

                Loaded community ->
                    div [ class "bg-white" ]
                        [ Page.viewHeader loggedIn (t "community.actions.title") (Route.Objectives model.communityId)
                        , viewForm loggedIn community model
                        ]

                LoadFailed err ->
                    Page.fullPageGraphQLError (t "error.invalidSymbol") err

                NotFound ->
                    Page.fullPageNotFound (t "community.actions.form.not_found") ""

                Unauthorized ->
                    Page.fullPageNotFound "not authorized" ""
    in
    { title = title
    , content =
        case loggedIn.hasObjectives of
            LoggedIn.FeatureLoaded True ->
                content

            LoggedIn.FeatureLoaded False ->
                Page.fullPageNotFound
                    (t "error.pageNotFound")
                    (t "community.objectives.disabled.description")

            LoggedIn.FeatureLoading ->
                Page.fullPageLoading
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

        text_ s =
            text (t s)
    in
    div [ class "mb-10" ]
        [ span [ class "input-label" ]
            [ text_ "community.actions.form.reward_label" ]
        , div [ class "flex w-full sm:w-2/5 h-12 rounded-sm border border-gray-500" ]
            [ input
                [ class "block w-4/5 border-none px-4 py-3 outline-none"
                , classList [ ( "border-red", hasErrors form.reward ) ]
                , type_ "number"
                , placeholder "0.00"
                , onInput EnteredReward
                , value (getInput form.reward)
                ]
                []
            , span
                [ class "w-1/5 flex text-white items-center justify-center bg-indigo-500 text-body uppercase rounded-r-sm" ]
                [ text (Eos.symbolToSymbolCodeString community.symbol) ]
            ]
        , viewFieldErrors (listErrors shared.translations form.reward)
        ]


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
            [ div []
                [ p [ class "input-label mb-6" ] [ text_ "community.actions.form.validity_label" ]
                , div [ class "flex" ]
                    [ div [ class "form-switch inline-block align-middle" ]
                        [ input
                            [ type_ "checkbox"
                            , id "expiration-toggle"
                            , name "expiration-toggle"
                            , class "form-switch-checkbox mr-2"
                            , checked (model.form.validation /= NoValidation)
                            , onCheck ToggleValidity
                            ]
                            []
                        , label [ class "form-switch-label", for "expiration-toggle" ] []
                        ]
                    , label [ class "flex text-body text-green", for "expiration-toggle" ]
                        [ p [ class "mr-1" ]
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
                        ]
                    ]
                ]
            ]
        , div
            [ class "" ]
            [ div [ class "mb-3 flex flex-row text-body items-bottom" ]
                [ input
                    [ id "date"
                    , type_ "checkbox"
                    , class "form-checkbox h-5 w-5 mr-2"
                    , checked (hasDateValidation model.form.validation)
                    , onCheck ToggleDeadline
                    ]
                    []
                , label
                    [ for "date", class "flex" ]
                    [ p [ class "mr-1" ]
                        [ b [] [ text_ "community.actions.form.date_validity" ]
                        , text_ "community.actions.form.date_validity_details"
                        ]
                    ]
                ]
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
                                        [ class "input w-full sm:w-2/5"
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
            , div [ class "mb-6 flex flex-row text-body items-bottom" ]
                [ input
                    [ id "quantity"
                    , type_ "checkbox"
                    , class "form-checkbox h-5 w-5 mr-2"
                    , checked (hasUnitValidation model.form.validation)
                    , onCheck ToggleUsages
                    ]
                    []
                , label [ for "quantity", class "flex" ]
                    [ p [ class "mr-1" ]
                        [ b [] [ text_ "community.actions.form.quantity_validity" ]
                        , text_ "community.actions.form.quantity_validity_details"
                        ]
                    ]
                ]
            ]
        , case model.form.validation of
            NoValidation ->
                text ""

            Validations _ usagesValidation ->
                case usagesValidation of
                    Just validation ->
                        div []
                            [ span [ class "input-label" ] [ text_ "community.actions.form.quantity_label" ]
                            , div [ class "mb-10" ]
                                [ input
                                    [ type_ "number"
                                    , class "input w-full sm:w-2/5"
                                    , classList [ ( "border-red", hasErrors validation ) ]
                                    , placeholder (t "community.actions.form.usages_placeholder")
                                    , value (getInput validation)
                                    , onInput EnteredUsages
                                    ]
                                    []
                                , viewFieldErrors (listErrors shared.translations validation)
                                ]
                            , case model.form.usagesLeft of
                                Just usagesLeftValidation ->
                                    div []
                                        [ span [ class "input-label" ]
                                            [ text_ "community.actions.form.usages_left_label" ]
                                        , div
                                            [ class "mb-10" ]
                                            [ input
                                                [ type_ "number"
                                                , class "input w-full sm:w-2/5"
                                                , classList [ ( "border-red", hasErrors usagesLeftValidation ) ]
                                                , value (getInput usagesLeftValidation)
                                                , onInput EnteredUsagesLeft
                                                ]
                                                []
                                            , viewFieldErrors (listErrors shared.translations usagesLeftValidation)
                                            ]
                                        ]

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
    in
    div [ class "mb-10" ]
        [ div [ class "flex flex-row justify-between mb-6" ]
            [ p [ class "input-label" ]
                [ text_ "community.actions.form.verification_label" ]
            ]
        , div [ class "mb-6" ]
            [ label [ class "inline-flex items-top" ]
                [ input
                    [ type_ "radio"
                    , class "form-radio h-5 w-5 text-green"
                    , name "verification"
                    , value "automatic"
                    , checked (model.form.verification == Automatic)
                    , onClick (SetVerification "automatic")
                    ]
                    []
                , span
                    [ class "flex ml-2 text-body"
                    , classList [ ( "text-green", model.form.verification == Automatic ) ]
                    ]
                    [ p [ class "mr-1" ]
                        [ b []
                            [ text_ "community.actions.form.automatic" ]
                        , text_ "community.actions.form.automatic_detail"
                        ]
                    ]
                ]
            ]
        , div [ class "mb-6" ]
            [ label [ class "inline-flex items-top" ]
                [ input
                    [ type_ "radio"
                    , class "form-radio h-5 w-5 text-green"
                    , name "verification"
                    , value "manual"
                    , checked (model.form.verification /= Automatic)
                    , onClick (SetVerification "manual")
                    ]
                    []
                , span
                    [ class "flex ml-2 text-body"
                    , classList [ ( "text-green", model.form.verification /= Automatic ) ]
                    ]
                    [ p [ class "mr-1" ]
                        [ b []
                            [ text_ "community.actions.form.manual" ]
                        , text_ "community.actions.form.manual_detail"
                        ]
                    ]
                ]
            , if model.form.verification /= Automatic then
                viewManualVerificationForm loggedIn model community

              else
                text ""
            ]
        ]


viewManualVerificationForm : LoggedIn.Model -> Model -> Community.Model -> Html Msg
viewManualVerificationForm ({ shared } as loggedIn) model community =
    let
        { t, tr } =
            shared.translators

        text_ s =
            text (t s)
    in
    case model.form.verification of
        Automatic ->
            text ""

        Manual { verifiers, verificationReward, minVotes, photoProof } ->
            let
                isPhotoProofEnabled =
                    case photoProof of
                        Just (Enabled _) ->
                            True

                        _ ->
                            False

                isProofNumberEnabled =
                    case photoProof of
                        Just (Enabled WithProofNumber) ->
                            True

                        _ ->
                            False
            in
            div [ class "mt-6 ml-8 sm:w-2/5" ]
                [ div [ class "mb-6" ]
                    [ label [ class "input-label block" ]
                        [ text_ "community.actions.form.votes_label" ]
                    , div [ class "space-x-4" ] <|
                        List.map (viewVotesCount (getInput minVotes)) [ 3, 5, 7, 9 ]
                    , viewFieldErrors (listErrors shared.translations minVotes)
                    ]
                , span [ class "input-label" ]
                    [ text (tr "community.actions.form.verifiers_label_count" [ ( "count", getInput minVotes ) ]) ]
                , div []
                    [ viewVerifierSelect shared model False
                    , viewFieldErrors (listErrors shared.translations verifiers)
                    , viewSelectedVerifiers loggedIn (getInput verifiers)
                    ]
                , span [ class "input-label" ]
                    [ text_ "community.actions.form.verifiers_reward_label" ]
                , div [ class "mb-10" ]
                    [ div [ class "flex flex-row border rounded-sm" ]
                        [ input
                            [ class "input w-4/5 border-none"
                            , type_ "number"
                            , placeholder "0.00"
                            , onInput EnteredVerifierReward
                            , value (getInput verificationReward)
                            ]
                            []
                        , span
                            [ class "w-1/5 flex input-token rounded-r-sm" ]
                            [ text (Eos.symbolToSymbolCodeString community.symbol) ]
                        ]
                    , viewFieldErrors (listErrors shared.translations verificationReward)
                    , div [ class "mt-8" ]
                        [ label [ class "flex text-body block" ]
                            [ input
                                [ type_ "checkbox"
                                , class "form-checkbox h-5 w-5 mr-2"
                                , checked isPhotoProofEnabled
                                , onCheck TogglePhotoProof
                                ]
                                []
                            , span []
                                [ b [ class "block" ] [ text "Photo validation" ]
                                , text "The users must send a photo performing the task for the claim to be valid."
                                ]
                            ]
                        , if isPhotoProofEnabled then
                            div [ class "mt-6" ]
                                [ label [ class "flex text-body block" ]
                                    [ input
                                        [ type_ "checkbox"
                                        , class "form-checkbox h-5 w-5 mr-2"
                                        , checked isProofNumberEnabled
                                        , onCheck TogglePhotoProofNumber
                                        ]
                                        []
                                    , span []
                                        [ b [ class "block" ] [ text "Verification number" ]
                                        , text "Users will have to send a unique verification code with the photo in order for the claim to be valid."
                                        ]
                                    ]
                                , div []
                                    [ label [ class "input-label" ]
                                        [ text "Write here instructions for the users" ]
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
                ]


viewVotesCount : String -> Int -> Html Msg
viewVotesCount selectedCount count =
    let
        countStr =
            String.fromInt count

        isChecked =
            selectedCount == countStr
    in
    label
        [ class "rounded-full relative overflow-hidden inline-block text-center leading-8 w-8"
        , classList
            [ ( "bg-orange-300 text-white border-orange-300", isChecked )
            , ( "hover:border-orange-300 cursor-pointer hover:text-orange-500 border border-grey-500", not isChecked )
            ]
        ]
        [ text countStr
        , input
            [ type_ "radio"
            , class "absolute left-0 opacity-0 cursor-pointer"
            , name "min_votes"
            , checked isChecked
            , onInput EnteredMinVotes
            , value countStr
            ]
            []
        ]


viewSelectedVerifiers : LoggedIn.Model -> List Profile -> Html Msg
viewSelectedVerifiers ({ shared } as loggedIn) selectedVerifiers =
    div [ class "flex flex-row mt-3 mb-6 flex-wrap" ]
        (selectedVerifiers
            |> List.map
                (\p ->
                    div
                        [ class "flex justify-between flex-col m-3 items-center" ]
                        [ Profile.view shared loggedIn.accountName p
                        , div
                            [ onClick (OnRemoveVerifier p)
                            , class "h-6 w-6 flex items-center mt-4"
                            ]
                            [ Icons.trash "" ]
                        ]
                )
        )


viewFieldErrors : List String -> Html msg
viewFieldErrors errors =
    div [ class "form-field-error" ]
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


selectConfiguration : Shared -> Bool -> Select.Config Msg Profile
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


viewVerifierSelect : Shared -> Model -> Bool -> Html Msg
viewVerifierSelect shared model isDisabled =
    let
        users =
            case model.status of
                Loaded community ->
                    community.members

                _ ->
                    []
    in
    case model.form.verification of
        Automatic ->
            text ""

        Manual { verifiers } ->
            div []
                [ Html.map SelectMsg
                    (Select.view (selectConfiguration shared isDisabled)
                        model.multiSelectState
                        users
                        (getInput verifiers)
                    )
                ]



-- UTILS


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
        CompletedCommunityLoad _ ->
            [ "CompletedCommunityLoad" ]

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

        ToggleValidity _ ->
            [ "ToggleValidity" ]

        ToggleDeadline _ ->
            [ "ToggleDeadline" ]

        ToggleUsages _ ->
            [ "ToggleDeadline" ]

        TogglePhotoProof _ ->
            [ "TogglePhotoValidation" ]

        TogglePhotoProofNumber _ ->
            [ "TogglePhotoWithNumberValidation" ]

        EnteredVerifierReward _ ->
            [ "EnteredVerifierReward" ]

        SetVerification _ ->
            [ "SetVerification" ]

        MarkAsCompleted ->
            [ "MarkAsCompleted" ]

        ValidateForm ->
            [ "ValidateDeadline" ]

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

        PressedEnter _ ->
            [ "PressedEnter" ]
