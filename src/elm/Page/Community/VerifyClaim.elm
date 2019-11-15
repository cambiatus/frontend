module Page.Community.VerifyClaim exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Api.Graphql
import Bespiral.Object exposing (Claim, Profile)
import Bespiral.Object.Action as Action
import Bespiral.Object.Check as Check
import Bespiral.Object.Claim as Claim exposing (ChecksOptionalArguments)
import Bespiral.Object.Community as Community
import Bespiral.Object.Objective as Objective
import Bespiral.Object.Profile as Profile
import Bespiral.Object.Validator as Validator
import Bespiral.Query exposing (ClaimRequiredArguments, ClaimsRequiredArguments)
import Bespiral.Scalar exposing (DateTime(..))
import DateFormat
import Eos
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import I18Next exposing (Delims, Replacements, Translations)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Shared)
import Time
import UpdateResult as UR
import Utils



-- INIT


init : LoggedIn.Model -> ClaimId -> ( Model, Cmd Msg )
init { accountName, shared } claimId =
    ( { claimId = claimId, status = LoadingVerification }
    , fetchVerification claimId accountName shared
    )



-- MODEL


type alias Model =
    { claimId : ClaimId
    , status : Status
    }


type Status
    = LoadingVerification
    | LoadVerification ModalStatus (Maybe Verification)
    | LoadVerificationFailed (Graphql.Http.Error VerificationResponse)


type ModalStatus
    = Opened Vote
    | Closed


type Vote
    = Disapproved
    | Approved


type alias ClaimId =
    String


type alias Verification =
    { symbol : String
    , logo : String
    , name : String
    , objectiveDescription : String
    , actionDescription : String
    , claimerReward : Float
    , verifierReward : Float
    , claimer : String
    , createdAt : DateTime
    , status : VerificationStatus
    }


type VerificationStatus
    = PENDING
    | DISAPPROVED_AND_UNDER_REVIEW
    | APPROVED_AND_UNDER_REVIEW
    | DISAPPROVED
    | APPROVED



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view { accountName, shared } { claimId, status } =
    let
        t : String -> String
        t =
            I18Next.t shared.translations
    in
    case status of
        LoadingVerification ->
            Page.fullPageLoading

        LoadVerification modalStatus maybeVerification ->
            case maybeVerification of
                Just verification ->
                    div
                        [ class "font-sans" ]
                        [ viewModal shared.translations modalStatus verification
                        , div
                            [ class "mx-4 mt-4 md:mt-10 md:mx-8 lg:mx-auto pb-4 md:pb-10 bg-white max-w-4xl rounded-lg" ]
                            [ viewHeader shared.endpoints.ipfs verification.logo verification.name
                            , viewStatus shared.translations verification.symbol verification.verifierReward verification.status
                            , viewInfo shared.translations verification
                            , viewAction shared.translations verification
                            ]
                        ]

                Nothing ->
                    div
                        [ class "flex justify-center items-center md:w-full mx-4 mt-4 md:mx-auto md:mt-10 bg-white max-w-4xl rounded-lg" ]
                        [ p
                            [ class "font-sans text-body text-black text-center p-8" ]
                            [ text (t "verify_claim.no_results_found") ]
                        ]

        LoadVerificationFailed err ->
            Page.fullPageGraphQLError (t "error.unknown") err


viewModal : Translations -> ModalStatus -> Verification -> Html Msg
viewModal translations modalStatus verification =
    let
        t : String -> String
        t =
            I18Next.t translations
    in
    case modalStatus of
        Opened vote ->
            let
                ( message, primaryText ) =
                    if vote == Disapproved then
                        ( "verify_claim.modal.message_disapprove"
                        , "verify_claim.modal.primary_disapprove"
                        )

                    else
                        ( "verify_claim.modal.message_approve"
                        , "verify_claim.modal.primary_approve"
                        )
            in
            div
                [ class "container modal" ]
                [ div
                    [ class "modal-bg"
                    , onClick (ClickedClose verification)
                    ]
                    []
                , div
                    [ class "modal-content" ]
                    [ div
                        [ class "w-full" ]
                        [ p
                            [ class "font-sans w-full font-bold text-heading text-2xl mb-4" ]
                            [ text (t "verify_claim.modal.title") ]
                        , button
                            [ onClick (ClickedClose verification)
                            ]
                            [ Icons.close "absolute fill-current text-gray-400 top-0 right-0 mx-8 my-4"
                            ]
                        , p [ class "text-body w-full font-sans mb-10" ]
                            [ text (t message) ]
                        ]
                    , div [ class "w-full md:bg-gray-100 md:flex md:absolute rounded-b-lg md:inset-x-0 md:bottom-0 md:p-4" ]
                        [ div [ class "flex-1" ] []
                        , button
                            [ class "flex-1 block button button-secondary mb-4 button-large w-full md:w-40 md:mb-0"
                            , onClick (ClickedClose verification)
                            ]
                            [ text (t "verify_claim.modal.secondary") ]
                        , div [ class "flex-1" ] []
                        , button
                            [ class "flex-1 block button button-primary button-large w-full md:w-40"
                            , onClick (ClickedConfirm verification vote)
                            ]
                            [ text (t primaryText) ]
                        , div [ class "flex-1" ] []
                        ]
                    ]
                ]

        Closed ->
            text ""


viewHeader : String -> String -> String -> Html Msg
viewHeader ipfsUrl logo name =
    let
        maybeLogo : Maybe String
        maybeLogo =
            if String.isEmpty logo then
                Nothing

            else
                Just (ipfsUrl ++ "/" ++ logo)
    in
    div
        [ class "p-6 flex items-center" ]
        [ case maybeLogo of
            Just logoUrl ->
                img
                    [ class "h-14"
                    , src logoUrl
                    ]
                    []

            Nothing ->
                div
                    [ class "w-16 h-16" ]
                    []
        , p
            [ class "pl-6 text-heading text-black font-medium" ]
            [ text name ]
        ]


viewStatus : Translations -> String -> Float -> VerificationStatus -> Html Msg
viewStatus translations symbol verifierReward verificationStatus =
    let
        t : String -> String
        t =
            I18Next.t translations

        tr : String -> Replacements -> String
        tr =
            I18Next.tr translations I18Next.Curly

        info :
            { bgColor : String
            , textColor : String
            , icon : String -> Html Msg
            , status : String
            , sub : Maybe String
            , aux : Maybe String
            }
        info =
            case verificationStatus of
                PENDING ->
                    { bgColor = " bg-gray-100"
                    , textColor = " text-black"
                    , icon = Icons.alert
                    , status = t "verify_claim.pending"
                    , sub = Nothing
                    , aux = Nothing
                    }

                DISAPPROVED_AND_UNDER_REVIEW ->
                    { bgColor = " bg-red"
                    , textColor = " text-white"
                    , icon = Icons.close
                    , status = t "verify_claim.disapproved"
                    , sub = Just (t "verify_claim.sub_wait_1")
                    , aux = Just (t "verify_claim.sub_wait_2")
                    }

                APPROVED_AND_UNDER_REVIEW ->
                    { bgColor = " bg-green"
                    , textColor = " text-white"
                    , icon = Icons.success
                    , status = t "verify_claim.approved"
                    , sub = Just (t "verify_claim.sub_wait_1")
                    , aux = Just (t "verify_claim.sub_wait_2")
                    }

                DISAPPROVED ->
                    { bgColor = " bg-red"
                    , textColor = " text-white"
                    , icon = Icons.close
                    , status = t "verify_claim.disapproved"
                    , sub =
                        Just
                            (tr "verify_claim.reward"
                                [ ( "value"
                                  , String.fromFloat verifierReward ++ " " ++ symbol
                                  )
                                ]
                            )
                    , aux = Nothing
                    }

                APPROVED ->
                    { bgColor = " bg-green"
                    , textColor = " text-white"
                    , icon = Icons.success
                    , status = t "verify_claim.approved"
                    , sub =
                        Just
                            (tr "verify_claim.reward"
                                [ ( "value"
                                  , String.fromFloat verifierReward ++ " " ++ symbol
                                  )
                                ]
                            )
                    , aux = Nothing
                    }
    in
    div
        [ class ("inline-flex flex-col items-center w-full p-6 " ++ info.bgColor ++ info.textColor)
        ]
        [ info.icon "w-8 h-8 fill-current"
        , p
            [ class "pt-6 font-medium" ]
            [ text info.status ]
        , case info.sub of
            Just subText ->
                p
                    [ class "pt-2" ]
                    [ text subText ]

            Nothing ->
                text ""
        , case info.aux of
            Just auxText ->
                p
                    [ class "pt-1" ]
                    [ text auxText ]

            Nothing ->
                text ""
        ]


viewInfo : Translations -> Verification -> Html Msg
viewInfo translations verification =
    let
        t : String -> String
        t =
            I18Next.t translations

        date : String
        date =
            verification.createdAt
                |> Just
                |> Utils.posixDateTime
                |> dateFormatter zoneFormatter
    in
    div
        [ class "px-6 pt-6" ]
        [ p
            [ class "text-caption uppercase text-green" ]
            [ text (t "verify_claim.objective") ]
        , p
            [ class "pt-2 text-body text-black" ]
            [ text verification.objectiveDescription ]
        , p
            [ class "pt-6 text-caption uppercase text-green" ]
            [ text (t "verify_claim.action") ]
        , p
            [ class "pt-2 text-body text-black" ]
            [ text verification.actionDescription ]
        , div
            [ class "md:hidden flex justify-between pt-6" ]
            [ div
                [ class "inline-flex flex-col justify-start" ]
                [ div
                    [ class "" ]
                    [ p
                        [ class "text-caption uppercase text-green" ]
                        [ text (t "verify_claim.claimer") ]
                    , p
                        [ class "pt-2 text-body text-orange-300" ]
                        [ text verification.claimer ]
                    ]
                , div
                    [ class "pt-6" ]
                    [ p
                        [ class "text-caption uppercase text-green" ]
                        [ text (t "verify_claim.claimer_reward") ]
                    , p
                        [ class "pt-2 text-body text-black" ]
                        [ text (String.fromFloat verification.claimerReward ++ " " ++ verification.symbol) ]
                    ]
                ]
            , div
                [ class "inline-flex flex-col justify-start" ]
                [ div
                    [ class "" ]
                    [ p
                        [ class "text-caption uppercase text-green" ]
                        [ text (t "verify_claim.date") ]
                    , p
                        [ class "pt-2 text-body text-black" ]
                        [ text date ]
                    ]
                , div
                    [ class "pt-6" ]
                    [ p
                        [ class "text-caption uppercase text-green" ]
                        [ text (t "verify_claim.your_reward") ]
                    , p
                        [ class "pt-2 text-body text-black" ]
                        [ text (String.fromFloat verification.verifierReward ++ " " ++ verification.symbol) ]
                    ]
                ]
            ]
        , div
            [ class "hidden md:visible md:flex justify-between pt-6" ]
            [ div
                []
                [ p
                    [ class "text-caption uppercase text-green" ]
                    [ text (t "verify_claim.claimer") ]
                , p
                    [ class "pt-2 text-body text-orange-300" ]
                    [ text verification.claimer ]
                ]
            , div
                []
                [ p
                    [ class "text-caption uppercase text-green" ]
                    [ text (t "verify_claim.date") ]
                , p
                    [ class "pt-2 text-body text-black" ]
                    [ text date ]
                ]
            , div
                []
                [ p
                    [ class "text-caption uppercase text-green" ]
                    [ text (t "verify_claim.claimer_reward") ]
                , p
                    [ class "pt-2 text-body text-black" ]
                    [ text (String.fromFloat verification.claimerReward ++ " " ++ verification.symbol) ]
                ]
            , div
                []
                [ p
                    [ class "text-caption uppercase text-green" ]
                    [ text (t "verify_claim.your_reward") ]
                , p
                    [ class "pt-2 text-body text-black" ]
                    [ text (String.fromFloat verification.verifierReward ++ " " ++ verification.symbol) ]
                ]
            ]
        ]


viewAction : Translations -> Verification -> Html Msg
viewAction translations verification =
    let
        t : String -> String
        t =
            I18Next.t translations
    in
    if verification.status == PENDING then
        div
            [ class "flex flex-col md:flex-row justify-center mt-16 md:mt-24 px-4 md:px-0" ]
            [ button
                [ class "button button-secondary uppercase button-medium w-full md:w-1/4"
                , onClick (ClickedDisapprove verification)
                ]
                [ text (t "verify_claim.disapprove") ]
            , button
                [ class "button button-primary button-medium uppercase w-full md:w-1/4 mt-4 md:mt-0 md:ml-6"
                , onClick (ClickedApproved verification)
                ]
                [ text (t "verify_claim.approve") ]
            ]

    else
        text ""



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedVerificationLoad (Result (Graphql.Http.Error VerificationResponse) VerificationResponse)
    | ClickedDisapprove Verification
    | ClickedApproved Verification
    | ClickedClose Verification
    | ClickedConfirm Verification Vote
    | GotVerificationResponse (Result Encode.Value String)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ accountName, shared } as loggedIn) =
    case msg of
        CompletedVerificationLoad (Ok response) ->
            let
                maybeVerification =
                    toMaybeVerification
                        (Eos.nameToString accountName)
                        response
            in
            { model | status = LoadVerification Closed maybeVerification }
                |> UR.init

        CompletedVerificationLoad (Err err) ->
            { model | status = LoadVerificationFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        ClickedDisapprove verification ->
            { model | status = LoadVerification (Opened Disapproved) (Just verification) }
                |> UR.init

        ClickedApproved verification ->
            { model | status = LoadVerification (Opened Approved) (Just verification) }
                |> UR.init

        ClickedClose verification ->
            { model | status = LoadVerification Closed (Just verification) }
                |> UR.init

        ClickedConfirm verification vote ->
            case LoggedIn.isAuth loggedIn of
                True ->
                    { model | status = LoadVerification Closed (Just verification) }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = msg
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    { actions =
                                        [ { accountName = "bes.cmm"
                                          , name = "verifyclaim"
                                          , authorization =
                                                { actor = accountName
                                                , permissionName = Eos.samplePermission
                                                }
                                          , data = encodeVerification model.claimId accountName vote
                                          }
                                        ]
                                    }
                            }

                False ->
                    model
                        |> UR.init
                        |> UR.addExt (Just (ClickedConfirm verification vote) |> LoggedIn.RequiredAuthentication)

        GotVerificationResponse (Ok _) ->
            model
                |> UR.init
                |> UR.addCmd
                    (Route.replaceUrl shared.navKey Route.Dashboard)

        GotVerificationResponse (Err err) ->
            model
                |> UR.init
                |> UR.logDebugValue msg err


encodeVerification : ClaimId -> Eos.Name -> Vote -> Encode.Value
encodeVerification claimId validator vote =
    let
        encodedClaimId : Encode.Value
        encodedClaimId =
            case String.toInt claimId of
                Just number ->
                    Encode.int number

                Nothing ->
                    Encode.null

        encodedVerifier : Encode.Value
        encodedVerifier =
            Eos.encodeName validator

        encodedVote : Encode.Value
        encodedVote =
            case vote of
                Disapproved ->
                    False
                        |> Eos.boolToEosBool
                        |> Eos.encodeEosBool

                Approved ->
                    True
                        |> Eos.boolToEosBool
                        |> Eos.encodeEosBool
    in
    Encode.object
        [ ( "claim_id", encodedClaimId )
        , ( "verifier", encodedVerifier )
        , ( "vote", encodedVote )
        ]



-- HELPERS


type alias VerificationResponse =
    { claim : ClaimResponse
    }


type alias ClaimResponse =
    { createdAt : DateTime
    , isVerified : Bool
    , checks : List CheckResponse
    , claimer : ProfileResponse
    , action : ActionResponse
    }


type alias CheckResponse =
    { isVerified : Bool
    }


type alias ProfileResponse =
    { account : String
    }


type alias ActionResponse =
    { description : String
    , reward : Float
    , verifierReward : Float
    , objective : ObjectiveResponse
    , validators : List ValidatorResponse
    }


type alias ObjectiveResponse =
    { description : String
    , community : CommunityResponse
    }


type alias CommunityResponse =
    { symbol : String
    , logo : String
    , name : String
    }


type alias ValidatorResponse =
    { validator : ProfileResponse
    }


type CheckStatus
    = BLANK
    | NEGATIVE
    | POSITIVE


fetchVerification : ClaimId -> Eos.Name -> Shared -> Cmd Msg
fetchVerification claimId accountName shared =
    let
        id : Int
        id =
            String.toInt claimId
                |> Maybe.withDefault -1

        validator : String
        validator =
            Eos.nameToString accountName
    in
    Api.Graphql.query
        shared
        (verificationSelectionSet id validator)
        CompletedVerificationLoad


verificationSelectionSet : Int -> String -> SelectionSet VerificationResponse RootQuery
verificationSelectionSet id validator =
    let
        claimInput : ClaimRequiredArguments
        claimInput =
            { input = { id = id }
            }

        selectionSet : SelectionSet ClaimResponse Bespiral.Object.Claim
        selectionSet =
            claimSelectionSet validator
    in
    SelectionSet.succeed VerificationResponse
        |> with (Bespiral.Query.claim claimInput selectionSet)


claimSelectionSet : String -> SelectionSet ClaimResponse Bespiral.Object.Claim
claimSelectionSet validator =
    let
        checksArg : ChecksOptionalArguments -> ChecksOptionalArguments
        checksArg _ =
            { input = Present { validator = Present validator }
            }
    in
    SelectionSet.succeed ClaimResponse
        |> with Claim.createdAt
        |> with Claim.isVerified
        |> with (Claim.checks checksArg checkSelectionSet)
        |> with (Claim.claimer profileSelectionSet)
        |> with (Claim.action actionSelectionSet)


checkSelectionSet : SelectionSet CheckResponse Bespiral.Object.Check
checkSelectionSet =
    SelectionSet.succeed CheckResponse
        |> with Check.isVerified


profileSelectionSet : SelectionSet ProfileResponse Bespiral.Object.Profile
profileSelectionSet =
    SelectionSet.succeed ProfileResponse
        |> with Profile.account


actionSelectionSet : SelectionSet ActionResponse Bespiral.Object.Action
actionSelectionSet =
    SelectionSet.succeed ActionResponse
        |> with Action.description
        |> with Action.reward
        |> with Action.verifierReward
        |> with (Action.objective objectiveSelectionSet)
        |> with (Action.validators validatorsSelectionSet)


objectiveSelectionSet : SelectionSet ObjectiveResponse Bespiral.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed ObjectiveResponse
        |> with Objective.description
        |> with (Objective.community communitySelectionSet)


communitySelectionSet : SelectionSet CommunityResponse Bespiral.Object.Community
communitySelectionSet =
    SelectionSet.succeed CommunityResponse
        |> with Community.symbol
        |> with Community.logo
        |> with Community.name


validatorsSelectionSet : SelectionSet ValidatorResponse Bespiral.Object.Validator
validatorsSelectionSet =
    SelectionSet.succeed ValidatorResponse
        |> with (Validator.validator profileSelectionSet)


toMaybeVerification : String -> VerificationResponse -> Maybe Verification
toMaybeVerification validator verificationResponse =
    let
        claimResponse : ClaimResponse
        claimResponse =
            verificationResponse.claim

        checksResponse : List CheckResponse
        checksResponse =
            claimResponse.checks

        claimerResponse : ProfileResponse
        claimerResponse =
            claimResponse.claimer

        actionResponse : ActionResponse
        actionResponse =
            claimResponse.action

        objectiveResponse : ObjectiveResponse
        objectiveResponse =
            actionResponse.objective

        communityResponse : CommunityResponse
        communityResponse =
            objectiveResponse.community

        validatorsResponse : List ProfileResponse
        validatorsResponse =
            List.map
                (\v -> v.validator)
                actionResponse.validators

        validatorVote : CheckStatus
        validatorVote =
            case List.head checksResponse of
                Just check ->
                    if check.isVerified then
                        POSITIVE

                    else
                        NEGATIVE

                Nothing ->
                    BLANK

        isValidVerifier : Bool
        isValidVerifier =
            not (claimResponse.isVerified == True && validatorVote == BLANK)

        isValidValidator : Bool
        isValidValidator =
            List.any
                (\v -> v.account == validator)
                validatorsResponse
    in
    if isValidValidator && isValidVerifier then
        Just
            { symbol = communityResponse.symbol
            , logo = communityResponse.logo
            , name = communityResponse.name
            , objectiveDescription = objectiveResponse.description
            , actionDescription = actionResponse.description
            , claimerReward = actionResponse.reward
            , verifierReward = actionResponse.verifierReward
            , claimer = claimerResponse.account
            , createdAt = claimResponse.createdAt
            , status =
                if claimResponse.isVerified == False && validatorVote == BLANK then
                    PENDING

                else if claimResponse.isVerified == False && validatorVote == NEGATIVE then
                    DISAPPROVED_AND_UNDER_REVIEW

                else if claimResponse.isVerified == False && validatorVote == POSITIVE then
                    APPROVED_AND_UNDER_REVIEW

                else if claimResponse.isVerified == True && validatorVote == NEGATIVE then
                    DISAPPROVED

                else
                    APPROVED
            }

    else
        Nothing


dateFormatter : Time.Zone -> Time.Posix -> String
dateFormatter =
    DateFormat.format
        [ DateFormat.dayOfMonthNumber
        , DateFormat.text " "
        , DateFormat.monthNameAbbreviated
        , DateFormat.text " "
        , DateFormat.yearNumber
        ]


zoneFormatter : Time.Zone
zoneFormatter =
    Time.utc


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClickedConfirm" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.succeed (Err Encode.null)
                    ]
                )
                val
                |> Result.map (Just << GotVerificationResponse)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedVerificationLoad r ->
            [ "CompletedVerificationLoad", UR.resultToString r ]

        ClickedDisapprove _ ->
            [ "ClickedDisapprove" ]

        ClickedApproved _ ->
            [ "ClickedApprove" ]

        ClickedClose _ ->
            [ "ClickedCancel" ]

        ClickedConfirm _ _ ->
            [ "ClickedConfirm" ]

        GotVerificationResponse r ->
            [ "GotSaveResponse", UR.resultToString r ]
