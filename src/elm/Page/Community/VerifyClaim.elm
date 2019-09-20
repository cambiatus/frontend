module Page.Community.VerifyClaim exposing (..)

import Asset.Icon
import Bespiral.Scalar exposing (DateTime(..))
import DateFormat
import Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import I18Next exposing (Delims, Replacements, Translations)
import Json.Encode as Encode
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Shared)
import Time
import UpdateResult as UR
import Utils
import View.Icon as Icon



-- INIT


init : LoggedIn.Model -> ClaimId -> ( Model, Cmd Msg )
init { accountName, shared } claimId =
    let
        validator : String
        validator =
            Eos.nameToString accountName

        verification : Verification
        verification =
            { symbol = "PUL"
            , logo = ""
            , name = "Pulpes"
            , objectiveDescription =
                """
                Mussum Ipsum, cacilds vidis litro abertis.
                Não sou faixa preta cumpadi, sou preto inteiris, inteiris.
                Diuretics paradis num copo é motivis de denguis.
                """
            , actionDescription =
                """
                Cevadis im ampola pa arma uma pindureta.
                Delegadis gente finis, bibendum egestas augue arcu ut est.
                Si num tem leite então bota uma pinga aí cumpadi!
                """
            , claimerReward = 10
            , verifierReward = 100
            , claimer = "alisson"
            , createdAt = DateTime "2019-09-20T16:00:00Z"
            , status = PENDING
            }
    in
    -- TODO: use LoadingVerification as first status and add API query
    ( { claimId = claimId, status = LoadVerification Closed (Just verification) }
      --    ( { claimId = claimId, status = LoadingVerification }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { claimId : ClaimId
    , status : Status
    }


type Status
    = LoadingVerification
    | LoadVerification ModalStatus (Maybe Verification)
    | LoadVerificationFailed (Graphql.Http.Error (Maybe Verification))


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
                            [ class "mx-4 md:mx-auto mt-0 md:mt-10 pb-4 md:pb-10 bg-white max-w-4xl rounded-lg" ]
                            [ viewHeader shared.endpoints.ipfs verification.logo verification.name
                            , viewStatus shared.translations verification.symbol verification.verifierReward verification.status
                            , viewInfo shared.translations verification
                            , viewAction shared.translations verification
                            ]
                        ]

                Nothing ->
                    -- TODO: add screen case verification does not exist
                    Debug.todo ""

        LoadVerificationFailed err ->
            Page.fullPageGraphQLError (t "verification.title") err


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
                [ class "z-50 bottom-0 md:inset-center fixed md:absolute w-full md:w-1/3 md:h-auto" ]
                [ div
                    [ class "relative rounded-t-lg md:rounded-lg bg-white" ]
                    [ div
                        [ class "px-4 pt-4 md:px-6 md:pt-6" ]
                        [ div
                            [ class "flex justify-between items-center" ]
                            [ p
                                [ class "text-heading font-bold" ]
                                [ text (t "verify_claim.modal.title") ]
                            , button
                                [ class "w-8 h-8"
                                , onClick (ClickedClose verification)
                                ]
                                [ Asset.Icon.close "text-gray-900 fill-current" ]
                            ]
                        , p
                            [ class "text-body pt-4 md:pt-6" ]
                            [ text (t message) ]
                        ]
                    , div
                        [ class "flex flex-col md:flex-row md:justify-center md:rounded-b-lg px-4 pt-20 pb-4 md:mt-28 md:px-0 md:py-4 md:bg-gray-100" ]
                        [ button
                            [ class "button button-secondary uppercase button-medium w-full md:w-1/3"
                            , onClick (ClickedClose verification)
                            ]
                            [ text (t "verify_claim.modal.secondary") ]
                        , button
                            [ class "button button-primary uppercase button-medium w-full md:w-1/3 mt-4 md:mt-0 md:ml-6"
                            , onClick (ClickedConfirm verification vote)
                            ]
                            [ text (t primaryText)
                            ]
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
                Just logo
    in
    div
        [ class "p-6 flex items-center" ]
        [ Icon.mediumView ipfsUrl maybeLogo ""
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
                    , icon = Asset.Icon.alert
                    , status = t "verify_claim.pending"
                    , sub = Nothing
                    , aux = Nothing
                    }

                DISAPPROVED_AND_UNDER_REVIEW ->
                    { bgColor = " bg-red"
                    , textColor = " text-white"
                    , icon = Asset.Icon.fail
                    , status = t "verify_claim.disapproved"
                    , sub = Just (t "verify_claim.sub_wait_1")
                    , aux = Just (t "verify_claim.sub_wait_2")
                    }

                APPROVED_AND_UNDER_REVIEW ->
                    { bgColor = " bg-green"
                    , textColor = " text-white"
                    , icon = Asset.Icon.success
                    , status = t "verify_claim.approved"
                    , sub = Just (t "verify_claim.sub_wait_1")
                    , aux = Just (t "verify_claim.sub_wait_2")
                    }

                DISAPPROVED ->
                    { bgColor = " bg-red"
                    , textColor = " text-white"
                    , icon = Asset.Icon.fail
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
                    , icon = Asset.Icon.success
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
            [ class "pt-8 font-medium" ]
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
    = CompletedVerificationLoad (Result (Graphql.Http.Error (Maybe Verification)) (Maybe Verification))
    | ClickedDisapprove Verification
    | ClickedApproved Verification
    | ClickedClose Verification
    | ClickedConfirm Verification Vote
    | GotVerificationResponse (Result Encode.Value String)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model { accountName, shared } =
    case msg of
        CompletedVerificationLoad (Ok maybeVerification) ->
            { model | status = LoadVerification Closed maybeVerification }
                |> UR.init

        CompletedVerificationLoad (Err err) ->
            { model | status = LoadVerificationFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        ClickedDisapprove verification ->
            { model | status = LoadVerification (Opened Disapproved) (Just verification) }
                |> UR.init
                |> UR.addExt (LoggedIn.TurnLights True)

        ClickedApproved verification ->
            { model | status = LoadVerification (Opened Approved) (Just verification) }
                |> UR.init
                |> UR.addExt (LoggedIn.TurnLights True)

        ClickedClose verification ->
            { model | status = LoadVerification Closed (Just verification) }
                |> UR.init
                |> UR.addExt (LoggedIn.TurnLights False)

        ClickedConfirm verification vote ->
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
