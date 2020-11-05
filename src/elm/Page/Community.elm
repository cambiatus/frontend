module Page.Community exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , subscriptions
    , update
    , view
    )

import Api
import Api.Graphql
import Avatar
import Cambiatus.Enum.VerificationType as VerificationType
import Claim
import Community exposing (Action, Model)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Html exposing (Html, a, button, div, hr, img, input, label, p, span, text)
import Html.Attributes exposing (accept, class, classList, disabled, id, multiple, src, style, type_)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (t)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Session.Shared exposing (Translators)
import Strftime
import Task
import Time exposing (Posix, posixToMillis)
import UpdateResult as UR
import Utils
import View.Modal as Modal



-- INIT


init : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) symbol =
    ( initModel loggedIn symbol
    , Cmd.batch
        [ Api.Graphql.query shared (Community.communityQuery symbol) CompletedLoadCommunity
        , Task.perform GotTime Time.now
        ]
    )


initModel : LoggedIn.Model -> Symbol -> Model
initModel _ _ =
    { date = Nothing
    , pageStatus = Loading
    , actionId = Nothing
    , openObjective = Nothing
    , claimConfirmationModalStatus = Closed
    , proofs = Nothing
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.proofs of
        Just (Proof _ (CodeParts _)) ->
            Time.every 1000 Tick

        _ ->
            Sub.none



-- MODEL


type alias Model =
    { date : Maybe Posix
    , pageStatus : PageStatus
    , actionId : Maybe Int
    , openObjective : Maybe Int
    , claimConfirmationModalStatus : ClaimConfirmationModalStatus
    , proofs : Maybe Proof
    }


type Proof
    = Proof ProofPhotoStatus ProofCode


type ProofCode
    = NoCodeRequired
    | CodeParts
        { code : Maybe String
        , claimTimestamp : Int
        , secondsAfterClaim : Int
        , availabilityPeriod : Int
        }


type PageStatus
    = Loading
    | Loaded Community.Model ActiveSection
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Community.Model))


type ActiveSection
    = Objectives EditStatus
    | ClaimWithProofs Community.Action


type EditStatus
    = NoEdit


type ClaimConfirmationModalStatus
    = Open Community.Action
    | InProgress
    | Closed


type ProofPhotoStatus
    = NoPhotoAdded
    | Uploading
    | UploadFailed Http.Error
    | Uploaded String



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        text_ s =
            text (t s)

        title =
            case model.pageStatus of
                Loaded community _ ->
                    community.title

                Loading ->
                    t ""

                _ ->
                    t "community.not_found"

        content =
            case model.pageStatus of
                Loading ->
                    Page.fullPageLoading

                NotFound ->
                    Page.viewCardEmpty [ text_ "community.not_found" ]

                Failed e ->
                    Page.fullPageGraphQLError (t "community.objectives.title") e

                Loaded community pageStatus ->
                    case pageStatus of
                        Objectives editStatus ->
                            let
                                canEdit =
                                    LoggedIn.isAccount community.creator loggedIn
                            in
                            div []
                                [ viewHeader loggedIn community
                                , div [ class "bg-white pt-20 pb-10 sm:pb-20" ]
                                    [ div [ class "container mx-auto" ]
                                        [ div [ class "px-4 text-center" ]
                                            [ p [ class "text-3xl text-black sm:text-4xl leading-tight font-bold mb-4" ]
                                                [ text community.title ]
                                            , p [ class "text-grey-200 text-sm" ] [ text community.description ]
                                            ]
                                        ]
                                    ]
                                , if community.hasObjectives then
                                    div [ class "container mx-auto px-4" ]
                                        [ viewClaimConfirmation loggedIn model
                                        , div [ class "bg-white py-6 sm:py-8 px-3 sm:px-6 rounded-lg mt-4" ]
                                            (Page.viewTitle (t "community.objectives.title_plural")
                                                :: List.indexedMap (viewObjective loggedIn model community)
                                                    community.objectives
                                                ++ [ if canEdit then
                                                        viewObjectiveNew loggedIn editStatus community.symbol

                                                     else
                                                        text ""
                                                   ]
                                            )
                                        ]

                                  else
                                    text ""
                                ]

                        ClaimWithProofs action ->
                            viewClaimWithPhoto model loggedIn action
    in
    { title = title
    , content =
        div
            -- id is used to scroll into view with port
            [ id "communityPage" ]
            [ content ]
    }


viewHeader : LoggedIn.Model -> Community.Model -> Html Msg
viewHeader { shared } community =
    div []
        [ div [ class "h-16 w-full bg-indigo-500 flex px-4 items-center" ]
            [ a
                [ class "items-center flex absolute"
                , Route.href Route.Dashboard
                ]
                [ Icons.back ""
                , p [ class "text-white text-sm ml-2" ]
                    [ text (t shared.translations "back")
                    ]
                ]
            , p [ class "text-white mx-auto" ] [ text community.title ]
            ]
        , div [ class "h-24 bg-indigo-500 flex flex-wrap content-end" ]
            [ div [ class "h-24 w-24 rounded-full mx-auto pt-12" ]
                [ img
                    [ src community.logo
                    , class "object-scale-down"
                    ]
                    []
                ]
            ]
        ]


viewClaimWithPhoto : Model -> LoggedIn.Model -> Action -> Html Msg
viewClaimWithPhoto model { shared } action =
    let
        { t } =
            shared.translators

        isUploadingInProgress =
            case model.proofs of
                Just (Proof Uploading _) ->
                    True

                _ ->
                    False
    in
    div [ class "bg-white border-t border-gray-300" ]
        [ div [ class "container p-4 mx-auto" ]
            [ div [ class "heading-bold leading-7 font-bold" ] [ text <| t "community.actions.proof.title" ]
            , p [ class "mb-4" ]
                [ text <|
                    Maybe.withDefault "" action.photoProofInstructions
                ]
            , case model.proofs of
                Just (Proof _ (CodeParts { code, secondsAfterClaim, availabilityPeriod })) ->
                    case code of
                        Just c ->
                            viewProofCode
                                shared.translators
                                c
                                secondsAfterClaim
                                availabilityPeriod

                        _ ->
                            text ""

                _ ->
                    text ""
            , div [ class "mb-4" ]
                [ span [ class "input-label block mb-2" ]
                    [ text (t "community.actions.proof.photo") ]
                , case model.proofs of
                    Just (Proof proofPhoto _) ->
                        viewPhotoUploader shared.translators proofPhoto

                    _ ->
                        text ""
                ]
            , div [ class "md:flex" ]
                [ button
                    [ class "modal-cancel"
                    , onClick
                        (if isUploadingInProgress then
                            NoOp

                         else
                            CloseProofSection CancelClicked
                        )
                    , classList [ ( "button-disabled", isUploadingInProgress ) ]
                    , disabled isUploadingInProgress
                    ]
                    [ text (t "menu.cancel") ]
                , button
                    [ class "modal-accept"
                    , classList [ ( "button-disabled", isUploadingInProgress ) ]
                    , onClick
                        (if isUploadingInProgress then
                            NoOp

                         else
                            ClaimAction action
                        )
                    , disabled isUploadingInProgress
                    ]
                    [ text (t "menu.send") ]
                ]
            ]
        ]


viewProofCode : Translators -> String -> Int -> Int -> Html msg
viewProofCode { t } proofCode secondsAfterClaim proofCodeValiditySeconds =
    let
        remainingSeconds =
            proofCodeValiditySeconds - secondsAfterClaim

        timerMinutes =
            remainingSeconds // 60

        timerSeconds =
            remainingSeconds - (timerMinutes * 60)

        toString timeVal =
            if timeVal < 10 then
                "0" ++ String.fromInt timeVal

            else
                String.fromInt timeVal

        timer =
            toString timerMinutes ++ ":" ++ toString timerSeconds
    in
    div [ class "mb-4" ]
        [ span [ class "input-label block mb-1" ]
            [ text (t "community.actions.form.verification_number") ]
        , div [ class "text-2xl text-black font-bold inline-block align-middle mr-2" ]
            [ text proofCode ]
        , span [ class "whitespace-no-wrap text-body rounded-full bg-lightred px-3 py-1 text-white" ]
            [ text (t "community.actions.proof.code_period_label")
            , text " "
            , text timer
            ]
        ]



-- VIEW OBJECTIVE


viewObjective : LoggedIn.Model -> Model -> Community.Model -> Int -> Community.Objective -> Html Msg
viewObjective loggedIn model metadata index objective =
    let
        canEdit =
            LoggedIn.isAccount metadata.creator loggedIn

        isOpen : Bool
        isOpen =
            case model.openObjective of
                Just obj ->
                    obj == index

                Nothing ->
                    False

        arrowStyle : String
        arrowStyle =
            if canEdit then
                " sm:flex-grow-2"

            else
                " "

        actsNButton : List (Html Msg)
        actsNButton =
            List.map (viewAction loggedIn metadata model.date) objective.actions
                |> List.intersperse (hr [ class "bg-border-grey text-border-grey" ] [])
    in
    div [ class "my-2" ]
        [ div
            [ class "px-3 py-4 bg-indigo-500 flex flex-col sm:flex-row sm:items-center sm:h-10 cursor-pointer"
            , if isOpen then
                onClick ClickedCloseObjective

              else
                onClick (ClickedOpenObjective index)
            ]
            [ div [ class "sm:flex-grow-7 sm:w-5/12" ]
                [ div
                    [ class "truncate overflow-hidden whitespace-no-wrap text-white font-medium text-sm overflow-hidden sm:flex-grow-8 sm:leading-normal sm:text-lg"
                    ]
                    [ text objective.description ]
                ]
            , div [ class ("flex flex-row justify-end mt-5 sm:mt-0" ++ arrowStyle) ]
                [ button
                    [ class ""
                    , if isOpen then
                        onClick ClickedCloseObjective

                      else
                        onClick (ClickedOpenObjective index)
                    ]
                    [ img
                        [ class "fill-current text-white h-2 w-4 stroke-current"
                        , src "/icons/objective_arrow.svg"
                        , classList [ ( "rotate-180", isOpen ) ]
                        ]
                        []
                    ]
                ]
            ]
        , if isOpen then
            div [ class "pt-5 px-3 pb-3 sm:px-6 bg-white rounded-b-lg border-solid border-grey border" ]
                actsNButton

          else
            text ""
        ]


viewObjectiveNew : LoggedIn.Model -> EditStatus -> Symbol -> Html Msg
viewObjectiveNew loggedIn edit communityId =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    a
        [ class "border border-dashed border-button-orange mt-6 w-full flex flex-row content-start px-4 py-2"
        , Route.href (Route.NewObjective communityId)
        , disabled (edit /= NoEdit)
        ]
        [ span [ class "px-2 text-orange" ] [ text "+" ]
        , span [ class "text-orange" ] [ text (t "community.objectives.new") ]
        ]



-- VIEW ACTION


viewAction : LoggedIn.Model -> Community.Model -> Maybe Posix -> Community.Action -> Html Msg
viewAction loggedIn metadata maybeDate action =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        text_ s =
            text (t s)

        canEdit =
            LoggedIn.isAccount metadata.creator loggedIn

        posixDeadline : Posix
        posixDeadline =
            action.deadline
                |> Utils.posixDateTime

        deadlineStr : String
        deadlineStr =
            posixDeadline
                |> Strftime.format "%d %B %Y" Time.utc

        pastDeadline : Bool
        pastDeadline =
            case action.deadline of
                Just _ ->
                    case maybeDate of
                        Just today ->
                            posixToMillis today > posixToMillis posixDeadline

                        Nothing ->
                            False

                Nothing ->
                    False

        rewardStrike : String
        rewardStrike =
            if pastDeadline || (action.usagesLeft < 1 && action.usages > 0) then
                " line-through"

            else
                ""

        dateColor : String
        dateColor =
            if pastDeadline then
                " text-red"

            else
                " text-indigo-500"

        usagesColor : String
        usagesColor =
            if action.usagesLeft >= 1 || action.usages == 0 then
                " text-indigo-500"

            else
                " text-red"

        ( claimColors, claimText ) =
            if pastDeadline || (action.usagesLeft < 1 && action.usages > 0) then
                ( " text-grey bg-grey cursor-not-allowed", "dashboard.closed" )

            else
                ( " text-white button button-primary", "dashboard.claim" )

        claimSize =
            if canEdit then
                " w-4/5"

            else
                " w-1/2"

        validatorAvatars =
            List.take 3 action.validators
                |> List.indexedMap
                    (\vIndex v ->
                        let
                            margin =
                                if vIndex /= 0 then
                                    " -ml-5"

                                else
                                    ""
                        in
                        ("h-10 w-10 border-white border-4 rounded-full bg-white" ++ margin)
                            |> Avatar.view v.avatar
                    )
                |> (\vals ->
                        let
                            numValidators =
                                List.length action.validators
                        in
                        if numValidators > 3 then
                            vals
                                ++ [ div
                                        [ class "h-10 w-10 flex flex-col border-white border-4 bg-grey rounded-full -ml-5" ]
                                        [ p [ class "text-date-purple m-auto text-xs font-black leading-none tracking-wide" ]
                                            [ text ("+" ++ String.fromInt (numValidators - 3)) ]
                                        ]
                                   ]

                        else
                            vals
                   )

        rewardStr =
            String.fromFloat action.reward ++ " " ++ Eos.symbolToSymbolCodeString metadata.symbol

        ( usages, usagesLeft ) =
            ( String.fromInt action.usages, String.fromInt action.usagesLeft )

        tr r_id replaces =
            I18Next.tr loggedIn.shared.translations I18Next.Curly r_id replaces

        validationType : String
        validationType =
            action.verificationType
                |> VerificationType.toString

        isClosed =
            pastDeadline || (action.usages > 0 && action.usagesLeft == 0)
    in
    if action.isCompleted then
        text ""

    else
        div [ class "py-6 px-2" ]
            [ div [ class "flex flex-col border-l-8 border-light-grey rounded-l-sm pl-2 sm:pl-6" ]
                [ span [ class "text-text-grey text-sm sm:text-base" ]
                    [ text action.description ]
                , div [ class "flex flex-col sm:flex-row sm:items-center sm:justify-between" ]
                    [ div [ class "text-xs mt-5 sm:w-1/3" ]
                        [ case action.deadline of
                            Just _ ->
                                div []
                                    [ span [ class "capitalize text-text-grey" ] [ text_ "community.actions.available_until" ]
                                    , span [ class dateColor ] [ text deadlineStr ]
                                    , span [] [ text_ "community.actions.or" ]
                                    ]

                            Nothing ->
                                text ""
                        , if action.usages > 0 then
                            p [ class usagesColor ]
                                [ text (tr "community.actions.usages" [ ( "usages", usages ), ( "usagesLeft", usagesLeft ) ]) ]

                          else
                            text ""
                        ]
                    , div [ class "sm:self-end" ]
                        [ div [ class "mt-3 flex flex-row items-center" ]
                            (if validationType == "CLAIMABLE" then
                                validatorAvatars

                             else
                                [ span [ class "text-date-purple uppercase text-sm mr-1" ]
                                    [ text_ "community.actions.automatic_analyzers" ]
                                , img [ src "/icons/tooltip.svg" ] []
                                ]
                            )
                        , div [ class "capitalize text-text-grey text-sm sm:text-right" ]
                            [ text_ "community.actions.verifiers" ]
                        ]
                    ]
                , div [ class "mt-5 flex flex-row items-baseline" ]
                    [ div [ class ("text-green text-base mt-5 flex-grow-1" ++ rewardStrike) ]
                        [ span [] [ text (t "community.actions.reward" ++ ": ") ]
                        , span [ class "font-medium" ] [ text rewardStr ]
                        ]
                    , div [ class "hidden sm:flex sm:visible flex-row justify-end flex-grow-1" ]
                        [ if validationType == "CLAIMABLE" then
                            button
                                [ class ("h-10 uppercase rounded-lg ml-1" ++ claimColors ++ claimSize)
                                , onClick
                                    (if isClosed then
                                        NoOp

                                     else
                                        OpenClaimConfirmation action
                                    )
                                ]
                                [ text_ claimText ]

                          else
                            text ""
                        ]
                    ]
                ]
            , div [ class "flex flex-row mt-8 justify-between sm:hidden" ]
                [ if validationType == "CLAIMABLE" then
                    button
                        [ class ("h-10 uppercase rounded-lg ml-1" ++ claimColors ++ claimSize)
                        , onClick (OpenClaimConfirmation action)
                        ]
                        [ text_ claimText ]

                  else
                    text ""
                ]
            ]


viewClaimConfirmation : LoggedIn.Model -> Model -> Html Msg
viewClaimConfirmation loggedIn model =
    let
        t s =
            I18Next.t loggedIn.shared.translations
                s

        text_ s =
            text (t s)

        acceptButtonText =
            t "dashboard.check_claim.yes"

        modalContent acceptMsg isInProgress =
            div []
                [ Modal.initWith
                    { closeMsg = CloseClaimConfirmation
                    , isVisible = True
                    }
                    |> Modal.withHeader (t "claim.modal.title")
                    |> Modal.withBody [ text_ "dashboard.check_claim.body" ]
                    |> Modal.withFooter
                        [ button
                            [ class "modal-cancel"
                            , classList [ ( "button-disabled", isInProgress ) ]
                            , onClick
                                (if isInProgress then
                                    NoOp

                                 else
                                    CloseClaimConfirmation
                                )
                            , disabled isInProgress
                            ]
                            [ text_ "dashboard.check_claim.no" ]
                        , button
                            [ class "modal-accept"
                            , classList [ ( "button-disabled", isInProgress ) ]
                            , onClick
                                (if isInProgress then
                                    NoOp

                                 else
                                    acceptMsg
                                )
                            , disabled isInProgress
                            ]
                            [ text acceptButtonText
                            ]
                        ]
                    |> Modal.toHtml
                ]
    in
    case model.claimConfirmationModalStatus of
        Open action ->
            let
                acceptMsg =
                    if action.hasProofPhoto then
                        OpenProofSection action

                    else
                        ClaimAction action
            in
            modalContent acceptMsg False

        InProgress ->
            modalContent NoOp True

        Closed ->
            text ""


viewPhotoUploader : Translators -> ProofPhotoStatus -> Html Msg
viewPhotoUploader { t } proofPhotoStatus =
    let
        uploadedAttrs =
            case proofPhotoStatus of
                Uploaded url ->
                    [ class " bg-no-repeat bg-center bg-cover"
                    , style "background-image" ("url(" ++ url ++ ")")
                    ]

                _ ->
                    []
    in
    label
        (class "relative bg-purple-500 w-full md:w-2/3 h-56 rounded-sm flex justify-center items-center cursor-pointer"
            :: uploadedAttrs
        )
        [ input
            [ class "hidden-img-input"
            , type_ "file"
            , accept "image/*"
            , Page.onFileChange EnteredPhoto
            , multiple False
            ]
            []
        , div []
            [ case proofPhotoStatus of
                Uploading ->
                    div [ class "spinner spinner-light" ] []

                Uploaded _ ->
                    span [ class "absolute bottom-0 right-0 mr-4 mb-4 bg-orange-300 w-8 h-8 p-2 rounded-full" ]
                        [ Icons.camera ]

                _ ->
                    div [ class "text-white text-body font-bold text-center" ]
                        [ div [ class "w-10 mx-auto mb-2" ] [ Icons.camera ]
                        , div [] [ text (t "community.actions.proof.upload_photo_hint") ]
                        ]
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = NoOp
    | GotTime Posix
    | CompletedLoadCommunity (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
      -- Objective
    | ClickedOpenObjective Int
    | ClickedCloseObjective
      -- Action
    | OpenClaimConfirmation Community.Action
    | CloseClaimConfirmation
    | ClaimAction Community.Action
    | GotClaimActionResponse (Result Value String)
      -- Proofs
    | OpenProofSection Community.Action
    | CloseProofSection ReasonToCloseProofSection
    | GotProofTime Int Posix
    | GetUnit64Name String
    | GotUnit64Name (Result Value String)
    | Tick Time.Posix
    | EnteredPhoto (List File)
    | CompletedPhotoUpload (Result Http.Error String)


type ReasonToCloseProofSection
    = CancelClicked
    | TimerExpired


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            I18Next.t loggedIn.shared.translations
    in
    case msg of
        NoOp ->
            UR.init model

        GotTime date ->
            UR.init { model | date = Just date }

        GetUnit64Name _ ->
            model |> UR.init

        GotUnit64Name (Ok unit64name) ->
            case ( model.proofs, model.actionId ) of
                ( Just (Proof proofPhoto (CodeParts proofCode)), Just actionId ) ->
                    let
                        verificationCode =
                            Claim.generateVerificationCode actionId unit64name proofCode.claimTimestamp

                        newProofCode =
                            CodeParts
                                { proofCode
                                    | code = Just verificationCode
                                }
                    in
                    { model | proofs = Just (Proof proofPhoto newProofCode) }
                        |> UR.init

                _ ->
                    model
                        |> UR.init

        GotUnit64Name (Err _) ->
            UR.init model

        Tick timer ->
            case model.proofs of
                Just (Proof proofPhoto (CodeParts proofCode)) ->
                    let
                        secondsAfterClaim =
                            (Time.posixToMillis timer // 1000) - proofCode.claimTimestamp

                        isProofCodeActive =
                            (proofCode.availabilityPeriod - secondsAfterClaim) > 0
                    in
                    if isProofCodeActive then
                        let
                            newProofCode =
                                CodeParts
                                    { proofCode | secondsAfterClaim = secondsAfterClaim }
                        in
                        { model | proofs = Just (Proof proofPhoto newProofCode) }
                            |> UR.init

                    else
                        update (CloseProofSection TimerExpired) model loggedIn

                _ ->
                    model |> UR.init

        GotProofTime actionId posix ->
            let
                initProofCodeParts =
                    CodeParts
                        { code = Nothing
                        , claimTimestamp = Time.posixToMillis posix // 1000
                        , secondsAfterClaim = 0
                        , availabilityPeriod = 30 * 60
                        }
            in
            { model
                | actionId = Just actionId
                , proofs = Just (Proof NoPhotoAdded initProofCodeParts)
            }
                |> UR.init
                |> UR.addPort
                    { responseAddress = GetUnit64Name (Eos.nameToString loggedIn.accountName)
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "accountNameToUnit64" )
                            , ( "accountName", Encode.string (Eos.nameToString loggedIn.accountName) )
                            ]
                    }

        CompletedLoadCommunity (Ok community) ->
            case community of
                Just c ->
                    { model
                        | pageStatus = Loaded c (Objectives NoEdit)
                    }
                        |> UR.init

                Nothing ->
                    { model | pageStatus = NotFound }
                        |> UR.init

        CompletedLoadCommunity (Err err) ->
            { model | pageStatus = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        ClickedOpenObjective index ->
            { model | openObjective = Just index }
                |> UR.init

        ClickedCloseObjective ->
            { model | openObjective = Nothing }
                |> UR.init

        OpenClaimConfirmation action ->
            { model | claimConfirmationModalStatus = Open action }
                |> UR.init

        OpenProofSection action ->
            let
                runProofCodeTimer =
                    Task.perform (GotProofTime action.id) Time.now
            in
            if action.hasProofPhoto then
                { model
                    | pageStatus =
                        case model.pageStatus of
                            Loaded community _ ->
                                Loaded community (ClaimWithProofs action)

                            _ ->
                                model.pageStatus
                    , claimConfirmationModalStatus = Closed
                    , proofs = Just (Proof NoPhotoAdded NoCodeRequired)
                }
                    |> UR.init
                    |> UR.addCmd
                        (if action.hasProofCode then
                            runProofCodeTimer

                         else
                            Cmd.none
                        )
                    |> UR.addPort
                        { responseAddress = NoOp
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "id", Encode.string "communityPage" )
                                , ( "name", Encode.string "scrollIntoView" )
                                ]
                        }

            else
                model
                    |> UR.init

        EnteredPhoto (file :: _) ->
            let
                uploadImage =
                    Api.uploadImage loggedIn.shared file CompletedPhotoUpload

                newProofs =
                    case model.proofs of
                        Just (Proof _ proofCode) ->
                            Just (Proof Uploading proofCode)

                        _ ->
                            Nothing
            in
            { model
                | proofs = newProofs
            }
                |> UR.init
                |> UR.addCmd uploadImage

        EnteredPhoto [] ->
            UR.init model

        CompletedPhotoUpload (Ok url) ->
            let
                newProofs =
                    case model.proofs of
                        Just (Proof _ proofCode) ->
                            Just (Proof (Uploaded url) proofCode)

                        _ ->
                            Nothing
            in
            { model | proofs = newProofs }
                |> UR.init

        CompletedPhotoUpload (Err error) ->
            let
                newProofs =
                    case model.proofs of
                        Just (Proof _ proofCode) ->
                            Just (Proof (UploadFailed error) proofCode)

                        _ ->
                            Nothing
            in
            { model | proofs = newProofs }
                |> UR.init
                |> UR.logHttpError msg error

        CloseClaimConfirmation ->
            { model | claimConfirmationModalStatus = Closed }
                |> UR.init

        CloseProofSection reason ->
            { model
                | pageStatus =
                    case model.pageStatus of
                        Loaded community (ClaimWithProofs _) ->
                            Loaded community (Objectives NoEdit)

                        _ ->
                            model.pageStatus
                , claimConfirmationModalStatus = Closed
                , proofs = Nothing
            }
                |> UR.init
                |> UR.addExt
                    (case reason of
                        TimerExpired ->
                            ShowFeedback LoggedIn.Failure "Time for claiming is expired"

                        CancelClicked ->
                            HideFeedback
                    )

        ClaimAction action ->
            let
                newModel =
                    case model.proofs of
                        Just (Proof _ _) ->
                            -- Claim with proof dialog has no confirmation
                            model

                        Nothing ->
                            { model | claimConfirmationModalStatus = InProgress }

                ( proofPhotoUrl, proofCode_, proofTime ) =
                    case model.proofs of
                        Just (Proof (Uploaded url) (CodeParts { code, claimTimestamp })) ->
                            ( url, Maybe.withDefault "" code, claimTimestamp )

                        Just (Proof (Uploaded url) NoCodeRequired) ->
                            ( url, "", 0 )

                        _ ->
                            ( "", "", 0 )
            in
            if LoggedIn.isAuth loggedIn then
                newModel
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = ClaimAction action
                        , responseData = Encode.null
                        , data =
                            Eos.encodeTransaction
                                [ { accountName = loggedIn.shared.contracts.community
                                  , name = "claimaction"
                                  , authorization =
                                        { actor = loggedIn.accountName
                                        , permissionName = Eos.samplePermission
                                        }
                                  , data =
                                        { actionId = action.id
                                        , maker = loggedIn.accountName
                                        , proofPhoto = proofPhotoUrl
                                        , proofCode = proofCode_
                                        , proofTime = proofTime
                                        }
                                            |> Claim.encodeClaimAction
                                  }
                                ]
                        }

            else
                newModel
                    |> UR.init
                    |> UR.addExt (Just (ClaimAction action) |> RequiredAuthentication)

        GotClaimActionResponse (Ok _) ->
            { model
                | claimConfirmationModalStatus = Closed
                , pageStatus =
                    case model.pageStatus of
                        Loaded community (ClaimWithProofs _) ->
                            Loaded community (Objectives NoEdit)

                        _ ->
                            model.pageStatus
                , proofs = Nothing
            }
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Success (t "dashboard.check_claim.success"))

        GotClaimActionResponse (Err _) ->
            { model
                | claimConfirmationModalStatus = Closed
            }
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Failure (t "dashboard.check_claim.failure"))


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClaimAction" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string |> Decode.map Ok
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotClaimActionResponse)
                |> Result.withDefault Nothing

        "GetUnit64Name" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "unit64name" Decode.string |> Decode.map Ok
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotUnit64Name)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        GotTime _ ->
            [ "GotTime" ]

        Tick _ ->
            [ "Tick" ]

        GotProofTime _ _ ->
            [ "GotProofTime" ]

        CompletedLoadCommunity r ->
            [ "CompletedLoadCommunity", UR.resultToString r ]

        ClickedOpenObjective _ ->
            [ "ClickedOpenObjective" ]

        ClickedCloseObjective ->
            [ "ClickedCloseObjective" ]

        OpenProofSection _ ->
            [ "OpenAddPhotoProof" ]

        CloseProofSection _ ->
            [ "CloseAddPhotoProof" ]

        EnteredPhoto _ ->
            [ "EnteredPhoto" ]

        CompletedPhotoUpload r ->
            [ "CompletedPhotoUpload", UR.resultToString r ]

        OpenClaimConfirmation _ ->
            [ "OpenClaimConfirmation" ]

        CloseClaimConfirmation ->
            [ "CloseClaimConfirmation" ]

        ClaimAction _ ->
            [ "ClaimAction" ]

        GetUnit64Name _ ->
            [ "GetUnit64Name" ]

        GotUnit64Name n ->
            [ "GotClaimActionResponse", UR.resultToString n ]

        GotClaimActionResponse r ->
            [ "GotClaimActionResponse", UR.resultToString r ]
