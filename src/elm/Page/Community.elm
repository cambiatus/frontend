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
import Cambiatus.Scalar exposing (DateTime(..))
import Claim
import Community exposing (Action, Model)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, button, div, hr, img, input, label, p, span, text)
import Html.Attributes exposing (accept, class, classList, disabled, multiple, src, type_)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (t)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Session.Shared exposing (Shared)
import Sha256 exposing (sha256)
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.proofTime, model.secondsAfterClaim ) of
        ( Just _, Just _ ) ->
            Time.every 1000 Tick

        _ ->
            Sub.none



-- MODEL


type alias Model =
    { date : Maybe Posix
    , community : LoadStatus
    , actionId : Maybe Int
    , members : List Member
    , openObjective : Maybe Int
    , modalStatus : ModalStatus
    , addPhotoStatus : AddPhotoStatus
    , proofPhoto : Maybe String
    , proofPhotoStatus : ImageStatus
    , proofCode : Maybe String
    , proofTime : Maybe Int
    , unit64name : Maybe String
    , secondsAfterClaim : Maybe Int
    , proofCodeValiditySeconds : Int
    , invitations : String
    , symbol : Symbol
    }


type AddPhotoStatus
    = AddPhotoOpen Community.Action
    | AddPhotoClosed


initModel : LoggedIn.Model -> Symbol -> Model
initModel _ symbol =
    { date = Nothing
    , community = Loading
    , members = []
    , actionId = Nothing
    , openObjective = Nothing
    , modalStatus = Closed
    , addPhotoStatus = AddPhotoClosed
    , proofPhoto = Nothing
    , proofPhotoStatus = NoImage
    , proofTime = Nothing
    , proofCode = Nothing
    , unit64name = Nothing
    , secondsAfterClaim = Nothing
    , proofCodeValiditySeconds = 30 * 60
    , invitations = ""
    , symbol = symbol
    }


type LoadStatus
    = Loading
    | Loaded Community.Model EditStatus
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Community.Model))


type EditStatus
    = NoEdit


type
    ModalStatus
    -- TODO: Use `Loading` and `Active` instead of Bool
    = Open Bool Community.Action
    | Closed


type alias Member =
    { id : String
    , accountName : String
    , nameWithAt : String
    }


type ImageStatus
    = NoImage
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
            case model.community of
                Loaded community _ ->
                    community.title

                Loading ->
                    t ""

                _ ->
                    t "community.not_found"

        content =
            case model.community of
                Loading ->
                    Page.fullPageLoading

                NotFound ->
                    Page.viewCardEmpty [ text_ "community.not_found" ]

                Failed e ->
                    Page.fullPageGraphQLError (t "community.objectives.title") e

                Loaded community editStatus ->
                    let
                        canEdit =
                            LoggedIn.isAccount community.creator loggedIn
                    in
                    div []
                        [ case model.addPhotoStatus of
                            AddPhotoOpen action ->
                                viewAddPhoto model loggedIn action

                            AddPhotoClosed ->
                                div []
                                    [ viewHeader loggedIn community
                                    , div [ class "bg-white p-20" ]
                                        [ div [ class "flex flex-wrap w-full items-center" ]
                                            [ p [ class "text-4xl font-bold" ]
                                                [ text community.title ]
                                            ]
                                        , p [ class "text-grey-200 text-sm" ] [ text community.description ]
                                        ]
                                    , if community.hasObjectives then
                                        div [ class "container mx-auto px-4" ]
                                            [ viewClaimModal loggedIn model
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
                        ]
    in
    { title = title
    , content = content
    }


viewAddPhoto : Model -> LoggedIn.Model -> Action -> Html Msg
viewAddPhoto model { accountName, shared } action =
    let
        { t } =
            shared.translators
    in
    div [ class "bg-white border-t border-gray-300" ]
        [ div [ class "container p-4 mx-auto" ]
            [ Page.viewTitle (t "community.actions.proof.title")
            , p [ class "mb-4" ]
                [ text <|
                    Maybe.withDefault "" action.photoProofInstructions
                ]
            , div [ class "mb-4" ]
                [ span [ class "input-label block" ]
                    [ text (t "community.actions.form.verification_number") ]
                , viewProofCode model
                ]
            , div [ class "mb-4" ]
                [ span [ class "input-label block" ]
                    [ text (t "community.actions.proof.photo") ]
                , viewPhotoUploader shared model
                ]
            , div [ class "md:flex" ]
                [ button
                    [ class "modal-cancel"
                    , onClick (CloseAddPhotoProof CancelClicked)
                    ]
                    [ text (t "menu.cancel") ]
                , button
                    [ class "modal-accept"
                    , onClick (ClaimAction action)
                    ]
                    [ text (t "menu.send") ]
                ]
            ]
        ]


viewProofCode model =
    let
        secondsPassed =
            Maybe.withDefault 0 model.secondsAfterClaim

        remainingSeconds =
            model.proofCodeValiditySeconds - secondsPassed

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
    p []
        [ div [ class "text-xl font-bold sm:inline" ]
            [ text <| Maybe.withDefault "No code found" model.proofCode ]
        , span [ class "whitespace-no-wrap text-body rounded-full bg-lightred px-2 py-1 sm:ml-2 text-white" ]
            [ text "the code is valid for"
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


viewClaimModal : LoggedIn.Model -> Model -> Html Msg
viewClaimModal loggedIn model =
    case model.modalStatus of
        Open isLoading action ->
            let
                hasProofPhoto =
                    Maybe.withDefault False action.hasProofPhoto

                t s =
                    I18Next.t loggedIn.shared.translations
                        s

                text_ s =
                    text (t s)

                acceptMsg =
                    case ( isLoading, hasProofPhoto ) of
                        ( True, _ ) ->
                            NoOp

                        ( False, False ) ->
                            ClaimAction action

                        ( False, True ) ->
                            OpenAddPhotoProof action

                acceptButtonText =
                    t "dashboard.check_claim.yes"
                        ++ (if hasProofPhoto then
                                -- Conventionally, the three dots mean that there will be an extra step (photo uploading)
                                "..."

                            else
                                ""
                           )
            in
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
                            , if not isLoading then
                                onClick CloseClaimConfirmation

                              else
                                onClick NoOp
                            , disabled isLoading
                            ]
                            [ text_ "dashboard.check_claim.no" ]
                        , button
                            [ class "modal-accept"
                            , onClick acceptMsg
                            , disabled isLoading
                            ]
                            [ text acceptButtonText
                            ]
                        ]
                    |> Modal.toHtml
                ]

        Closed ->
            text ""


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
        , div [ class "h-24 lg:h-56 bg-indigo-500 flex flex-wrap content-end" ]
            [ div [ class "h-24 w-24 rounded-full mx-auto pt-12" ]
                [ img
                    [ src community.logo
                    , class "object-scale-down"
                    ]
                    []
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
    | OpenAddPhotoProof Community.Action
    | CloseAddPhotoProof ReasonToClosePhotoProof
    | GotProofTime Int Posix
    | GotUnit64Name (Result Value String)
    | Tick Time.Posix
    | EnteredPhoto (List File)
    | CompletedPhotoUpload (Result Http.Error String)
    | ClaimAction Community.Action
    | GetUnit64Name String
    | GotClaimActionResponse (Result Value String)


type ReasonToClosePhotoProof
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
            let
                proofCode =
                    String.fromInt (Maybe.withDefault 0 model.actionId)
                        ++ unit64name
                        ++ String.fromInt (Maybe.withDefault 0 model.proofTime)
                        |> sha256
                        |> String.slice 0 8
            in
            UR.init
                { model
                    | proofCode = Just proofCode
                }

        GotUnit64Name (Err _) ->
            UR.init model

        Tick timer ->
            case model.proofTime of
                Just proofTime ->
                    let
                        secondsAfterClaim =
                            (Time.posixToMillis timer // 1000) - proofTime

                        isProofCodeActive =
                            (model.proofCodeValiditySeconds - secondsAfterClaim) > 0
                    in
                    if isProofCodeActive then
                        { model | secondsAfterClaim = Just secondsAfterClaim }
                            |> UR.init

                    else
                        update (CloseAddPhotoProof TimerExpired) model loggedIn

                Nothing ->
                    model |> UR.init

        GotProofTime actionId posix ->
            let
                proofTime =
                    -- Timestamp in seconds
                    Time.posixToMillis posix // 1000
            in
            UR.init
                { model
                    | actionId = Just actionId
                    , proofTime = Just proofTime
                    , secondsAfterClaim = Just 0
                }
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
                    UR.init { model | community = Loaded c NoEdit }

                Nothing ->
                    UR.init { model | community = NotFound }

        CompletedLoadCommunity (Err err) ->
            { model | community = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        ClickedOpenObjective index ->
            { model | openObjective = Just index }
                |> UR.init

        ClickedCloseObjective ->
            { model | openObjective = Nothing }
                |> UR.init

        OpenClaimConfirmation action ->
            { model | modalStatus = Open False action }
                |> UR.init

        OpenAddPhotoProof action ->
            case action.hasProofPhoto of
                Just True ->
                    { model
                        | addPhotoStatus = AddPhotoOpen action
                        , modalStatus = Closed
                    }
                        |> UR.init
                        |> UR.addCmd (Task.perform (GotProofTime action.id) Time.now)

                _ ->
                    { model
                        | modalStatus = Open False action
                        , addPhotoStatus = AddPhotoClosed
                    }
                        |> UR.init

        EnteredPhoto (file :: _) ->
            let
                uploadImage =
                    Api.uploadImage loggedIn.shared file CompletedPhotoUpload
            in
            { model | proofPhotoStatus = Uploading }
                |> UR.init
                |> UR.addCmd uploadImage

        EnteredPhoto [] ->
            UR.init model

        CompletedPhotoUpload (Ok url) ->
            { model
                | proofPhotoStatus = Uploaded url
            }
                |> UR.init

        CompletedPhotoUpload (Err error) ->
            model
                |> UR.init
                |> UR.logHttpError msg error

        CloseClaimConfirmation ->
            { model | modalStatus = Closed }
                |> UR.init

        CloseAddPhotoProof reason ->
            { model
                | addPhotoStatus = AddPhotoClosed
                , proofTime = Nothing
                , proofCode = Nothing
                , secondsAfterClaim = Nothing
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
                    { model | modalStatus = Open True action }

                proofCode =
                    Maybe.withDefault "" model.proofCode

                proofTime =
                    Maybe.withDefault 0 model.proofTime

                proofPhoto =
                    case action.hasProofPhoto of
                        Just True ->
                            case model.proofPhotoStatus of
                                Uploaded url ->
                                    url

                                _ ->
                                    ""

                        _ ->
                            ""
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
                                        , proofPhoto = proofPhoto
                                        , proofCode = proofCode
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
              -- TODO: Make it better
                | modalStatus = Closed
                , addPhotoStatus = AddPhotoClosed
                , proofCode = Nothing
                , proofPhotoStatus = NoImage
                , proofPhoto = Nothing
                , proofTime = Nothing
            }
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Success (t "dashboard.check_claim.success"))

        GotClaimActionResponse (Err _) ->
            { model
                | modalStatus = Closed
            }
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Failure (t "dashboard.check_claim.failure"))


viewPhotoUploader : Shared -> Model -> Html Msg
viewPhotoUploader shared model =
    let
        { t } =
            shared.translators
    in
    label
        [ class "relative bg-purple-500 w-full md:w-2/3 h-56 rounded-sm flex justify-center items-center cursor-pointer" ]
        [ input
            [ class "hidden-img-input"
            , type_ "file"
            , accept "image/*"
            , Page.onFileChange EnteredPhoto
            , multiple False
            ]
            []
        , div []
            [ case model.proofPhotoStatus of
                Uploading ->
                    div [ class "spinner spinner-light" ] []

                Uploaded url ->
                    div []
                        [ img [ src url ] []
                        , span [ class "absolute bottom-0 right-0 mr-4 mb-4 bg-orange-300 w-8 h-8 p-2 rounded-full" ]
                            [ Icons.camera ]
                        ]

                _ ->
                    div [ class "w-10" ] [ Icons.camera ]
            ]

        -- TODO: Show orange icon only if photo is uploaded
        ]


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

        OpenAddPhotoProof _ ->
            [ "OpenAddPhotoProof" ]

        CloseAddPhotoProof _ ->
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
