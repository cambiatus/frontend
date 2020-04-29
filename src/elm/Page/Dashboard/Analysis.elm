module Page.Dashboard.Analysis exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , update
    , view
    )

import Api.Graphql
import Cambiatus.Query
import Claim
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, button, div, p, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import I18Next
import Icons
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Page
import Profile
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Shared)
import Strftime
import Time
import UpdateResult as UR
import Utils


init : LoggedIn.Model -> ( Model, Cmd Msg )
init { shared, accountName, selectedCommunity } =
    ( initModel
    , fetchAnalysis shared selectedCommunity accountName
    )



-- MODEL


type alias Model =
    { status : Status
    , modalStatus : ModalStatus
    }


initModel : Model
initModel =
    { status = Loading
    , modalStatus = ModalClosed
    }


type Status
    = Loading
    | Loaded Filter (List Claim.Model)
    | Failed


type Filter
    = All
    | Approved
    | Disapproved
    | UnderReview
    | UnderReviewPending


type ModalStatus
    = ModalClosed
    | ModalLoading Int Bool
    | ModalOpened Int Bool



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view ({ shared } as loggedIn) model =
    let
        t : String -> String
        t =
            I18Next.t shared.translations
    in
    case model.status of
        Loading ->
            Page.fullPageLoading

        Loaded filter claims ->
            div []
                [ Page.viewHeader loggedIn (t "dashboard.all_analysis.title") Route.Dashboard
                , div [ class "container mx-auto px-4 mb-10" ]
                    [ text "filtro de conta do claimer"
                    , text "filtro do estado do claim"
                    , div [ class "flex flex-wrap -mx-2" ]
                        (List.map (viewClaim loggedIn filter) claims)
                    ]
                , viewAnalysisModal loggedIn model
                ]

        Failed ->
            text ""


viewClaim : LoggedIn.Model -> Filter -> Claim.Model -> Html Msg
viewClaim ({ shared, accountName, selectedCommunity } as loggedIn) filter claim =
    let
        t =
            I18Next.t shared.translations

        text_ s =
            text (I18Next.t shared.translations s)

        date dateTime =
            Just dateTime
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc

        ( msg, textColor ) =
            if claim.isVerified then
                ( t "dashboard.all_analysis.approved", "text-green" )

            else if Claim.isAlreadyValidated claim accountName then
                ( t "dashboard.all_analysis.pending", "text-black" )

            else
                ( t "dashboard.all_analysis.disapproved", "text-red" )
    in
    div [ class "w-full sm:w-full md:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-4" ]
        [ if Claim.isAlreadyValidated claim accountName then
            div [ class " flex flex-col p-4 my-2 rounded-lg bg-white" ]
                [ div [ class "flex justify-center mb-8" ]
                    [ Profile.view shared accountName claim.claimer
                    ]
                , div [ class "mb-6" ]
                    [ div
                        [ class "bg-gray-100 flex items-center justify-center h-6 w-32" ]
                        [ p
                            [ class ("font-sans text-caption uppercase " ++ textColor) ]
                            [ text msg ]
                        ]
                    , p [ class "text-body" ]
                        [ text claim.action.description ]
                    , p
                        [ class "text-gray-900 text-caption uppercase" ]
                        [ text <| date claim.createdAt ]
                    ]
                , a
                    [ class "button button-secondary w-full font-medium mb-2"
                    , Route.href <| Route.Claim selectedCommunity claim.action.objective.id claim.action.id claim.id
                    ]
                    [ text_ "dashboard.all_analysis.more_details" ]
                ]

          else
            div [ class " flex flex-col p-4 my-2 rounded-lg bg-white", id <| "claim-" ++ String.fromInt claim.id ]
                [ div [ class "flex justify-start mb-8" ]
                    [ Profile.view shared accountName claim.claimer
                    ]
                , div [ class "mb-6" ]
                    [ p [ class "text-body" ]
                        [ text claim.action.description ]
                    , p
                        [ class "text-gray-900 text-caption uppercase" ]
                        [ text <| date claim.createdAt ]
                    ]
                , div [ class "flex" ]
                    [ button
                        [ class "flex-1 button button-secondary font-medium text-red"
                        , onClick (OpenModal claim.id False)
                        ]
                        [ text_ "dashboard.reject" ]
                    , div [ class "w-4" ] []
                    , button
                        [ class "flex-1 button button-primary font-medium"
                        , onClick (OpenModal claim.id True)
                        ]
                        [ text_ "dashboard.verify" ]
                    ]
                ]
        ]


viewAnalysisModal : LoggedIn.Model -> Model -> Html Msg
viewAnalysisModal loggedIn model =
    case model.modalStatus of
        ModalOpened claimId vote ->
            let
                t s =
                    I18Next.t loggedIn.shared.translations s

                text_ s =
                    text (t s)
            in
            div [ class "modal container" ]
                [ div [ class "modal-bg", onClick CloseModal ] []
                , div [ class "modal-content" ]
                    [ div [ class "w-full" ]
                        [ p [ class "w-full font-bold text-heading text-2xl mb-4" ]
                            [ text_ "claim.modal.title" ]
                        , button
                            [ onClick CloseModal ]
                            [ Icons.close "absolute fill-current text-gray-400 top-0 right-0 mx-4 my-4"
                            ]
                        , p [ class "text-body w-full font-sans mb-10" ]
                            [ if vote then
                                text_ "claim.modal.message_approve"

                              else
                                text_ "claim.modal.message_disapprove"
                            ]
                        ]
                    , div [ class "modal-footer" ]
                        [ button [ class "modal-cancel", onClick CloseModal ]
                            [ text_ "claim.modal.secondary" ]
                        , button [ class "modal-accept", onClick (VoteClaim claimId vote) ]
                            [ if vote then
                                text_ "claim.modal.primary_approve"

                              else
                                text_ "claim.modal.primary_disapprove"
                            ]
                        ]
                    ]
                ]

        ModalLoading _ _ ->
            Page.fullPageLoading

        ModalClosed ->
            text ""



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = ClaimsLoaded (Result (Graphql.Http.Error (List Claim.Model)) (List Claim.Model))
    | OpenModal Int Bool
    | CloseModal
    | VoteClaim Int Bool
    | GotVoteResult Int (Result Decode.Value String)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        ClaimsLoaded (Ok results) ->
            { model | status = Loaded All results } |> UR.init

        ClaimsLoaded (Err _) ->
            { model | status = Failed } |> UR.init

        OpenModal claimId vote ->
            { model | modalStatus = ModalOpened claimId vote } |> UR.init

        CloseModal ->
            { model | modalStatus = ModalClosed } |> UR.init

        VoteClaim claimId vote ->
            case model.status of
                Loaded _ _ ->
                    let
                        newModel =
                            { model
                                | modalStatus = ModalLoading claimId vote
                            }
                    in
                    if LoggedIn.isAuth loggedIn then
                        UR.init newModel
                            |> UR.addPort
                                { responseAddress = msg
                                , responseData = Encode.null
                                , data =
                                    Eos.encodeTransaction
                                        { actions =
                                            [ { accountName = "bes.cmm"
                                              , name = "verifyclaim"
                                              , authorization =
                                                    { actor = loggedIn.accountName
                                                    , permissionName = Eos.samplePermission
                                                    }
                                              , data = Claim.encodeVerification claimId loggedIn.accountName vote
                                              }
                                            ]
                                        }
                                }

                    else
                        UR.init newModel
                            |> UR.addExt (Just (VoteClaim claimId vote) |> LoggedIn.RequiredAuthentication)

                _ ->
                    model
                        |> UR.init

        GotVoteResult claimId (Ok _) ->
            case model.status of
                Loaded filter claims ->
                    let
                        maybeClaim : Maybe Claim.Model
                        maybeClaim =
                            List.find (\c -> c.id == claimId) claims

                        message val =
                            [ ( "value", val ) ]
                                |> I18Next.tr loggedIn.shared.translations I18Next.Curly "claim.reward"
                    in
                    case maybeClaim of
                        Just claim ->
                            let
                                value =
                                    String.fromFloat claim.action.verifierReward
                                        ++ " "
                                        ++ Eos.symbolToString claim.action.objective.community.symbol
                            in
                            { model
                                | status = Loaded filter claims
                            }
                                |> UR.init
                                |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Success (message value))
                                |> UR.addCmd
                                    (Route.Analysis
                                        |> Route.replaceUrl loggedIn.shared.navKey
                                    )

                        Nothing ->
                            model
                                |> UR.init

                _ ->
                    model |> UR.init

        GotVoteResult _ (Err _) ->
            case model.status of
                Loaded filter claims ->
                    { model | status = Loaded filter claims }
                        |> UR.init

                _ ->
                    model |> UR.init


fetchAnalysis : Shared -> Symbol -> Eos.Name -> Cmd Msg
fetchAnalysis shared communityId account =
    let
        arg =
            { claimer = Absent
            , symbol = Present (Eos.symbolToString communityId)
            , validator = Present (Eos.nameToString account)
            , all = Present True
            }
    in
    Api.Graphql.query
        shared
        (Cambiatus.Query.claims { input = arg } Claim.selectionSet)
        ClaimsLoaded


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "VoteClaim" :: claimId :: _ ->
            let
                id =
                    String.toInt claimId
                        |> Maybe.withDefault 0
            in
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.succeed (Err Encode.null)
                    ]
                )
                val
                |> Result.map (Just << GotVoteResult id)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ClaimsLoaded r ->
            [ "ChecksLoaded", UR.resultToString r ]

        OpenModal _ _ ->
            [ "OpenModal" ]

        CloseModal ->
            [ "CloseModal" ]

        VoteClaim claimId _ ->
            [ "VoteClaim", String.fromInt claimId ]

        GotVoteResult _ r ->
            [ "GotVoteResult", UR.resultToString r ]
