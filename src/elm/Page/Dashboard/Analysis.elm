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
import Api.Relay
import Cambiatus.Query
import Claim
import Community
import Eos
import Eos.Account as Eos
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, button, div, option, p, select, span, text)
import Html.Attributes exposing (class, id, selected, value)
import Html.Events exposing (onClick)
import I18Next
import Icons
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Page
import Profile exposing (Profile)
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import Strftime
import Time
import UpdateResult as UR
import Utils


init : LoggedIn.Model -> ( Model, Cmd Msg )
init ({ shared, selectedCommunity } as loggedIn) =
    let
        newModel =
            initModel
    in
    ( newModel
    , Cmd.batch
        [ fetchAnalysis loggedIn newModel.filters Nothing
        , Api.Graphql.query shared (Community.communityQuery selectedCommunity) CompletedCommunityLoad
        ]
    )



-- MODEL


type alias Model =
    { status : Status
    , communityStatus : CommunityStatus
    , modalStatus : ModalStatus
    , autoCompleteState : Select.State
    , reloadOnNextQuery : Bool
    , filters : Filter
    }


initModel : Model
initModel =
    { status = Loading
    , communityStatus = LoadingCommunity
    , modalStatus = ModalClosed
    , autoCompleteState = Select.newState ""
    , reloadOnNextQuery = False
    , filters = { profile = Nothing, statusFilter = All }
    }


type Status
    = Loading
    | Loaded (List Claim.Model) (Maybe Api.Relay.PageInfo)
    | Failed


type CommunityStatus
    = LoadingCommunity
    | LoadedCommunity Community.Model
    | FailedCommunity


type alias Filter =
    { profile : Maybe Profile
    , statusFilter : StatusFilter
    }


type StatusFilter
    = All
    | Approved
    | Rejected
    | Pending


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

        Loaded claims pageInfo ->
            div []
                [ Page.viewHeader loggedIn (t "all_analysis.title") Route.Dashboard
                , div [ class "container mx-auto px-4 mb-10" ]
                    [ viewFilters loggedIn model
                    , if List.length claims > 0 then
                        div []
                            [ div [ class "flex flex-wrap -mx-2" ] (List.map (viewClaim loggedIn) claims)
                            , viewPagination loggedIn pageInfo
                            ]

                      else
                        viewEmptyResults
                    ]
                , viewAnalysisModal loggedIn model
                ]

        Failed ->
            text ""


viewFilters : LoggedIn.Model -> Model -> Html Msg
viewFilters ({ shared } as loggedIn) model =
    let
        t : String -> String
        t =
            I18Next.t shared.translations

        text_ s =
            text (t s)
    in
    div [ class "mt-4 mb-12" ]
        [ div []
            [ span [ class "input-label" ]
                [ text_ "all_analysis.filter.user" ]
            , case model.communityStatus of
                LoadedCommunity community ->
                    let
                        selectedUsers =
                            Maybe.map (\v -> [ v ]) model.filters.profile
                                |> Maybe.withDefault []
                    in
                    div []
                        [ Html.map SelectMsg
                            (Select.view
                                (selectConfiguration shared False)
                                model.autoCompleteState
                                community.members
                                selectedUsers
                            )
                        , viewSelectedVerifiers loggedIn selectedUsers
                        ]

                _ ->
                    text ""
            ]
        , div [ class "mt-6" ]
            [ span [ class "input-label" ] [ text_ "all_analysis.filter.status.label" ]
            , select
                [ class "input w-full mb-2 border form-select border-gray-500 rounded-sm"
                , Html.Events.on "change"
                    (Decode.map
                        (\val ->
                            case val of
                                "approved" ->
                                    SelectStatusFilter Approved

                                "rejected" ->
                                    SelectStatusFilter Rejected

                                "pending" ->
                                    SelectStatusFilter Pending

                                _ ->
                                    SelectStatusFilter All
                        )
                        Html.Events.targetValue
                    )
                ]
                [ option
                    [ value ""
                    , selected (model.filters.statusFilter == All)
                    ]
                    [ text_ "all_analysis.all" ]
                , option
                    [ value "approved"
                    , selected (model.filters.statusFilter == Approved)
                    ]
                    [ text_ "all_analysis.approved" ]
                , option
                    [ value "rejected"
                    , selected (model.filters.statusFilter == Rejected)
                    ]
                    [ text_ "all_analysis.disapproved" ]
                , option
                    [ value "pending"
                    , selected (model.filters.statusFilter == Pending)
                    ]
                    [ text_ "all_analysis.pending" ]
                ]
            ]
        ]


viewEmptyResults : Html Msg
viewEmptyResults =
    div []
        [ text "empty"
        ]


viewClaim : LoggedIn.Model -> Claim.Model -> Html Msg
viewClaim { shared, accountName, selectedCommunity } claim =
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
            case claim.status of
                Claim.Approved ->
                    ( t "all_analysis.approved", "text-green" )

                Claim.Rejected ->
                    ( t "all_analysis.disapproved", "text-red" )

                Claim.Pending ->
                    ( t "all_analysis.pending", "text-black" )
    in
    div [ class "w-full md:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-4" ]
        [ if Claim.isAlreadyValidated claim accountName then
            div [ class "flex flex-col p-4 my-2 rounded-lg bg-white" ]
                [ div [ class "flex justify-center mb-8" ]
                    [ Profile.view shared accountName claim.claimer
                    ]
                , div [ class "mb-6" ]
                    [ div
                        [ class "bg-gray-100 flex items-center justify-center h-6 w-32 mb-2" ]
                        [ p
                            [ class ("text-caption uppercase " ++ textColor) ]
                            [ text msg ]
                        ]
                    , p [ class "text-body mb-2" ]
                        [ text claim.action.description ]
                    , p
                        [ class "text-gray-900 text-caption uppercase" ]
                        [ text <| date claim.createdAt ]
                    ]
                , a
                    [ class "button button-secondary w-full font-medium mb-2"
                    , Route.href <| Route.Claim selectedCommunity claim.action.objective.id claim.action.id claim.id
                    ]
                    [ text_ "all_analysis.more_details" ]
                ]

          else
            div [ class "flex flex-col p-4 my-2 rounded-lg bg-white", id <| "claim-" ++ String.fromInt claim.id ]
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


viewPagination : LoggedIn.Model -> Maybe Api.Relay.PageInfo -> Html Msg
viewPagination { shared } maybePageInfo =
    let
        t s =
            I18Next.t shared.translations s

        text_ s =
            text (t s)
    in
    case maybePageInfo of
        Just pageInfo ->
            div [ class "flex justify-center" ]
                [ if pageInfo.hasNextPage then
                    button
                        [ Route.href Route.Analysis
                        , class "button button-primary uppercase w-full"
                        , onClick ShowMore
                        ]
                        [ text_ "all_analysis.show_more" ]

                  else
                    text ""
                ]

        Nothing ->
            text ""


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
    = ClaimsLoaded (Result (Graphql.Http.Error (Maybe Claim.Paginated)) (Maybe Claim.Paginated))
    | OpenModal Int Bool
    | CloseModal
    | VoteClaim Int Bool
    | GotVoteResult Int (Result Decode.Value String)
    | SelectMsg (Select.Msg Profile)
    | OnSelectVerifier (Maybe Profile)
    | CompletedCommunityLoad (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | ShowMore
    | ClearSelectSelection
    | SelectStatusFilter StatusFilter


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        ClaimsLoaded (Ok results) ->
            case model.status of
                Loaded claims _ ->
                    let
                        newClaims =
                            if model.reloadOnNextQuery then
                                Claim.paginatedToList results

                            else
                                claims ++ Claim.paginatedToList results
                    in
                    { model
                        | status = Loaded newClaims (Claim.paginatedPageInfo results)
                        , reloadOnNextQuery = False
                    }
                        |> UR.init

                _ ->
                    { model
                        | status =
                            Loaded (Claim.paginatedToList results) (Claim.paginatedPageInfo results)
                        , reloadOnNextQuery = False
                    }
                        |> UR.init

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
                Loaded claims pageInfo ->
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
                                | status = Loaded claims pageInfo
                            }
                                |> UR.init
                                |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Success (message value))
                                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey Route.Analysis)

                        Nothing ->
                            model
                                |> UR.init

                _ ->
                    model |> UR.init

        GotVoteResult _ (Err _) ->
            case model.status of
                Loaded claims pageInfo ->
                    { model | status = Loaded claims pageInfo }
                        |> UR.init

                _ ->
                    model |> UR.init

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration loggedIn.shared False) subMsg model.autoCompleteState
            in
            UR.init { model | autoCompleteState = updated }
                |> UR.addCmd cmd

        OnSelectVerifier maybeProfile ->
            let
                oldFilters =
                    model.filters
            in
            case model.status of
                Loaded _ _ ->
                    let
                        newModel =
                            { model
                                | status = Loading
                                , filters = { oldFilters | profile = maybeProfile }
                                , reloadOnNextQuery = True
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchAnalysis loggedIn newModel.filters Nothing)

                _ ->
                    UR.init model

        CompletedCommunityLoad (Ok community) ->
            case community of
                Just cmm ->
                    UR.init { model | communityStatus = LoadedCommunity cmm }

                Nothing ->
                    UR.init { model | communityStatus = FailedCommunity }

        CompletedCommunityLoad (Err error) ->
            { model | communityStatus = FailedCommunity }
                |> UR.init
                |> UR.logGraphqlError msg error

        ShowMore ->
            case model.status of
                Loaded _ pageInfo ->
                    let
                        cursor : Maybe String
                        cursor =
                            Maybe.andThen .endCursor pageInfo
                    in
                    model
                        |> UR.init
                        |> UR.addCmd (fetchAnalysis loggedIn model.filters cursor)

                _ ->
                    UR.init model

        ClearSelectSelection ->
            case model.status of
                Loaded _ _ ->
                    let
                        oldFilters =
                            model.filters

                        newModel =
                            { model
                                | status = Loading
                                , filters = { oldFilters | profile = Nothing }
                                , reloadOnNextQuery = True
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchAnalysis loggedIn newModel.filters Nothing)

                _ ->
                    UR.init model

        SelectStatusFilter statusFilter ->
            let
                oldFilters =
                    model.filters

                newModel =
                    { model
                        | filters = { oldFilters | statusFilter = statusFilter }
                        , status = Loading
                        , reloadOnNextQuery = True
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd (fetchAnalysis loggedIn newModel.filters Nothing)


fetchAnalysis : LoggedIn.Model -> Filter -> Maybe String -> Cmd Msg
fetchAnalysis { accountName, selectedCommunity, shared } { profile, statusFilter } maybeCursorAfter =
    let
        filterRecord =
            { claimer = Absent
            , status = Absent
            }
                |> (\fr ->
                        { fr
                            | claimer =
                                case profile of
                                    Just p ->
                                        Present (Eos.nameToString p.account)

                                    Nothing ->
                                        Absent
                        }
                   )
                |> (\fr ->
                        { fr
                            | status =
                                case statusFilter of
                                    All ->
                                        Absent

                                    Approved ->
                                        Present "approved"

                                    Rejected ->
                                        Present "rejected"

                                    Pending ->
                                        Present "pending"
                        }
                   )

        filter =
            if filterRecord.claimer /= Absent || filterRecord.status /= Absent then
                Present filterRecord

            else
                Absent

        args =
            { input =
                { symbol = Eos.symbolToString selectedCommunity
                , account = Eos.nameToString accountName
                , filter = filter
                }
            }

        mapFn =
            \s ->
                if String.isEmpty s then
                    Nothing

                else
                    Just (Present s)

        pagination =
            \a ->
                { a
                    | first = Present 16
                    , after =
                        Maybe.andThen mapFn maybeCursorAfter
                            |> Maybe.withDefault Absent
                }
    in
    Api.Graphql.query
        shared
        (Cambiatus.Query.claimsAnalysisHistory pagination args Claim.claimPaginatedSelectionSet)
        ClaimsLoaded



-- Configure Select


selectFilter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
selectFilter minChars toLabel query items =
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
            , filter = selectFilter 2 (\p -> Eos.nameToString p.account)
            }
            |> Select.withMultiSelection True
        )
        shared
        isDisabled


viewSelectedVerifiers : LoggedIn.Model -> List Profile -> Html Msg
viewSelectedVerifiers ({ shared } as loggedIn) selectedVerifiers =
    div [ class "flex flex-row mt-3 mb-10 flex-wrap" ]
        (selectedVerifiers
            |> List.map
                (\p ->
                    div
                        [ class "flex justify-between flex-col m-3 items-center" ]
                        [ Profile.view shared loggedIn.accountName p
                        , div
                            [ onClick ClearSelectSelection
                            , class "h-6 w-6 flex items-center mt-4"
                            ]
                            [ Icons.trash "" ]
                        ]
                )
        )


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

        SelectMsg _ ->
            [ "SelectMsg", "sub" ]

        OnSelectVerifier _ ->
            [ "OnSelectVerifier" ]

        CompletedCommunityLoad r ->
            [ "CompletedCommunityLoad", UR.resultToString r ]

        ShowMore ->
            [ "ShowMore" ]

        ClearSelectSelection ->
            [ "ClearSelectSelection" ]

        SelectStatusFilter _ ->
            [ "SelectStatusFilter" ]
