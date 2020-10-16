module Page.Dashboard.Claim exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view, viewVoters)

import Api.Graphql
import Browser.Events exposing (onClick)
import Cambiatus.Object.Claim as Claim
import Cambiatus.Object.Profile as Profile
import Cambiatus.Query
import Cambiatus.Scalar exposing (DateTime(..))
import Claim
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, button, div, h3, p, span, text)
import Html.Attributes exposing (class, classList)
import I18Next
import Json.Encode as Encode
import Page
import Profile
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Shared)
import Strftime
import Time
import UpdateResult as UR
import Utils



-- INIT


init : LoggedIn.Model -> Symbol -> Int -> ( Model, Cmd Msg )
init { shared } communityId claimId =
    ( initModel communityId claimId
    , fetchClaim claimId shared
    )



-- MODEL


type alias Model =
    { communityId : Symbol
    , claimId : Int
    , statusClaim : Status
    }


initModel : Symbol -> Int -> Model
initModel communityId claimId =
    { communityId = communityId
    , claimId = claimId
    , statusClaim = Loading
    }


type Status
    = Loading
    | Loaded Claim.Model
    | Failed (Graphql.Http.Error Claim.Model)



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        t : String -> String
        t =
            I18Next.t shared.translations

        title =
            case model.statusClaim of
                Loaded claim ->
                    claim.action.description

                _ ->
                    ""

        content =
            div []
                [ case model.statusClaim of
                    Loading ->
                        Page.fullPageLoading

                    Loaded claim ->
                        div [ class "bg-gray-100 py-2" ]
                            [ Page.viewHeader loggedIn claim.action.description Route.Analysis
                            , div [ class "mt-10 mb-8" ]
                                [ Profile.viewLarge shared loggedIn.accountName claim.claimer
                                ]
                            , div [ class "mx-auto container px-4" ]
                                [ viewTitle shared claim
                                , viewDetails shared model claim
                                , viewVoters loggedIn claim
                                ]
                            ]

                    Failed err ->
                        Page.fullPageGraphQLError (t "error.unknown") err
                ]
    in
    { title = title
    , content = content
    }


viewTitle : Shared -> Claim.Model -> Html msg
viewTitle shared claim =
    let
        text_ s =
            text (I18Next.t shared.translations s)
    in
    div [ class "text-heading font-bold text-center mb-8" ]
        [ case claim.status of
            Claim.Approved ->
                div [ class "inline-block" ]
                    [ text_ "claim.title_approved.1"
                    , span [ class "text-green ml-1" ] [ text_ "claim.title_approved.2" ]
                    ]

            Claim.Rejected ->
                div [ class "inline-block" ]
                    [ text_ "claim.title_rejected.1"
                    , span [ class "text-red ml-1" ] [ text_ "claim.title_rejected.2" ]
                    ]

            Claim.Pending ->
                div [ class "inline-block" ]
                    [ text_ "claim.title_under_review.1"
                    , span [ class "text-gray ml-1" ] [ text_ "claim.title_under_review.2" ]
                    , span [ class "mr-1" ] [ text_ "claim.title_under_review.3" ]
                    ]
        ]


viewDetails : Shared -> Model -> Claim.Model -> Html msg
viewDetails shared model claim =
    let
        text_ s =
            text (I18Next.t shared.translations s)

        isRejected =
            claim.status == Claim.Rejected
    in
    div []
        [ div [ class "mb-8" ]
            [ p
                [ class "text-caption uppercase text-green" ]
                [ text_ "claim.action" ]
            , div [ class "mb-2" ]
                [ p [ class "pt-2 text-body" ]
                    [ text claim.action.description ]
                ]
            ]
        , div [ class "mb-8" ]
            [ div
                [ class "flex justify-between lg:justify-start" ]
                [ div [ class "mr-6" ]
                    [ p [ class "text-caption uppercase text-green" ]
                        [ text_ "claim.date" ]
                    , p [ class "pt-2 text-body" ]
                        [ text
                            (claim.createdAt
                                |> Just
                                |> Utils.posixDateTime
                                |> Strftime.format "%d %b %Y %H:%M" Time.utc
                            )
                        ]
                    ]
                , div []
                    [ p
                        [ class "text-caption uppercase text-green" ]
                        [ text_ "claim.claimer_reward" ]
                    , p
                        [ class "pt-2 text-body"
                        , classList [ ( "text-red line-through", isRejected ) ]
                        ]
                        [ text (String.fromFloat claim.action.reward ++ " " ++ Eos.symbolToSymbolCodeString model.communityId) ]
                    ]
                ]
            ]
        , div [ class "mb-8" ]
            [ p
                [ class "text-caption uppercase text-green" ]
                [ text_ "claim.objective" ]
            , p
                [ class "pt-2 text-body" ]
                [ text claim.action.objective.description ]
            ]
        , div [ class "mb-8" ]
            [ p
                [ class "text-caption uppercase text-green" ]
                [ text_ "claim.your_reward" ]
            , p
                [ class "pt-2 text-body" ]
                [ text (String.fromFloat claim.action.verifierReward ++ " " ++ Eos.symbolToSymbolCodeString model.communityId) ]
            ]
        ]


viewVoters : LoggedIn.Model -> Claim.Model -> Html msg
viewVoters ({ shared } as loggedIn) claim =
    let
        text_ s =
            text (I18Next.t shared.translations s)

        pendingValidators =
            List.filter
                (\p -> not <| List.member p.account (List.map (\c -> c.validator.account) claim.checks))
                claim.action.validators
    in
    div []
        [ div [ class "mb-8" ]
            [ p [ class "text-caption uppercase text-green" ]
                [ text_ "claim.approved_by" ]
            , div []
                [ if List.isEmpty claim.checks then
                    div [ class "flex mb-10" ]
                        [ div [ class "pt-2" ] [ Profile.viewEmpty shared ]
                        ]

                  else
                    div [ class "flex flex-wrap -mx-2 pt-2" ]
                        (List.map
                            (\c ->
                                if c.isApproved then
                                    div [ class "px-2" ]
                                        [ Profile.view shared loggedIn.accountName c.validator
                                        ]

                                else
                                    text ""
                            )
                            claim.checks
                        )
                ]
            ]
        , div [ class "mb-8" ]
            [ p [ class "text-caption uppercase text-green" ]
                [ text_ "claim.disapproved_by" ]
            , div [ class "flex mb-10 " ]
                [ if List.filter (\check -> check.isApproved == False) claim.checks |> List.isEmpty then
                    div [ class "pt-2" ] [ Profile.viewEmpty shared ]

                  else
                    div [ class "flex flex-wrap -mx-2 pt-2" ]
                        (List.map
                            (\c ->
                                if not c.isApproved then
                                    div [ class "px-2" ]
                                        [ Profile.view shared loggedIn.accountName c.validator
                                        ]

                                else
                                    text ""
                            )
                            claim.checks
                        )
                ]
            ]
        , div [ class "mb-8" ]
            [ p [ class "text-caption uppercase text-green" ]
                [ text_ "claim.pending" ]
            , div [ class "pt-2" ]
                [ if List.length claim.checks == claim.action.verifications then
                    div [ class "flex" ]
                        [ Profile.viewEmpty shared
                        ]

                  else
                    div [ class "flex flex-row flex-wrap space-x-6" ]
                        (List.map (\v -> Profile.view shared loggedIn.accountName v) pendingValidators)
                ]
            ]
        , if Claim.isValidated claim loggedIn.accountName then
            text ""

          else
            div [ class "mb-8 border-t pt-8" ]
                [ h3 [ class "font-bold mb-6 text-center" ]
                    [ text "Are you approve or disapprove this action?" ]
                , div [ class "flex justify-around sm:justify-center sm:space-x-3" ]
                    [ button
                        [ class "button button-secondary text-red"

                        --, onClick (ConfirmVoteOpen claim.id False)
                        ]
                        [ text_ "dashboard.reject" ]
                    , button
                        [ class "button button-primary"

                        --, onClick (ConfirmVoteOpen claim.id True)
                        ]
                        [ text_ "dashboard.verify" ]
                    ]
                ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = ClaimLoaded (Result (Graphql.Http.Error Claim.Model) Claim.Model)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model _ =
    case msg of
        ClaimLoaded (Ok response) ->
            { model | statusClaim = Loaded response }
                |> UR.init

        ClaimLoaded (Err err) ->
            { model | statusClaim = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg err



-- HELPERS


fetchClaim : Int -> Shared -> Cmd Msg
fetchClaim claimId shared =
    Api.Graphql.query
        shared
        (Cambiatus.Query.claim { input = { id = claimId } } Claim.selectionSet)
        ClaimLoaded


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr _ =
    case addr of
        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ClaimLoaded r ->
            [ "ClaimLoaded", UR.resultToString r ]
