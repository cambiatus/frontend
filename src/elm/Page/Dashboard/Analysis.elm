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
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import I18Next
import Json.Encode as Encode
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Shared)
import UpdateResult as UR


init : LoggedIn.Model -> ( Model, Cmd Msg )
init { shared, accountName, selectedCommunity } =
    ( Loading
    , fetchAnalysis shared selectedCommunity accountName
    )



-- MODEL


type Model
    = Loading
    | Loaded Filter (List Claim.Model)
    | Failed


type Filter
    = All
    | Approved
    | Disapproved
    | UnderReview
    | UnderReviewPending



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view ({ shared } as loggedIn) model =
    let
        t : String -> String
        t =
            I18Next.t shared.translations
    in
    case model of
        Loading ->
            Page.fullPageLoading

        Loaded filter _ ->
            div [ class "bg-white py-2" ]
                [ Page.viewHeader loggedIn (t "dashboard.all_analysis.title") Route.Dashboard
                , text "WIP"
                ]

        Failed ->
            text ""



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = ClaimsLoaded (Result (Graphql.Http.Error (List Claim.Model)) (List Claim.Model))


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model _ =
    case msg of
        ClaimsLoaded (Ok results) ->
            Loaded All results |> UR.init

        ClaimsLoaded (Err _) ->
            Failed |> UR.init


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
jsAddressToMsg addr _ =
    case addr of
        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ClaimsLoaded r ->
            [ "ChecksLoaded", UR.resultToString r ]
