module Page.Dashboard.Analysis exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , update
    , view
    )

import Claim
import Graphql.Http
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import I18Next
import Json.Encode as Encode
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import UpdateResult as UR


init : LoggedIn.Model -> ( Model, Cmd Msg )
init { shared } =
    ( Loading
    , Cmd.none
    )



-- MODEL


type Model
    = Loading
    | Loaded Filter
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

        Loaded filter ->
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
    = ChecksLoaded (Result (Graphql.Http.Error (List Claim.Model)) (List Claim.Model))


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model _ =
    case msg of
        ChecksLoaded (Ok _) ->
            model |> UR.init

        ChecksLoaded (Err _) ->
            model |> UR.init


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr _ =
    case addr of
        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ChecksLoaded r ->
            [ "ChecksLoaded", UR.resultToString r ]
