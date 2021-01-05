module Page.Profile.Claims exposing
    ( Model
    , Msg(..)
    , init
    , msgToString
    , update
    , view
    )

import Api.Graphql
import Cambiatus.Object
import Cambiatus.Object.Profile as Profile
import Cambiatus.Query
import Claim
import Eos.Account as Eos
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, div, text)
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR


type alias Model =
    { status : Status }


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn account =
    ( { status = Loading }, profileClaimQuery loggedIn account )


type Status
    = Loading
    | Loaded ProfileClaims
    | NotFound
    | Failed (Graphql.Http.Error (Maybe ProfileClaims))



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        pageTitle =
            t "profile.claims.title"

        content =
            case model.status of
                Loading ->
                    Page.fullPageLoading loggedIn.shared

                Loaded profileClaims ->
                    div []
                        [ Page.viewHeader loggedIn pageTitle Route.Dashboard
                        , text "Empty yet"
                        ]

                NotFound ->
                    Page.fullPageNotFound (t "profile.claims.not_found.title") (t "profile.claims.not_found.subtitle")

                Failed e ->
                    Page.fullPageGraphQLError pageTitle e
    in
    { title = pageTitle, content = content }


type alias ProfileClaims =
    { claims : List Claim.Model }


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = ClaimsLoaded (Result (Graphql.Http.Error (Maybe ProfileClaims)) (Maybe ProfileClaims))


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        ClaimsLoaded (Ok results) ->
            case results of
                Just claims ->
                    { model | status = Loaded claims }
                        |> UR.init

                Nothing ->
                    { model | status = NotFound }
                        |> UR.init

        ClaimsLoaded (Err e) ->
            { model | status = Failed e }
                |> UR.init


profileClaimQuery : LoggedIn.Model -> String -> Cmd Msg
profileClaimQuery ({ shared } as loggedIn) accountName =
    Api.Graphql.query
        shared
        (Cambiatus.Query.profile { account = accountName } selectionSet)
        ClaimsLoaded


selectionSet : SelectionSet ProfileClaims Cambiatus.Object.Profile
selectionSet =
    SelectionSet.succeed ProfileClaims
        |> with (Profile.claims Claim.selectionSet)


msgToString : Msg -> List String
msgToString msg =
    case msg of
        _ ->
            []
