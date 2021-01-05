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
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn account =
    ( initModel
    , profileClaimQuery loggedIn account
    )


type alias Model =
    { status : Status
    , claimModalStatus : Claim.ModalStatus
    }


initModel : Model
initModel =
    { status = Loading
    , claimModalStatus = Claim.Closed
    }


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

                Loaded profile ->
                    div []
                        [ Page.viewHeader loggedIn pageTitle Route.Dashboard
                        , viewResults loggedIn profile.claims
                        ]

                NotFound ->
                    Page.fullPageNotFound
                        (t "profile.claims.not_found.title")
                        (t "profile.claims.not_found.subtitle")

                Failed e ->
                    Page.fullPageGraphQLError pageTitle e
    in
    { title = pageTitle, content = content }


viewResults : LoggedIn.Model -> List Claim.Model -> Html Msg
viewResults loggedIn claims =
    let
        viewClaim claim =
            Claim.viewClaimCard loggedIn claim
                |> Html.map ClaimMsg
    in
    div [ class "container mx-auto px-4 mb-10" ]
        [ if List.length claims > 0 then
            div [ class "flex flex-wrap -mx-2" ]
                (claims
                    |> List.reverse
                    |> List.map viewClaim
                )

          else
            viewEmptyResults loggedIn
        ]


viewEmptyResults : LoggedIn.Model -> Html Msg
viewEmptyResults { shared } =
    let
        text_ s =
            text (shared.translators.t s)
    in
    div [ class "w-full text-center" ]
        [ div [ class "w-full flex justify-center" ]
            [ img [ src "/images/empty-analysis.svg", class "object-contain h-32 mb-3" ] []
            ]
        , div [ class "inline-block text-gray" ]
            [ text_ "profile.claims.empty_results"
            ]
        ]


type alias ProfileClaims =
    { claims : List Claim.Model }


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = ClaimsLoaded (Result (Graphql.Http.Error (Maybe ProfileClaims)) (Maybe ProfileClaims))
    | ClaimMsg Claim.Msg


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

        ClaimMsg _ ->
            UR.init model


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
        ClaimsLoaded r ->
            [ "ClaimsLoaded", UR.resultToString r ]

        ClaimMsg _ ->
            [ "ClaimMsg" ]
