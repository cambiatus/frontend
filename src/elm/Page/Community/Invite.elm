module Page.Community.Invite exposing
    ( Model
    , Msg
    , init
    , msgToString
    , update
    , view
    )

-- import Session.Guest as Guest

import Api.Graphql
import Community exposing (Invite, inviteQuery)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (class, src)
import I18Next exposing (t)
import Page exposing (Session(..), toShared)
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared as Shared exposing (Shared)
import UpdateResult as UR


init : Session -> String -> ( Model, Cmd Msg )
init session invitationId =
    ( initModel, Api.Graphql.query (toShared session) (Community.inviteQuery invitationId) CompletedLoad )


initModel : Model
initModel =
    { status = Loading }


type alias Model =
    { status : Status
    }


type Status
    = Loading
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Invite))
    | Loaded Invite


view : Session -> Model -> Html msg
view session model =
    let
        shared =
            toShared session
    in
    div [ class "flex flex-col min-h-screen" ]
        [ div [ class "flex-grow" ]
            [ case model.status of
                Loading ->
                    div [] [ text "loading" ]

                NotFound ->
                    div [] [ text "not found" ]

                Failed e ->
                    Page.fullPageGraphQLError (t shared.translations "") e

                Loaded invite ->
                    div []
                        [ viewHeader
                        , viewContent shared invite
                        ]
            ]
        , viewFooter session
        ]


viewHeader : Html msg
viewHeader =
    div [ class "w-full h-16 flex px-4 items-center bg-indigo-500" ]
        []


viewContent : Shared -> Invite -> Html msg
viewContent shared invite =
    let
        text_ s =
            text (I18Next.t shared.translations s)
    in
    div [ class "bg-white" ]
        [ img [ src (shared.endpoints.ipfs ++ "/" ++ invite.community.logo), class "object-scale-down" ] []
        , div []
            [ span []
                [ text
                    (invite.creator.userName
                        |> Maybe.withDefault (Eos.nameToString invite.creator.account)
                    )
                ]
            , text_ "community.invite.title"
            , span []
                [ text invite.community.title ]
            ]
        , div []
            [ button [ class "buton button-secondary" ] [ text_ "community.invite.no" ]
            , button [ class "buton button-primary" ] [ text_ "community.invite.yes" ]
            ]
        ]


viewFooter : Session -> Html msg
viewFooter session =
    let
        shared =
            case session of
                LoggedIn loggedIn ->
                    loggedIn.shared

                Guest guest ->
                    guest.shared
    in
    LoggedIn.viewFooter shared



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Invite)) (Maybe Invite))


update : Session -> Msg -> Model -> UpdateResult
update _ msg model =
    case msg of
        CompletedLoad (Ok (Just invitation)) ->
            UR.init { model | status = Loaded invitation }

        CompletedLoad (Ok Nothing) ->
            UR.init { model | status = NotFound }

        CompletedLoad (Err error) ->
            { model | status = Failed error }
                |> UR.init
                |> UR.logGraphqlError msg error


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]
