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

        inviter =
            invite.creator.userName
                |> Maybe.withDefault (Eos.nameToString invite.creator.account)
    in
    div [ class "bg-white pb-20" ]
        [ div [ class "flex flex-wrap content-end" ]
            [ div [ class "flex items-center justify-center h-24 w-24 rounded-full mx-auto -mt-12 bg-white" ]
                [ img
                    [ src (shared.endpoints.ipfs ++ "/" ++ invite.community.logo)
                    , class "object-scale-down h-20 w-20"
                    ]
                    []
                ]
            ]
        , div [ class "flex mx-auto justify-center mt-6" ]
            [ span [ class "font-bold mr-1" ] [ text inviter ]
            , span [ class "mr-1" ] [ text_ "community.invitation.title" ]
            , span [ class "font-bold" ] [ text invite.community.title ]
            ]
        , div [ class "flex mx-auto justify-center mt-6" ]
            [ span [ class "mr-1" ] [ text_ "community.invitation.subtitle" ]
            , text invite.community.title
            , text "?"
            ]
        , div [ class "flex justify-center w-full mt-6" ]
            [ button [ class "button button-sm button-secondary uppercase mr-8" ] [ text_ "community.invitation.no" ]
            , button [ class "button button-sm button-primary uppercase" ] [ text_ "community.invitation.yes" ]
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
