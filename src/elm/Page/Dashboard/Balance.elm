module Page.Dashboard.Balance exposing
    ( Model
    , Msg
    , UpdateResult
    , init
    , jsAddressToMsg
    , msgToString
    , update
    , viewCard
    , viewInvitationModal
    )

import Api
import Api.Graphql
import Community exposing (Balance)
import Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, button, div, img, input, p, span, text)
import Html.Attributes exposing (class, classList, disabled, href, id, src, value)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (Delims(..), t)
import Icons
import Json.Encode as Encode exposing (Value)
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Transfer exposing (Transfer)
import UpdateResult as UR
import Url



-- INIT


init : LoggedIn.Model -> Balance -> ( Model, Cmd Msg )
init { shared } balance =
    let
        cmd =
            Api.Graphql.query shared
                (Community.logoTitleQuery balance.asset.symbol)
                CompletedDashboardInfoLoad
    in
    ( { balance = balance
      , status = ViewingBalance
      , dashboardInfo = Nothing
      , autoCompleteState = Select.newState ""
      , inviteModal = Closed
      , copied = False
      }
    , cmd
    )



-- MODEL


type alias Model =
    { balance : Balance
    , status : Status
    , dashboardInfo : Maybe Community.DashboardInfo
    , autoCompleteState : Select.State
    , inviteModal : ModalStatus
    , copied : Bool
    }


type Status
    = ViewingBalance


type ModalStatus
    = Closed
    | Loading
    | Failed String
    | Loaded String



-- VIEW


viewCard : LoggedIn.Model -> Model -> Html Msg
viewCard loggedIn model =
    case model.status of
        ViewingBalance ->
            viewCardBalance loggedIn model


viewCardBalance : LoggedIn.Model -> Model -> Html Msg
viewCardBalance loggedIn ({ balance } as model) =
    let
        ipfsUrl =
            loggedIn.shared.endpoints.ipfs

        text_ s =
            text (t loggedIn.shared.translations s)

        symbolText =
            Eos.symbolToString balance.asset.symbol

        balanceText =
            String.fromFloat balance.asset.amount ++ " "

        logo =
            case model.dashboardInfo of
                Just info ->
                    info.logo

                Nothing ->
                    ""

        title =
            case model.dashboardInfo of
                Just info ->
                    info.title

                Nothing ->
                    ""
    in
    div [ class "w-full sm:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-6" ]
        [ div [ class "flex flex-col items-center justify-center px-3 pt-5 pb-2 rounded-lg hover:shadow-lg bg-white" ]
            [ div [ class "" ] [ img [ class "object-none object-scale-down h-20 mb-2", src (ipfsUrl ++ "/" ++ logo) ] [] ]
            , div [ class "" ] [ p [ class "leading-none text-menu" ] [ text title ] ]
            , div [ class "flex items-center justify-center" ]
                [ div [ class "text-indigo-500 font-bold text-3xl" ]
                    [ text balanceText ]
                , div [ class "text-indigo-500 ml-2" ]
                    [ text symbolText ]
                ]
            , div [ class "input-label mb-4" ]
                [ text_ "account.my_wallet.balances.current" ]
            , div [ class "flex flex-col py-3 w-full" ]
                [ a
                    [ class "button button-secondary button-sm text-xs font-medium w-full mb-4"
                    , Route.href (Route.Community balance.asset.symbol)
                    ]
                    [ text_ "menu.explore" ]
                , button
                    [ class "button button-secondary button-sm text-xs w-full mb-4"
                    , onClick CreateInvite
                    ]
                    [ text_ "community.invite.title" ]
                , a
                    [ class "button button-primary button-sm text-xs w-full"
                    , Route.href (Route.Transfer balance.asset.symbol Nothing)
                    ]
                    [ text_ "account.my_wallet.balances.button" ]
                ]
            ]
        ]


viewInvitationModal : LoggedIn.Model -> Model -> Html Msg
viewInvitationModal { shared } model =
    let
        t s =
            I18Next.t shared.translations s

        text_ s =
            text (t s)

        protocol =
            case shared.url.protocol of
                Url.Http ->
                    "http://"

                Url.Https ->
                    "https://"

        url invitationId =
            protocol ++ shared.url.host ++ "/invite/" ++ invitationId
    in
    case model.inviteModal of
        Closed ->
            text ""

        _ ->
            div [ class "modal container" ]
                [ div [ class "modal-bg", onClick CloseInviteModal ] []
                , div [ class "modal-content" ]
                    [ div [ class "w-full" ]
                        [ p [ class "text-2xl font-medium mb-4" ]
                            [ text_ "community.invite.title" ]
                        , button [ onClick CloseInviteModal ]
                            [ Icons.close "absolute fill-current text-gray-400 top-0 right-0 mx-8 my-4" ]
                        , case model.inviteModal of
                            Closed ->
                                text ""

                            Loading ->
                                div [ class "flex items-center justify-center" ]
                                    [ div [ class "spinner spinner--delay" ] [] ]

                            Failed err ->
                                div []
                                    [ div [ class "flex items-center justify-center text-heading text-red" ]
                                        [ p [ class "text-sm text-red" ] [ text err ] ]
                                    , div [ class "w-full md:bg-gray-100 flex md:absolute rounded-b-lg md:inset-x-0 md:bottom-0 md:p-4 justify-center items-center" ]
                                        [ button
                                            [ class "button button-primary"
                                            , onClick CloseInviteModal
                                            ]
                                            [ text "OK" ]
                                        ]
                                    ]

                            Loaded invitationId ->
                                div [ class "flex flex-wrap items-center mt-24 md:mt-0" ]
                                    [ div [ class "flex flex-col items-left w-full mb-4" ]
                                        [ span [ class "input-label" ]
                                            [ text_ "community.invite.label" ]
                                        , input
                                            [ class "text-menu border p-2 md:border-none md:text-heading outline-none text-black"
                                            , id "invitation-id"
                                            , value (url invitationId)
                                            , disabled True
                                            ]
                                            []
                                        ]
                                    , div [ class "w-full md:bg-gray-100 flex md:absolute rounded-b-lg md:inset-x-0 md:bottom-0 md:p-4 justify-center items-center" ]
                                        [ button
                                            [ classList
                                                [ ( "button-primary", not model.copied )
                                                , ( "button-success", model.copied )
                                                ]
                                            , class "button w-full md:w-48"
                                            , onClick (CopyToClipboard "invitation-id")
                                            ]
                                            [ if model.copied then
                                                text_ "community.invite.copied"

                                              else
                                                text_ "community.invite.copy"
                                            ]
                                        ]
                                    ]
                        ]
                    ]
                ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedDashboardInfoLoad (Result (Graphql.Http.Error (Maybe Community.DashboardInfo)) (Maybe Community.DashboardInfo))
    | CreateInvite
    | CloseInviteModal
    | CompletedInviteCreation (Result Http.Error String)
    | CopyToClipboard String
    | CopiedToClipboard


update : LoggedIn.Model -> Msg -> Model -> UpdateResult
update ({ shared } as loggedIn) msg model =
    case msg of
        CompletedDashboardInfoLoad (Err _) ->
            model |> UR.init

        CompletedDashboardInfoLoad (Ok res) ->
            case res of
                Just result ->
                    result
                        |> updateDashboardInfo model
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init

        CreateInvite ->
            UR.init
                { model | inviteModal = Loading }
                |> UR.addCmd
                    (CompletedInviteCreation
                        |> Api.communityInvite shared model.balance.asset.symbol loggedIn.accountName
                    )

        CloseInviteModal ->
            UR.init
                { model | inviteModal = Closed }

        CompletedInviteCreation (Ok invitationId) ->
            { model | inviteModal = Loaded invitationId }
                |> UR.init

        CompletedInviteCreation (Err httpError) ->
            UR.init
                { model | inviteModal = Failed (I18Next.t shared.translations "community.invite.failed") }
                |> UR.logHttpError msg httpError

        CopyToClipboard elementId ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = CopiedToClipboard
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "id", Encode.string elementId )
                            , ( "name", Encode.string "copyToClipboard" )
                            ]
                    }

        CopiedToClipboard ->
            { model | copied = True }
                |> UR.init


updateDashboardInfo : Model -> Community.DashboardInfo -> Model
updateDashboardInfo model result =
    { model | dashboardInfo = Just result }


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr _ =
    case addr of
        "CopiedToClipboard" :: _ ->
            Just CopiedToClipboard

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    let
        resultToString ss r =
            case r of
                Ok _ ->
                    ss ++ [ "Ok" ]

                Err _ ->
                    ss ++ [ "Err" ]
    in
    case msg of
        CompletedDashboardInfoLoad result ->
            resultToString [ "CompletedDashboardInfoLoad" ] result

        CreateInvite ->
            [ "CreateInvite" ]

        CloseInviteModal ->
            [ "CloseInviteModal" ]

        CompletedInviteCreation _ ->
            [ "CompletedInviteCreation" ]

        CopyToClipboard _ ->
            [ "CopyToClipboard" ]

        CopiedToClipboard ->
            [ "CopiedToClipboard" ]
