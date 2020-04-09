module Page.ViewTransfer exposing (Model, Msg, init, msgToString, subscriptions, update, view)

import Api.Graphql
import Cambiatus.Scalar exposing (DateTime(..))
import Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import I18Next
import Page
import Profile
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Strftime
import Time
import Transfer exposing (Transfer, transferQuery)
import UpdateResult as UR
import Utils



-- INIT


init : LoggedIn.Model -> Int -> ( Model, Cmd Msg )
init { shared } transferId =
    let
        model =
            { status = Loading transferId
            , transferId = transferId
            }
    in
    ( model
    , Api.Graphql.query shared (transferQuery transferId) CompletedTransferLoad
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    { status : Status
    , transferId : Int
    }


type State
    = Transferred
    | Received


type Status
    = Loading Int
    | InvalidId String
    | LoadFailed (Graphql.Http.Error (Maybe Transfer))
    | Loaded (Maybe Transfer) State



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    let
        t =
            I18Next.t loggedIn.shared.translations
    in
    case model.status of
        Loading _ ->
            Page.fullPageLoading

        InvalidId invalidId ->
            div [ class "container mx-auto px-4" ]
                [ Page.viewHeader loggedIn (t "transfer_result.title") Route.Dashboard
                , div []
                    [ text (invalidId ++ t "transfer_result.errors.invalid_id") ]
                ]

        LoadFailed error ->
            div []
                [ Page.viewHeader loggedIn (t "transfer_result.title") Route.Dashboard
                , Page.fullPageGraphQLError (t "transfer_result.title") error
                ]

        Loaded maybeTransfer state ->
            case maybeTransfer of
                Just transfer ->
                    div []
                        [ Page.viewHeader loggedIn (t "transfer_result.title") Route.Dashboard
                        , div []
                            [ viewTransfer loggedIn transfer state
                            , viewDetails loggedIn transfer state
                            ]
                        ]

                Nothing ->
                    div [ class "container mx-auto px-4" ]
                        [ div []
                            [ text "Could not load the transfer" ]
                        ]


viewTransfer : LoggedIn.Model -> Transfer -> State -> Html Msg
viewTransfer loggedIn _ state =
    let
        t =
            I18Next.t loggedIn.shared.translations
    in
    div [ class "flex w-full justify-center bg-green py-8" ]
        [ div [ class "flex-row w-full lg:w-2/3" ]
            [ div [ class "flex flex-wrap justify-center" ]
                [ img
                    [ class "h-64 w-full lg:w-1/3"
                    , src "/images/transfer-doggo.svg"
                    ]
                    []
                , h2 [ class "w-full lg:w-2/3 mt-8 mb-6 lg:px-8 text-center lg:text-left text-3xl font-medium text-white" ]
                    [ text <|
                        case state of
                            Transferred ->
                                t "transfer_result.transfer_success"

                            Received ->
                                t "transfer_result.receive_success"
                    ]
                ]
            ]
        ]


viewTransferCard : LoggedIn.Model -> Transfer -> State -> Html Msg
viewTransferCard loggedIn transfer state =
    let
        originUser =
            case state of
                Received ->
                    transfer.to

                Transferred ->
                    transfer.from

        destinationUser =
            case state of
                Received ->
                    transfer.from

                Transferred ->
                    transfer.to

        viewUser_ =
            Profile.view loggedIn.shared.endpoints.ipfs loggedIn.accountName loggedIn.shared.translations
    in
    div [ class "flex flex-row w-full justify-center items-center bg-gray-100 px-6 pt-8 pb-6" ]
        [ div [ class "w-1/4" ] [ viewUser_ originUser ]
        , div [ class "w-1/2" ] [ viewAmount loggedIn transfer state ]
        , div [ class "w-1/4" ] [ viewUser_ destinationUser ]
        ]


viewAmount : LoggedIn.Model -> Transfer -> State -> Html Msg
viewAmount { shared } transfer state =
    let
        t =
            I18Next.t shared.translations

        head =
            case state of
                Received ->
                    div [ class "flex flex-row" ]
                        [ div [ class "" ]
                            [ div [ class "border border-solid border-green border-t-0 border-r-3 border-b-3 border-l-0 inline-block rotate-135" ]
                                []
                            ]
                        , div [ class "" ]
                            [ hr [ class "items-center border-sm border-dashed border-green" ] [] ]
                        ]

                Transferred ->
                    div [ class "flex flex-row" ]
                        [ div [ class "" ]
                            [ hr [ class "ml-5 border border-dashed border-green" ] [] ]
                        ]

        tail =
            case state of
                Received ->
                    div [ class "flex flex-row" ]
                        [ div [ class "" ]
                            [ hr [ class "border border-dashed border-green" ] [] ]
                        ]

                Transferred ->
                    div [ class "flex flex-row" ]
                        [ div [ class "" ]
                            [ hr [ class "items-center border border-dashed border-green " ] [] ]
                        , div [ class "" ]
                            [ div [ class "border border-solid border-green border-t-0 border-r-3 border-b-3 border-l-0 inline-block p-1 -rotate-45" ]
                                []
                            ]
                        ]
    in
    div [ class "flex flex-row justify-center" ]
        [ head
        , div [ class "w-32 border border-solid rounded-sm border-green bg-white px-4 py-1" ]
            [ p [ class "text-caption text-gray-900" ]
                [ text <|
                    case state of
                        Received ->
                            String.toUpper (t "transfer_result.received")

                        Transferred ->
                            String.toUpper (t "transfer_result.transferred")
                ]
            , div [ class "flex flex-row items-center" ]
                [ p [ class "text-lg font-semibold text-green" ]
                    [ text <|
                        String.fromFloat transfer.value
                    ]
                , span [ class "ml-2 text-caption text-green font-thin" ]
                    [ text <| Eos.symbolToString transfer.symbol ]
                ]
            ]
        , tail
        ]


viewDetails : LoggedIn.Model -> Transfer -> State -> Html Msg
viewDetails ({ shared } as loggedIn) transfer state =
    let
        t str =
            I18Next.t shared.translations str
                |> String.toUpper

        date =
            Just transfer.blockTime
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc
    in
    div [ class "flex flex-wrap mb-4 bg-white" ]
        [ div [ class "container mx-auto" ]
            [ div [ class "flex w-full" ]
                [ viewTransferCard loggedIn transfer state
                ]
            , div [ class "w-full mb-10 text-body" ]
                [ viewDetail (t "transfer_result.date") date
                , case transfer.memo of
                    Just memo ->
                        viewDetail (t "transfer_result.message") memo

                    Nothing ->
                        text ""
                ]
            ]
        ]


viewDetail : String -> String -> Html Msg
viewDetail title content =
    div [ class "m-4" ]
        [ h5 [ class "leading-tight text-caption mb-1 text-reward-green" ]
            [ text title ]
        , p [ class "text-lg text-body" ]
            [ text content ]
        ]



-- UPDATE


type Msg
    = CompletedTransferLoad (Result (Graphql.Http.Error (Maybe Transfer)) (Maybe Transfer))


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


findState : Maybe Transfer -> LoggedIn.Model -> State
findState maybeTransfer { accountName } =
    case maybeTransfer of
        Just transfer ->
            if transfer.from.account == accountName then
                Transferred

            else if transfer.to.account == accountName then
                Received

            else
                Transferred

        Nothing ->
            Transferred


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model user =
    case msg of
        CompletedTransferLoad (Ok transfer) ->
            let
                -- find out state either transferred or received
                state =
                    findState transfer user
            in
            { model | status = Loaded transfer state }
                |> UR.init

        CompletedTransferLoad (Err error) ->
            model
                |> updateStatus (LoadFailed error)
                |> UR.init
                |> UR.logGraphqlError msg error


updateStatus : Status -> Model -> Model
updateStatus status model =
    { model | status = status }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedTransferLoad r ->
            [ "CompletedTransferLoad", UR.resultToString r ]
