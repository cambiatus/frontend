module Page.Transfer exposing (Model, Msg, init, msgToString, subscriptions, update, view)

import Api
import Api.Graphql
import Avatar
import Bespiral.Scalar exposing (DateTime(..))
import Eos exposing (Symbol, symbolFromString)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy as Lazy
import I18Next
import Icons
import Page
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Strftime
import Time
import Transfer exposing (Transfer, transferQuery)
import UpdateResult as UR
import User exposing (User, view)
import Utils



-- INIT


init : LoggedIn.Model -> Int -> ( Model, Cmd Msg )
init { shared } transferId =
    let
        currentStatus =
            Loading transferId

        model =
            { status = currentStatus }
    in
    ( model, initCmd shared currentStatus )


initCmd : Shared -> Status -> Cmd Msg
initCmd shared status =
    case status of
        Loading transferId ->
            Api.Graphql.query shared (transferQuery transferId) CompletedTransferLoad

        _ ->
            Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    { status : Status
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
                [ Lazy.lazy viewHeader loggedIn
                , div []
                    [ text (invalidId ++ t "transfer_result.errors.invalid_id") ]
                ]

        LoadFailed error ->
            div []
                [ viewHeader loggedIn
                , Page.fullPageGraphQLError (t "transfer_result.title") error
                ]

        Loaded maybeTransfer state ->
            case maybeTransfer of
                Just transfer ->
                    div []
                        [ Lazy.lazy viewHeader loggedIn
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


viewHeader : LoggedIn.Model -> Html Msg
viewHeader ({ shared } as loggedIn) =
    let
        t =
            I18Next.t loggedIn.shared.translations
    in
    div [ class "h-16 w-full bg-indigo-500 flex px-4 items-center" ]
        [ a
            [ class "items-center flex absolute"
            , Route.href Route.Dashboard
            ]
            [ Icons.back ""
            , p [ class "text-white text-sm ml-2" ]
                [ text (I18Next.t shared.translations "back")
                ]
            ]
        , p [ class "text-white mx-auto" ] [ text (t "transfer_result.title") ]
        ]


viewTransfer : LoggedIn.Model -> Transfer -> State -> Html Msg
viewTransfer loggedIn transfer state =
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
                , h2 [ class "w-full lg:w-2/3 mt-8 mb-20 lg:px-8 text-center lg:text-left text-3xl font-medium font-sans text-white" ]
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
        ipfsUrl =
            loggedIn.shared.endpoints.ipfs

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
            User.view loggedIn.shared.endpoints.ipfs loggedIn.accountName loggedIn.shared.translations
    in
    div [ class "flex flex-row w-full justify-center items-center py-5 rounded bg-gray-100" ]
        [ viewUser_ originUser
        , viewAmount loggedIn transfer state
        , viewUser_ destinationUser
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
                        [ div [ class "pr-0 pl-1 py-2 m-2 " ]
                            [ div [ class "border border-solid border-green border-t-0 border-r-3 border-b-3 border-l-0 inline-block p-1 sm:ml-5 ml-10 mt-5 rotate-135" ]
                                []
                            ]
                        , div [ class "pl-3 pr-3 py-2 m-2" ]
                            [ hr [ class "sm:-ml-6 -ml-5 items-center border border-dashed border-green m-auto w-6 mt-6" ] [] ]
                        ]

                Transferred ->
                    div [ class "flex flex-row" ]
                        [ div [ class "px-4 py-2 m-2" ]
                            [ hr [ class "ml-5 border border-dashed border-green w-8 mt-6 m-auto mb-6" ] [] ]
                        ]

        tail =
            case state of
                Received ->
                    div [ class "flex flex-row" ]
                        [ div [ class "pl-2 pr-10 py-2 m-2" ]
                            [ hr [ class "sm:-ml-8 -ml-10 border border-dashed border-green sm:w-8 w-6 mt-6 m-auto mb-6" ] [] ]
                        ]

                Transferred ->
                    div [ class "flex flex-row" ]
                        [ div [ class "pl-2 pr-2 py-2 m-2" ]
                            [ hr [ class "-ml-8 items-center border border-dashed border-green w-6 mt-6 m-auto mb-6" ] [] ]
                        , div [ class "px-4 py-2 m-2" ]
                            [ div [ class "border border-solid border-green border-t-0 border-r-3 border-b-3 border-l-0 inline-block p-1 -rotate-45 -ml-12 mt-5" ]
                                []
                            ]
                        ]
    in
    div [ class "flex flex-row" ]
        [ head
        , div [ class "px-4 py-2 m-2" ]
            [ div [ class "-ml-10 border border-solid rounded border-green bg-white" ]
                [ div [ class "ml-1" ]
                    [ p [ class "pt-1 text-caption font-hairline text-gray-900 pl-1" ]
                        [ text <|
                            case state of
                                Received ->
                                    String.toUpper (t "transfer_result.received")

                                Transferred ->
                                    String.toUpper (t "transfer_result.transferred")
                        ]
                    , div [ class "flex flex-row" ]
                        [ p [ class "pl-1 pr-5 text-lg font-bold text-green" ]
                            [ text <|
                                String.fromFloat transfer.value
                            ]
                        , span [ class "ml-2 text-caption text-green mt-1 mb-1 font-thin pr-3 pl-3" ]
                            [ text <| Eos.symbolToString transfer.symbol ]
                        ]
                    ]
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
        [ div [ class "flex w-full lg:w-2/3 m-4 lg:mx-auto lg:-mt-20" ]
            [ viewTransferCard loggedIn transfer state
            ]
        , div [ class "w-full mb-10" ]
            [ viewDetail (t "transfer_result.community") transfer.community.name
            , viewDetail (t "transfer_result.date") date
            , viewDetail (t "transfer_result.message") <| Maybe.withDefault "" transfer.memo
            ]
        ]


viewDetail : String -> String -> Html Msg
viewDetail title content =
    div [ class "m-4" ]
        [ h5 [ class "leading-tight text-xs mb-1 text-reward-green" ]
            [ text title ]
        , p [ class "text-lg font-sans not-italic" ]
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
