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
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { status : Status
    }


type State
    = Transferred
    | Received
    | NotInvolved -- if the user looking at the transaction is not involved


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
                        [ viewHeader loggedIn
                        , viewDoggo loggedIn transfer state
                        , viewCommunity loggedIn transfer
                        ]

                Nothing ->
                    div [ class "container mx-auto px-4" ]
                        [ div []
                            [ text "Could not load the sale" ]
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


viewDoggo : LoggedIn.Model -> Transfer -> State -> Html Msg
viewDoggo loggedIn transfer state =
    let
        t =
            I18Next.t loggedIn.shared.translations
    in
    div [ class "static flex" ]
        [ div [ class "w-full bg-green h-50" ]
            [ div [ class "flex-row" ]
                [ div [ class "px-4 py-2 m-2" ]
                    [ h2 [ class "text-center mt-8 font-medium font-sans text-white not-italic" ]
                        [ text <|
                            case state of
                                Transferred ->
                                    t "transfer_result.transfer_success"

                                Received ->
                                    t "transfer_result.receive_success"

                                NotInvolved ->
                                    t "transfer_result.transfer_success"
                        ]
                    ]
                , div [ class "bg-no-repeat bg-auto h-64 ml-32 -mt-5 px-4 py-2 m-2", style "background-image" "url(/images/transfer-doggo.svg)" ]
                    []
                , div [ class "flex justify-center items-center -mt-16 absolute md:w-full mx-4 md:mx-auto" ]
                    [ viewTransferCard loggedIn transfer state
                    ]
                ]
            ]
        ]


viewTransferCard : LoggedIn.Model -> Transfer -> State -> Html Msg
viewTransferCard loggedIn transfer state =
    let
        ipfsUrl =
            loggedIn.shared.endpoints.ipfs
    in
    div [ class "flex flex-row inline-block rounded overflow-auto bg-gray-100" ]
        [ div [ class "px-4 py-2 m-2" ]
            [ div [ class "h-8 w-8 rounded-full mx-auto mt-5" ]
                [ Avatar.view ipfsUrl
                    (case state of
                        Received ->
                            transfer.from.avatar

                        Transferred ->
                            transfer.from.avatar

                        NotInvolved ->
                            transfer.from.avatar
                    )
                    "h-10 w-10"
                ]
            , div [ class "px-6 py-4" ]
                [ span [ class "text-base inline-block bg-black rounded-full px-3 py-1 text-sm font-semibold text-white" ]
                    [ case state of
                        Received ->
                            text "You"

                        Transferred ->
                            text "You"

                        NotInvolved ->
                            Eos.viewName transfer.fromId
                    ]
                ]
            ]
        , div [ class "mt-5" ] [ viewAmount loggedIn transfer state ]
        , div [ class "-ml-20 px-4 py-2 m-2" ]
            [ div [ class "h-8 w-8 rounded-full mx-auto mt-5" ]
                [ Avatar.view ipfsUrl
                    (case state of
                        Received ->
                            transfer.from.avatar

                        Transferred ->
                            transfer.to.avatar

                        NotInvolved ->
                            transfer.to.avatar
                    )
                    "h-10 w-10"
                ]
            , div [ class "px-6 py-4" ]
                [ span [ class "inline-block bg-black rounded-full px-3 py-1 text-sm font-semibold text-white" ]
                    [ Eos.viewName <|
                        case state of
                            Received ->
                                transfer.fromId

                            Transferred ->
                                transfer.toId

                            NotInvolved ->
                                transfer.toId
                    ]
                ]
            ]
        ]


viewAmount : LoggedIn.Model -> Transfer -> State -> Html Msg
viewAmount { shared } transfer state =
    let
        t =
            I18Next.t shared.translations

        head =
            case state of
                Received ->
                    div [ class "px-4 py-2 m-2 " ]
                        [ i [ class "mt-5 left" ]
                            []
                        ]

                _ ->
                    div [ class "px-4 py-2 m-2" ]
                        [ hr [ class "ml-5 border border-dashed border-green w-8 mt-6 m-auto mb-6" ] [] ]

        tail =
            case state of
                Received ->
                    div [ class "px-4 py-2 m-2" ]
                        [ hr [ class "ml-5 border border-dashed border-green w-8 mt-6 m-auto mb-6" ] [] ]

                _ ->
                    div [ class "flex flex row" ]
                        [ div [ class "px-4 py-2 m-2" ]
                            [ hr [ class "-ml-8 border border-dashed border-green w-6 mt-6 m-auto mb-6" ] [] ]
                        , div [ class "px-4 py-2 m-2" ]
                            [ i [ class "-ml-12 mt-5 right" ] [] ]
                        ]
    in
    div [ class "-ml-16 flex flex-row mt-5" ]
        [ head
        , div [ class "px-4 py-2 m-2" ]
            [ div [ class "-ml-10 border border-solid rounded border-green bg-white" ]
                [ div [ class "ml-1" ]
                    [ p [ class "mt-2 text-caption font-hairline text-gray-900" ]
                        [ text <|
                            case state of
                                Received ->
                                    String.toUpper (t "transfer_result.received")

                                Transferred ->
                                    String.toUpper (t "transfer_result.transferred")

                                NotInvolved ->
                                    String.toUpper (t "transfer_result.transferred")
                        ]
                    , div [ class "flex flex-row" ]
                        [ p [ class "font-medium text-green" ]
                            [ text <|
                                String.fromFloat transfer.value
                            ]
                        , span [ class "ml-2 text-sm text-green mt-1 mb-1 font-thin mr-5" ]
                            [ text <| Eos.symbolToString transfer.symbol ]
                        ]
                    ]
                ]
            ]
        , tail
        ]


viewCommunity : LoggedIn.Model -> Transfer -> Html Msg
viewCommunity { shared } transfer =
    let
        t str =
            I18Next.t shared.translations str
                |> String.toUpper

        date =
            Just transfer.blockTime
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc
    in
    div [ class "flex mb-4 bg-white" ]
        [ div [ class "w-full h-50 mt-20 mb-10" ]
            [ viewRest (t "transfer_result.community") <| Eos.symbolToString transfer.symbol
            , viewRest (t "transfer_result.date") date
            , viewRest (t "transfer_result.message") <| Maybe.withDefault "" transfer.memo
            ]
        ]


viewRest : String -> String -> Html Msg
viewRest title content =
    div [ class "mt-5 ml-16" ]
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
            if transfer.fromId == accountName then
                Transferred

            else if transfer.toId == accountName then
                Received

            else
                NotInvolved

        Nothing ->
            NotInvolved


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
