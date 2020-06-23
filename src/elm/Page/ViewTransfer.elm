module Page.ViewTransfer exposing (Model, Msg, init, msgToString, subscriptions, update, view)

import Api.Graphql
import Cambiatus.Scalar exposing (DateTime(..))
import Emoji
import Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, div, h2, h5, img, p, span, text)
import Html.Attributes exposing (class, src)
import I18Next
import Icons
import Page
import Profile
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
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
    | LoadFailed (Graphql.Http.Error (Maybe Transfer))
    | Loaded (Maybe Transfer) State



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        t =
            I18Next.t loggedIn.shared.translations

        title =
            case model.status of
                Loaded maybeTransfer _ ->
                    case maybeTransfer of
                        Just _ ->
                            t "transfer_result.title"

                        Nothing ->
                            ""

                _ ->
                    ""

        content =
            case model.status of
                Loading _ ->
                    Page.fullPageLoading

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
    in
    { title = title
    , content = content
    }


viewTransfer : LoggedIn.Model -> Transfer -> State -> Html Msg
viewTransfer loggedIn transfer state =
    let
        t =
            I18Next.t loggedIn.shared.translations
    in
    div [ class "flex w-full justify-center bg-green py-8" ]
        [ div [ class "flex-row w-full lg:w-2/3" ]
            [ div [ class "flex flex-wrap justify-center items-center" ]
                [ img
                    [ class "h-64 w-full lg:w-1/3"
                    , src "/images/transfer-doggo.svg"
                    ]
                    []
                , h2 [ class "w-full lg:w-2/3 mt-8 lg:px-8 text-center lg:text-left text-3xl font-medium text-white" ]
                    [ text <|
                        case state of
                            Transferred ->
                                t "transfer_result.transfer_success"

                            Received ->
                                t "transfer_result.receive_success"
                    ]
                , div
                    [ class "flex flex-wrap w-full p-4 mt-4 mx-2 bg-black bg-opacity-10 rounded-lg" ]
                    [ p [ class "w-full text-xs text-white uppercase font-bold" ]
                        [ text (t "transfer_result.transaction_id.title") ]
                    , p [ class "w-full text-xs text-white uppercase" ]
                        [ text (t "transfer_result.transaction_id.body") ]
                    , p [ class "w-full tracking-widest text-center text-4xl mt-4" ]
                        [ text (Emoji.encode transfer.createdTx) ]
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
            Profile.view loggedIn.shared loggedIn.accountName
    in
    div [ class "flex flex-row w-full justify-center items-center bg-gray-100 px-6 pt-8 pb-6" ]
        [ div [ class "w-1/6" ] [ viewUser_ originUser ]
        , div [ class "w-4/6" ] [ viewAmount loggedIn transfer state ]
        , div [ class "w-1/6" ] [ viewUser_ destinationUser ]
        ]


viewAmount : LoggedIn.Model -> Transfer -> State -> Html Msg
viewAmount { shared } transfer state =
    let
        t =
            I18Next.t shared.translations

        direction =
            case state of
                Received ->
                    "rotate-90"

                Transferred ->
                    "rotate--90"
    in
    div [ class "flex flex-row justify-center items-center" ]
        [ div [ class "w-1/4 flex  justify-end" ] [ Icons.arrowDown ("fill-current text-green " ++ direction) ]
        , div [ class "w-32 border border-solid rounded-sm border-green bg-white px-4 py-1 mx-2" ]
            [ p [ class "text-caption text-gray-900" ]
                [ text <|
                    case state of
                        Received ->
                            String.toUpper (t "transfer_result.received")

                        Transferred ->
                            String.toUpper (t "transfer_result.transferred")
                ]
            , div [ class "flex flex-row items-center" ]
                [ p [ class "text-heading font-semibold text-green" ]
                    [ text <|
                        String.fromFloat transfer.value
                    ]
                , span [ class "ml-2 text-caption text-green font-thin" ]
                    [ text <| Eos.symbolToString transfer.symbol ]
                ]
            ]
        , div [ class "w-1/4" ] [ Icons.arrowDown ("fill-current text-green " ++ direction) ]
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
            , div [ class "w-full mb-10 px-4" ]
                [ viewDetail (t "transfer_result.date") date
                , case transfer.memo of
                    Just memo ->
                        if String.length memo > 0 then
                            viewDetail (t "transfer_result.message") memo

                        else
                            text ""

                    Nothing ->
                        text ""
                , a [ class "button button-secondary w-full mt-10", Route.href Route.Dashboard ]
                    [ text (t "transfer_result.my_balance") ]
                ]
            ]
        ]


viewDetail : String -> String -> Html Msg
viewDetail title content =
    div [ class "my-4" ]
        [ h5 [ class "input-label mb-2" ]
            [ text title ]
        , p [ class "text-body" ]
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
