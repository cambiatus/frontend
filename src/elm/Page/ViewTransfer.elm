module Page.ViewTransfer exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Cambiatus.Enum.TransferDirectionValue as TransferDirectionValue exposing (TransferDirectionValue)
import Cambiatus.Scalar exposing (DateTime(..))
import Emoji
import Eos
import Eos.Explorer
import Graphql.Http
import Html exposing (Html, a, div, h2, h5, img, p, span, text)
import Html.Attributes exposing (class, href, src, target)
import Icons
import Markdown
import Page
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Transfer exposing (Transfer, transferQuery)
import UpdateResult as UR
import Utils
import View.Components



-- INIT


init : LoggedIn.Model -> Int -> ( Model, Cmd Msg )
init { shared, authToken } transferId =
    let
        model =
            { status = Loading
            , transferId = transferId
            }
    in
    ( model
    , Api.Graphql.query shared (Just authToken) (transferQuery transferId) CompletedTransferLoad
    )



-- MODEL


type alias Model =
    { status : Status
    , transferId : Int
    }


type Status
    = Loading
    | LoadFailed (Graphql.Http.Error (Maybe Transfer))
    | Loaded (Maybe Transfer) TransferDirectionValue Transfer.ProfileSummaries



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            case model.status of
                Loaded maybeTransfer _ _ ->
                    case maybeTransfer of
                        Just _ ->
                            t "transfer_result.title"

                        Nothing ->
                            ""

                _ ->
                    ""

        content =
            case model.status of
                Loading ->
                    Page.fullPageLoading loggedIn.shared

                LoadFailed error ->
                    div []
                        [ Page.viewHeader loggedIn (t "transfer_result.title")
                        , Page.fullPageGraphQLError (t "transfer_result.title") error
                        ]

                Loaded maybeTransfer direction profileSummaries ->
                    case maybeTransfer of
                        Just transfer ->
                            div []
                                [ Page.viewHeader loggedIn (t "transfer_result.title")
                                , div []
                                    [ viewTransfer loggedIn transfer direction
                                    , viewDetails loggedIn transfer profileSummaries direction
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


viewTransfer : LoggedIn.Model -> Transfer -> TransferDirectionValue -> Html Msg
viewTransfer loggedIn transfer direction =
    let
        t =
            loggedIn.shared.translators.t
    in
    div [ class "flex w-full justify-center bg-green py-8" ]
        [ div [ class "flex-row w-full lg:w-2/3" ]
            [ div [ class "flex flex-wrap justify-center items-center" ]
                [ img
                    [ class "h-64 w-full lg:w-1/3"
                    , src "/images/transfer-doggo.svg"
                    ]
                    []
                , h2 [ class "w-full lg:w-2/3 mt-8 lg:px-8 text-center lg:text-left text-3xl font-semibold text-white" ]
                    [ text <|
                        case direction of
                            TransferDirectionValue.Sending ->
                                t "transfer_result.transfer_success"

                            TransferDirectionValue.Receiving ->
                                t "transfer_result.receive_success"
                    ]
                , div
                    [ class "flex flex-wrap w-full p-4 mt-4 mx-2 bg-black bg-opacity-10 rounded-lg" ]
                    [ p [ class "w-full text-sm text-white uppercase font-bold" ]
                        [ text (t "transfer_result.transaction_id.title") ]
                    , p [ class "w-full text-sm text-white uppercase" ]
                        [ text (t "transfer_result.transaction_id.body") ]
                    , p [ class "w-full tracking-widest text-center text-4xl mt-4" ]
                        [ text (Emoji.encode transfer.createdTx) ]
                    ]
                ]
            ]
        ]


viewDetails :
    LoggedIn.Model
    -> Transfer
    -> Transfer.ProfileSummaries
    -> TransferDirectionValue
    -> Html Msg
viewDetails ({ shared } as loggedIn) transfer profileSummaries direction =
    let
        t str =
            shared.translators.t str
                |> String.toUpper
    in
    div [ class "flex flex-wrap mb-4 bg-white" ]
        [ div [ class "container mx-auto" ]
            [ viewTransferCard loggedIn
                transfer
                direction
                profileSummaries
                GotProfileSummaryMsg
                [ class "w-full px-4 bg-gray-100 py-6 my-8 md:px-20" ]
            , div [ class "w-full mb-10 px-4" ]
                [ viewDetail (t "transfer_result.date")
                    (View.Components.dateViewer []
                        identity
                        shared
                        (Utils.fromDateTime transfer.blockTime)
                    )
                , case transfer.memo of
                    Just memo ->
                        if memo == Markdown.empty then
                            text ""

                        else
                            Markdown.view [] memo
                                |> viewDetail (t "transfer_result.message")

                    Nothing ->
                        text ""
                , a
                    [ class "button button-primary w-full mt-10"
                    , Eos.Explorer.Transfer transfer.createdTx
                        |> Eos.Explorer.link shared.environment
                        |> href
                    , target "_blank"
                    ]
                    [ text (t "block_explorer.see") ]
                , a [ class "button button-secondary w-full mt-4", Route.href Route.Dashboard ]
                    [ text (t "transfer_result.my_balance") ]
                ]
            ]
        ]


viewTransferCard :
    LoggedIn.Model
    -> Transfer
    -> TransferDirectionValue
    -> Transfer.ProfileSummaries
    -> (Bool -> Profile.Summary.Msg -> msg)
    -> List (Html.Attribute msg)
    -> Html msg
viewTransferCard loggedIn transfer transferDirection profileSummaries profileSummaryToMsg attrs =
    let
        { t } =
            loggedIn.shared.translators

        ( leftProfile, rightProfile, rotateClass ) =
            case transferDirection of
                TransferDirectionValue.Receiving ->
                    ( transfer.to, transfer.from, "rotate-90" )

                TransferDirectionValue.Sending ->
                    ( transfer.from, transfer.to, "-rotate-90" )

        arrowClass =
            "fill-current text-green " ++ rotateClass

        viewSummary profile summary =
            Profile.Summary.view loggedIn.shared.translators loggedIn.accountName profile summary
                |> Html.map (profileSummaryToMsg (profile == leftProfile))
    in
    div
        (class "grid grid-cols-5 place-items-center"
            :: attrs
        )
        [ viewSummary leftProfile profileSummaries.left
        , div [ class "col-span-3 flex items-center space-x-2 md:space-x-3" ]
            [ Icons.arrowDown arrowClass
            , div [ class "bg-white border border-green rounded-label px-3 pb-1 min-w-30" ]
                [ span [ class "text-gray-900 text-sm uppercase" ]
                    [ text <|
                        case transferDirection of
                            TransferDirectionValue.Receiving ->
                                t "transfer_result.received"

                            TransferDirectionValue.Sending ->
                                t "transfer_result.transferred"
                    ]
                , div [ class "flex text-green" ]
                    [ span [ class "font-semibold text-lg" ]
                        [ transfer.value
                            |> Eos.formatSymbolAmount loggedIn.shared.translators
                                transfer.community.symbol
                            |> text
                        ]
                    , span [ class "text-sm ml-2 mb-2" ]
                        [ transfer.community.symbol
                            |> Eos.symbolToSymbolCodeString
                            |> text
                        ]
                    ]
                ]
            , Icons.arrowDown arrowClass
            ]
        , viewSummary rightProfile profileSummaries.right
        ]


viewDetail : String -> Html Msg -> Html Msg
viewDetail title content =
    div [ class "my-4" ]
        [ h5 [ class "label" ] [ text title ]
        , p [] [ content ]
        ]



-- UPDATE


type Msg
    = CompletedTransferLoad (RemoteData (Graphql.Http.Error (Maybe Transfer)) (Maybe Transfer))
    | GotProfileSummaryMsg Bool Profile.Summary.Msg


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


findDirection : Maybe Transfer -> LoggedIn.Model -> TransferDirectionValue
findDirection maybeTransfer { accountName } =
    case maybeTransfer of
        Just transfer ->
            if transfer.to.account == accountName then
                TransferDirectionValue.Receiving

            else
                TransferDirectionValue.Sending

        Nothing ->
            TransferDirectionValue.Sending


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model user =
    case msg of
        CompletedTransferLoad (RemoteData.Success transfer) ->
            let
                direction =
                    findDirection transfer user

                summary =
                    Profile.Summary.init False
            in
            { model
                | status =
                    Loaded transfer
                        direction
                        { left = summary, right = summary }
            }
                |> UR.init

        CompletedTransferLoad (RemoteData.Failure error) ->
            model
                |> updateStatus (LoadFailed error)
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just user.accountName)
                    "Got an error when loading transfer"
                    { moduleName = "Page.ViewTransfer", function = "update" }
                    []
                    error

        CompletedTransferLoad _ ->
            UR.init model

        GotProfileSummaryMsg isLeft subMsg ->
            case model.status of
                Loaded maybeTransfer direction profileSummaries ->
                    { model
                        | status =
                            Transfer.updateProfileSummaries profileSummaries isLeft subMsg
                                |> Loaded maybeTransfer direction
                    }
                        |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Got a Profile.Summary.Msg, but transfer wasn't loaded"
                            (Just user.accountName)
                            { moduleName = "Page.ViewTransfer", function = "update" }
                            []


updateStatus : Status -> Model -> Model
updateStatus status model =
    { model | status = status }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedTransferLoad r ->
            [ "CompletedTransferLoad", UR.remoteDataToString r ]

        GotProfileSummaryMsg _ subMsg ->
            "GotProfileSummaryMsg" :: Profile.Summary.msgToString subMsg
