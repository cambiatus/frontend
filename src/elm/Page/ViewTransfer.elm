module Page.ViewTransfer exposing (Model, Msg, init, msgToString, update, view)

import Cambiatus.Enum.TransferDirectionValue as TransferDirectionValue exposing (TransferDirectionValue)
import Emoji
import Eos
import Eos.Explorer
import Graphql.Http
import Html exposing (Html, a, div, h2, img, p, span, text)
import Html.Attributes exposing (alt, class, href, src, target)
import Icons
import Markdown
import Page
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Transfer exposing (Transfer, transferQuery)
import UpdateResult as UR
import Utils
import View.Components



-- INIT


init : LoggedIn.Model -> Int -> UpdateResult
init loggedIn transferId =
    let
        model =
            { status = Loading
            , transferId = transferId
            }
    in
    UR.init model
        |> UR.addExt
            (LoggedIn.query loggedIn
                (transferQuery transferId)
                CompletedTransferLoad
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
                            div [ class "flex-grow flex flex-col" ]
                                [ Page.viewHeader loggedIn (t "transfer_result.title")
                                , div [ class "relative flex-grow flex items-center" ]
                                    [ div [ class "bg-purple-500 absolute top-0 bottom-0 left-0 right-1/2 hidden lg:block" ] []
                                    , div [ class "bg-white absolute top-0 bottom-0 left-1/2 right-0 hidden lg:block" ] []
                                    , div
                                        [ class "relative flex-grow grid grid-cols-1 mb-10 lg:container lg:mx-auto lg:px-4 lg:grid-cols-2 lg:py-10 lg:mb-0"
                                        , Html.Attributes.id "content-container"
                                        ]
                                        [ viewTransfer loggedIn transfer direction
                                        , viewDetails loggedIn transfer profileSummaries direction
                                        ]
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
    div [ class "isolate text-white text-center bg-purple-500 pt-4 pb-6 lg:pb-0 lg:pt-0 lg:w-2/3 lg:mx-auto" ]
        [ div [ class "container mx-auto px-4 lg:mx-0 lg:px-0 lg:max-w-none" ]
            [ img
                [ class "h-64 w-full"
                , src "/images/transfer-doggo.svg"
                , alt ""
                ]
                []
            , h2 [ class "font-bold text-xl mt-7 mb-4" ]
                [ text <|
                    case direction of
                        TransferDirectionValue.Sending ->
                            t "transfer_result.transfer_success"

                        TransferDirectionValue.Receiving ->
                            t "transfer_result.receive_success"
                ]
            , p []
                [ text (t "transfer_result.transaction_id.body") ]
            , p [ class "text-xl md:text-2xl mt-4" ]
                [ text (Emoji.encode transfer.createdTx) ]
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
    div [ class "bg-white pt-6 pb-10 lg:pt-0 lg:pb-0 lg:w-2/3 lg:mx-auto lg:self-center" ]
        [ div [ class "container mx-auto px-4 lg:mx-0 lg:px-0 lg:max-w-none" ]
            [ viewTransferCard loggedIn
                transfer
                direction
                profileSummaries
                GotProfileSummaryMsg
                [ class "pb-6 border-b border-gray-500" ]
            , viewDetail [ class "mt-6" ]
                (t "transfer_result.date")
                (p []
                    [ View.Components.dateViewer []
                        identity
                        shared
                        (Utils.fromDateTime transfer.blockTime)
                    , View.Components.dateViewer []
                        (\translations ->
                            { translations
                                | today = Just ", {{date}}"
                                , yesterday = Just ", {{date}}"
                                , other = ""
                            }
                        )
                        shared
                        (Utils.fromDateTime transfer.blockTime)
                    ]
                )
            , case transfer.memo of
                Nothing ->
                    text ""

                Just memo ->
                    if memo == Markdown.empty then
                        text ""

                    else
                        viewDetail [ class "mt-6" ]
                            (t "transfer_result.message")
                            (Markdown.view [] memo)
            , a
                [ class "button button-primary w-full mt-6"
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
            "fill-current text-black " ++ rotateClass

        viewSummary profile summary =
            summary
                |> Profile.Summary.withNameBg False
                |> Profile.Summary.withRelativeSelector "#content-container"
                |> Profile.Summary.view loggedIn.shared.translators loggedIn.accountName profile
                |> Html.map (profileSummaryToMsg (profile == leftProfile))
    in
    div
        (class "flex justify-between"
            :: attrs
        )
        [ viewSummary leftProfile profileSummaries.left
        , div [ class "flex mt-2 space-x-2 justify-self-center" ]
            [ Icons.arrowDown arrowClass
            , div [ class "flex flex-col px-2 font-bold" ]
                [ span [ class "text-black text-sm uppercase" ]
                    [ text <|
                        case transferDirection of
                            TransferDirectionValue.Receiving ->
                                t "transfer_result.received"

                            TransferDirectionValue.Sending ->
                                t "transfer_result.transferred"
                    ]
                , span [ class "text-green" ]
                    [ Eos.assetToString loggedIn.shared.translators
                        { amount = transfer.value
                        , symbol = transfer.community.symbol
                        }
                        |> text
                    ]
                ]
            , Icons.arrowDown arrowClass
            ]
        , viewSummary rightProfile profileSummaries.right
        ]


viewDetail : List (Html.Attribute Msg) -> String -> Html Msg -> Html Msg
viewDetail containerAttrs title content =
    div containerAttrs
        [ p [ class "label" ] [ text title ]
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
