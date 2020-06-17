module Page.Notification exposing (Model, Msg(..), init, msgToString, update, view)

import Api.Graphql
import Browser exposing (Document)
import Eos
import Eos.Account as Eos
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Graphql.Http
import Html exposing (Html, div, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import I18Next exposing (Delims(..))
import Notification exposing (History, MintData, NotificationType(..), SaleHistoryData, TransferData)
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Strftime
import Time
import UpdateResult as UR
import Utils



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) =
    ( initModel
    , Api.Graphql.query shared (Notification.notificationHistoryQuery loggedIn.accountName) CompletedLoadNotificationHistory
    )



-- MODEL


type alias Model =
    Status


initModel : Model
initModel =
    Loading


type Status
    = Loading
    | Loaded (List History)


type Payload
    = T TransferData
    | S SaleHistoryData
    | M MintData



-- VIEW


view : LoggedIn.Model -> Model -> Document Msg
view loggedIn model =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        body =
            case model of
                Loading ->
                    Page.fullPageLoading

                Loaded notifications ->
                    div [ class "container mx-auto px-4 mb-6" ]
                        [ Page.viewTitle (t "notifications.title")
                        , if notifications == [] then
                            viewEmptyNotifications loggedIn.shared

                          else
                            viewNotifications loggedIn notifications
                        ]
    in
    Document
        (t "notifications.title")
        [ body ]


viewNotifications : LoggedIn.Model -> List History -> Html Msg
viewNotifications loggedIn notifications =
    div [ class "shadow-md rounded-lg bg-white mt-5" ]
        (List.map (viewNotification loggedIn) notifications)


viewNotification : LoggedIn.Model -> History -> Html Msg
viewNotification loggedIn notification =
    let
        isReadIndicator =
            if notification.isRead then
                ""

            else
                " bg-orange-100 first:rounded-t-lg last:rounded-b-lg"
    in
    case notification.payload of
        Transfer data ->
            div [ class ("border-b last:border-b-0 border-gray-500 hover:bg-gray-100 first-hover:rounded-t-lg last-hover:rounded-b-lg" ++ isReadIndicator) ]
                [ viewNotificationTransfer loggedIn.shared notification data ]

        Mint data ->
            div [ class ("border-b last:border-b-0 border-gray-500 hover:bg-gray-100 first-hover:rounded-t-lg last-hover:rounded-b-lg" ++ isReadIndicator) ]
                [ viewNotificationMint loggedIn.shared notification data ]

        SaleHistory data ->
            div [ class ("border-b last:border-b-0 border-gray-500 hover:bg-gray-100 first-hover:rounded-t-lg last-hover:rounded-b-lg" ++ isReadIndicator) ]
                [ viewNotificationSaleHistory loggedIn notification data ]


viewNotificationTransfer : Shared -> History -> TransferData -> Html Msg
viewNotificationTransfer shared history notification =
    let
        isReceive =
            Eos.nameToString history.recipientId /= notification.fromId

        amount =
            if isReceive then
                notification.amount

            else
                notification.amount * -1

        maybeLogo =
            if String.isEmpty notification.community.logo then
                Nothing

            else
                Just (shared.endpoints.ipfs ++ "/" ++ notification.community.logo)

        date =
            Just history.insertedAt
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc

        description =
            if isReceive then
                [ ( "user", notification.fromId )
                , ( "amount", String.fromFloat notification.amount )
                ]
                    |> I18Next.tr shared.translations I18Next.Curly "notifications.transfer.receive"

            else
                [ ( "user", notification.toId )
                , ( "amount", String.fromFloat notification.amount )
                ]
                    |> I18Next.tr shared.translations I18Next.Curly "notifications.transfer.sent"
    in
    div
        [ class "flex items-start lg:items-center p-4"
        , onClick (MarkAsRead history.id (T notification))
        ]
        [ div [ class "flex-none" ]
            [ case maybeLogo of
                Just logoUrl ->
                    img
                        [ class "w-10 h-10 object-scale-down"
                        , src logoUrl
                        ]
                        []

                Nothing ->
                    div
                        [ class "w-10 h-10 object-scale-down" ]
                        []
            ]
        , div [ class "flex-col flex-grow-1 pl-4" ]
            [ p
                [ class "font-sans text-black text-sm leading-relaxed" ]
                [ text description ]
            , p
                [ class "font-normal font-sans text-gray-900 text-caption uppercase" ]
                [ text date ]
            ]
        , div [ class "flex flex-none pl-4" ]
            (viewAmount amount notification.symbol)
        ]


viewNotificationMint : Shared -> History -> MintData -> Html Msg
viewNotificationMint shared history notification =
    let
        maybeLogo =
            if String.isEmpty notification.community.logo then
                Nothing

            else
                Just (shared.endpoints.ipfs ++ "/" ++ notification.community.logo)

        date =
            Just history.insertedAt
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc

        description =
            [ ( "amount", String.fromFloat notification.quantity )
            , ( "symbol", notification.community.symbol )
            ]
                |> I18Next.tr shared.translations I18Next.Curly "notifications.issue.receive"
    in
    div
        [ class "flex items-start lg:items-center p-4"
        , onClick (MarkAsRead history.id (M notification))
        ]
        [ div [ class "flex-none" ]
            [ case maybeLogo of
                Just logoUrl ->
                    img
                        [ class "w-10 h-10 object-scale-down"
                        , src logoUrl
                        ]
                        []

                Nothing ->
                    div
                        [ class "w-10 h-10 object-scale-down" ]
                        []
            ]
        , div [ class "flex-col flex-grow-1 pl-4" ]
            [ p
                [ class "font-sans text-black text-sm leading-relaxed" ]
                [ text description ]
            , p
                [ class "font-normal font-sans text-gray-900 text-caption uppercase" ]
                [ text date ]
            ]
        , div [ class "flex flex-none pl-4" ]
            (viewAmount notification.quantity notification.community.symbol)
        ]


viewNotificationSaleHistory : LoggedIn.Model -> History -> SaleHistoryData -> Html Msg
viewNotificationSaleHistory ({ shared } as loggedIn) notification sale =
    let
        maybeLogo =
            if String.isEmpty sale.community.logo then
                Nothing

            else
                Just (shared.endpoints.ipfs ++ "/" ++ sale.community.logo)

        date =
            Just notification.insertedAt
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc
    in
    div
        [ class "flex items-start lg:items-center p-4"
        , onClick (MarkAsRead notification.id (S sale))
        ]
        (div
            [ class "flex-none" ]
            [ case maybeLogo of
                Just logoUrl ->
                    img
                        [ class "w-10 h-10 object-scale-down"
                        , src logoUrl
                        ]
                        []

                Nothing ->
                    div
                        [ class "w-10 h-10 object-scale-down" ]
                        []
            ]
            :: viewNotificationSaleHistoryDetail loggedIn sale date
        )


viewNotificationSaleHistoryDetail : LoggedIn.Model -> SaleHistoryData -> String -> List (Html msg)
viewNotificationSaleHistoryDetail ({ shared } as loggedIn) sale date =
    let
        isBuy =
            loggedIn.accountName == sale.fromId

        description =
            if isBuy then
                [ ( "user", Eos.nameToString sale.toId )
                , ( "sale", sale.sale.title )
                ]
                    |> I18Next.tr shared.translations I18Next.Curly "notifications.saleHistory.buy"

            else
                [ ( "user", Eos.nameToString sale.fromId )
                , ( "sale", sale.sale.title )
                ]
                    |> I18Next.tr shared.translations I18Next.Curly "notifications.saleHistory.sell"
    in
    [ div [ class "flex-col flex-grow-1 pl-4" ]
        [ p
            [ class "font-sans text-black text-sm leading-relaxed" ]
            [ text description ]
        , p
            [ class "font-normal font-sans text-gray-900 text-caption uppercase" ]
            [ text date ]
        ]
    , div [ class "flex flex-none pl-4" ]
        [ img
            [ src (shared.endpoints.ipfs ++ "/" ++ Maybe.withDefault "" sale.sale.image)
            , class "object-scale-down rounded-full h-10"
            ]
            []
        ]
    ]


viewAmount : Float -> String -> List (Html msg)
viewAmount amount symbol =
    let
        amountText =
            FormatNumber.format usLocale amount

        color =
            if amount > 0 then
                "text-green"

            else
                "text-red"
    in
    [ div [ class "text-2xl", class color ] [ text amountText ]
    , div [ class "uppercase text-sm font-thin mt-3 ml-2 font-sans", class color ] [ text symbol ]
    ]


viewEmptyNotifications : Shared -> Html msg
viewEmptyNotifications shared =
    let
        t s =
            I18Next.t shared.translations s
    in
    div
        [ class "rounded-lg bg-white mt-5 p-8 text-center shadow" ]
        [ p [] [ text (t "notifications.empty") ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoadNotificationHistory (Result (Graphql.Http.Error (List History)) (List History))
    | MarkAsRead Int Payload
    | CompletedReading (Result (Graphql.Http.Error History) History)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadNotificationHistory (Ok notifications) ->
            Loaded notifications
                |> UR.init

        CompletedLoadNotificationHistory (Err err) ->
            UR.init model
                |> UR.logGraphqlError msg err

        MarkAsRead notificationId data ->
            let
                cmd =
                    Api.Graphql.mutation loggedIn.shared
                        (Notification.markAsReadMutation notificationId)
                        CompletedReading
            in
            case data of
                T transfer ->
                    model
                        |> UR.init
                        |> UR.addCmd cmd
                        |> UR.addCmd
                            (Route.ViewTransfer transfer.id
                                |> Route.replaceUrl loggedIn.shared.navKey
                            )

                M _ ->
                    model
                        |> UR.init
                        |> UR.addCmd cmd

                S sale ->
                    model
                        |> UR.init
                        |> UR.addCmd cmd
                        |> UR.addCmd
                            (Route.ViewSale (String.fromInt sale.sale.id)
                                |> Route.replaceUrl loggedIn.shared.navKey
                            )

        CompletedReading (Ok hist) ->
            case model of
                Loaded histories ->
                    let
                        updatedHistories =
                            List.map
                                (\h ->
                                    if h.id == hist.id then
                                        { h | isRead = True }

                                    else
                                        h
                                )
                                histories
                    in
                    Loaded updatedHistories
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        CompletedReading (Err e) ->
            model
                |> UR.init
                |> UR.logGraphqlError msg e


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadNotificationHistory r ->
            [ "CompletedLoadNotificationHistory", UR.resultToString r ]

        MarkAsRead _ _ ->
            [ "MarkAsRead" ]

        CompletedReading r ->
            [ "CompletedReading", UR.resultToString r ]
