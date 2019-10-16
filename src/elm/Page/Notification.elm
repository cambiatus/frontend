module Page.Notification exposing (..)

import Api.Graphql
import Eos.Account as Eos
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, src)
import I18Next exposing (Delims(..), t)
import Notification exposing (History, NotificationType(..), SaleHistoryData, TransferData)
import Page
import Route exposing (Route)
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared exposing (Shared)
import Strftime
import Time exposing (Posix)
import UpdateResult as UR
import Utils



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) =
    ( initModel loggedIn
    , Api.Graphql.query shared (Notification.notificationHistoryQuery loggedIn.accountName) CompletedLoadNotificationHistory
    )



-- MODEL


type alias Model =
    { status : Status
    }


initModel : LoggedIn.Model -> Model
initModel loggedIn =
    { status = Loading }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (List History))
    | Loaded (List History)



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    case model.status of
        Loading ->
            Page.fullPageLoading

        LoadingFailed e ->
            Page.fullPageGraphQLError (t "notifications.title") e

        Loaded notifications ->
            div [ class "container mx-auto px-4 mb-6" ]
                [ Page.viewTitle (t "notifications.title")
                , if notifications == [] then
                    viewEmptyNotifications loggedIn.shared

                  else
                    viewNotifications loggedIn notifications
                ]


viewNotifications : LoggedIn.Model -> List History -> Html msg
viewNotifications loggedIn notifications =
    div [ class "shadow-md rounded-lg bg-white mt-5" ]
        (List.map (viewNotification loggedIn) notifications)


viewNotification : LoggedIn.Model -> History -> Html msg
viewNotification loggedIn notification =
    case notification.payload of
        Transfer data ->
            div [ class "border-b last:border-b-0 border-gray-500 hover:bg-gray-100 first-hover:rounded-t-lg last-hover:rounded-b-lg" ]
                [ viewNotificationTransfer loggedIn.shared notification data ]

        SaleHistory data ->
            div [ class "border-b last:border-b-0 border-gray-500 hover:bg-gray-100 first-hover:rounded-t-lg last-hover:rounded-b-lg" ]
                [ viewNotificationSaleHistory loggedIn notification data ]


viewNotificationTransfer : Shared -> History -> TransferData -> Html msg
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
    div [ class "flex items-start lg:items-center p-4" ]
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


viewNotificationSaleHistory : LoggedIn.Model -> History -> SaleHistoryData -> Html msg
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
    a
        [ class "flex items-start lg:items-center p-4"
        , Route.href (Route.ViewSale (String.fromInt sale.sale.id))
        ]
        ([ div
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
         ]
            ++ viewNotificationSaleHistoryDetail loggedIn sale date
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

        amount =
            if isBuy then
                -sale.amount

            else
                sale.amount
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


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadNotificationHistory (Ok notifications) ->
            { model | status = Loaded notifications }
                |> UR.init

        CompletedLoadNotificationHistory (Err err) ->
            UR.init model
                |> UR.logGraphqlError msg err


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadNotificationHistory r ->
            [ "CompletedLoadNotificationHistory", UR.resultToString r ]
