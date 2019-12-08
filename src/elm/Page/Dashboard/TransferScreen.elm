module Page.Dashboard.TransferScreen exposing (Model, Msg, init, msgToString, subscriptions, update, view)

import Api
import Api.Graphql
import Avatar
import Bespiral.Scalar exposing (DateTime(..))
import Eos exposing (Symbol, symbolFromString)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import I18Next
import Icons
import Html.Lazy as Lazy
import Page
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Time
import Transfer exposing (Transfer, transferQuery)
import UpdateResult as UR



-- INIT


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init { shared } transferId =
    let
        currentStatus =
            initStatus transferId

        model =
            { status = currentStatus }
    in
    ( model, initCmd shared currentStatus )


initStatus : String -> Status
initStatus transferId =
    case String.toInt transferId of
        Just tID ->
            Loading tID

        Nothing ->
            InvalidId transferId


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
            div [class "container mx-auto px-4"]
                [ Lazy.lazy viewHeader loggedIn
                , div []
                    [ text (invalidId ++ (t "transferscreen.errors.invalid_id")) ]
                ]

        LoadFailed error ->
            div []
                [ viewHeader loggedIn
                , Page.fullPageGraphQLError (t "transferscreen.title") error
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
        , p [ class "text-white mx-auto" ] [ text (t "transferscreen.title") ]
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
                    [ img [ class "ml-32", src "%PUBLIC_URL%/images/transfer-doggo.svg" ] [] ]
                , div [ class "px-4 py-2 m-2" ]
                    [ p [ class "font-medium font-sans text-white not-italic max-w-sm" ]
                        [ text <|
                            case state of
                                Transferred ->
                                    t "transferscreen.transfer_success"

                                Received ->
                                    t "transferscreen.receive_success"

                                NotInvolved ->
                                    t "transferscreen.transfer_success"
                        ]
                    ]
                , div [ class "ml-84 absolute items-center -mt-8 h-30" ]
                    [ viewTransferCard loggedIn transfer state
                    ]
                ]
            ]
        ]


viewTransferCard : LoggedIn.Model -> Transfer -> State -> Html Msg
viewTransferCard loggedIn transfer state =
    let
        avatar =
            case LoggedIn.profile loggedIn of
                Just profile ->
                    profile.avatar

                Nothing ->
                    Avatar.empty
    in
    div [ class "flex flex-row max-w-sm w-auto inline-block rounded overflow-auto bg-gray-100" ]
        [ div [ class "px-4 py-2 m-2" ]
            [ div [ class "h-8 w-8 rounded-full mx-auto" ]
                [ Avatar.view "" avatar ""
                ]
            , div [ class "px-6 py-4" ]
                [ span [ class "text-base inline-block bg-black rounded-full px-3 py-1 text-sm font-semibold text-white mr-2" ]
                    [ case state of
                        Received ->
                            text "You"

                        Transferred ->
                            text "You"

                        NotInvolved ->
                            Eos.viewName transfer.from
                    ]
                ]
            ]
        , div [] [ viewAmount loggedIn transfer state ]
        , div [ class "px-4 py-2 m-2" ]
            [ div [ class "h-8 w-8 rounded-full mx-auto" ]
                [ Avatar.view "" avatar ""
                ]
            , div [ class "px-6 py-4" ]
                [ span [ class "inline-block bg-black rounded-full px-3 py-1 text-sm font-semibold text-white mr-2" ]
                    [ Eos.viewName <|
                        case state of
                            Received ->
                                transfer.from

                            Transferred ->
                                transfer.to

                            NotInvolved ->
                                transfer.to
                    ]
                ]
            ]
        ]


viewAmount : LoggedIn.Model -> Transfer -> State -> Html Msg
viewAmount { shared } transfer state =
    let
        t =
            I18Next.t shared.translations
    in
    div [ class "flex flex-row top-1/2" ]
        [ div [ class "px-4 py-2 m-2 " ]
            [ hr [ class "h-0 border border-dashed border-green" ]
                []
            ]
        , div [ class "px-4 py-2 m-2" ]
            [ div [ class "border border-solid rounded border-green bg-white" ]
                [ p [ class "text-xs text-gray-900" ]
                    [ text <|
                        case state of
                            Received ->
                                String.toUpper (t "transferscreen.received")

                            Transferred ->
                                String.toUpper (t "transferscreen.transferred")

                            NotInvolved ->
                                String.toUpper (t "transferscreen.transferred")
                    ]
                , div [ class "flex flex-row" ]
                    [ p [ class "mt-1 font-medium text-green" ]
                        [ text <|
                            (\str ->
                                if String.contains str "." then
                                    str

                                else
                                    str ++ ".000"
                            )
                            <|
                                String.fromFloat transfer.value
                        ]
                    , span [ class "ml-2 text-sm text-green mt-1 font-thin" ] [ text <| Eos.symbolToString transfer.symbol ]
                    ]
                ]
            ]
        ]


viewCommunity : LoggedIn.Model -> Transfer -> Html Msg
viewCommunity { shared } transfer =
    let
        t str =
            I18Next.t shared.translations str
                |> String.toUpper
    in
    div [ class "flex mb-4 bg-white" ]
        [ div [ class "w-full h-50 mt-20 mb-10" ]
            [ viewRest (t "transferscreen.community") <| Eos.symbolToString transfer.symbol
            , viewRest (t "transferscreen.date") <| dateTimeToString transfer.blockTime
            , viewRest (t "transferscreen.message") <| Maybe.withDefault "" transfer.memo
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
            if transfer.from == accountName then
                Transferred

            else if transfer.to == accountName then
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


dateTimeToString : DateTime -> String
dateTimeToString (DateTime dt) =
    dt
