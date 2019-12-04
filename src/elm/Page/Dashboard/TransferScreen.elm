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
init ({ shared } as loggedIn) transferId =
    let
        modelCmd =
            case maybeTransferID of
                Just transferID ->
                    ( { status = Loading }
                    , Api.Graphql.query loggedIn.shared (transferQuery transferID) CompletedTransferLoad
                    )

                Nothing ->
                    ( { status = InvalidId transferId }
                    , Cmd.none
                    )

        maybeTransferID =
            String.toInt transferId
    in
    modelCmd



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
    = Loading
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
        Loading ->
            Page.fullPageLoading

        InvalidId _ ->
            div []
                [ viewHeader loggedIn
                , Page.fullPageLoading
                ]

        LoadFailed error ->
            div []
                [ viewHeader loggedIn
                , Page.fullPageGraphQLError (t "transfer.title") error
                ]

        Loaded maybeTransfer state ->
            case maybeTransfer of
                Just transfer ->
                    div []
                        [ viewHeader loggedIn
                        , viewDoggo loggedIn transfer state
                        , viewCommunity transfer
                        ]

                Nothing ->
                    div [ class "container mx-auto px-4" ]
                        [ div []
                            [ text "Could not load the sale" ]
                        ]


viewHeader : LoggedIn.Model -> Html Msg
viewHeader ({ shared } as loggedIn) =
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
        , p [ class "text-white mx-auto" ] [ text (I18Next.t shared.translations "transfer.title") ]
        ]


viewDoggo : LoggedIn.Model -> Transfer -> State -> Html Msg
viewDoggo loggedIn transfer state =
    div [ class "static flex" ]
        [ div [ class "w-full bg-green h-50" ]
            [ div [ class "flex-row" ]
                [ div [ class "px-4 py-2 m-2" ]
                    [ img [ class "ml-32", src "%PUBLIC_URL%/images/transfer-doggo.svg" ] [] ]
                , div [ class "px-4 py-2 m-2" ]
                    [ p [class "-mr-1 font-medium font-sans text-white not-italic max-w-sm" ]
                        [ text <|
                            case state of
                                Transferred ->
                                    "Hey, this transfer was successfully made"

                                Received ->
                                    "Hey, this receipt was successfully made"

                                NotInvolved ->
                                    "Hey, this transfer was successfully made"
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
        , div [] [ viewAmount transfer state ]
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


viewAmount : Transfer -> State -> Html Msg
viewAmount transfer state =
    div [ class "flex flex-row top-1/2" ]
        [ div [class "px-4 py-2 m-2 "]
            [ hr [ class "h-0 border border-dashed border-green" ]
                []
            ]
        , div [class "px-4 py-2 m-2"]
            [ div [ class "border border-dashed border-green bg-white" ]
                [ p [ class "leading-none text-gray-900" ]
                    [ text <|
                        case state of
                            Received ->
                                "RECEIVED"

                            Transferred ->
                                "TRANSFERRED"

                            NotInvolved ->
                                "TRANSFERRED"
                    ]
                , div [ class "flex flex-row" ]
                    [ p [ class "font-medium text-green" ]
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
                    , span [ class "text-green font-thin" ] [ text <| "   " ++ Eos.symbolToString transfer.symbol ]
                    ]
                ]
            ]
        ]


viewCommunity : Transfer -> Html Msg
viewCommunity transfer =
    div [ class "flex mb-4" ]
        [ div [ class "w-full bg-white h-50" ]
            [ viewRest "COMMUNITY" <| Eos.symbolToString transfer.symbol
            , viewRest "DATE" <| dateTimeToString transfer.blockTime
            , viewRest "MESSAGE" <| Maybe.withDefault "" transfer.memo
            ]
        ]


viewRest : String -> String -> Html Msg
viewRest title content =
    div [ class "mt-auto" ]
        [ h5 [ class "leading-tight text-reward-green" ]
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
