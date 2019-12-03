module Page.Shop.TransferScreen exposing (Model, Msg, init, msgToString, subscriptions, update, view)

import Api
import Api.Graphql
import Avatar
import Eos exposing (Symbol, symbolFromString)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import I18Next
import Page
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Shop exposing (Sale, TransferSale)
import Time
import Transfer exposing (Transfer)
import UpdateResult as UR



-- INIT


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) transferId =
    let
        saleLoadCommand = 
            case maybeTransferID of
                Just saleID ->
                    Api.Graphql.query loggedIn.shared (Shop.saleQuery saleID) CompletedSaleLoad

                Nothing ->
                    Cmd.none

        
        maybeTransferID = 
            String.toInt transferId

        transferCommand = 
            case maybeTransferID of
                Just transferID ->
                    Cmd.none

                Nothing ->
                    Cmd.none

    in
    ( { status = Loading
      }
    , Cmd.batch [saleLoadCommand, transferCommand ]
    )



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


type Status
    = Loading
    | InvalidId String
    | LoadFailed (Graphql.Http.Error (Maybe Sale))
    | Loaded (Maybe Sale)



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view { shared } model =
    let
        t =
            I18Next.t shared.translations
    in
    div []
        [ viewDoggo model
        , viewCommunity model
        ]



-- viewSale : TransferSale -> Sale -> Html Msg
-- viewSale tSale sale =


viewSale =
    div [ class "max-w-sm rounded overflow-hidden bg-gray-100" ]
        [ div []
            [ img
                [ class "shadow-inner h-24 lg:h-auto lg:w-48 flex-none bg-cover rounded-t lg:rounded-t-none lg:rounded-l text-center overflow-hidden"
                , src "%PUBLIC_URL%/images/register-dog.png" --(sale.image |> Maybe.withDefault "")
                , alt "Image here"
                ]
                []
            ]
        , h5 [ class "text-black" ] [ text "sale.title" ] --[text sale.title ]
        , p [ class "text-gray-900" ] [ text <| String.fromInt 10 ] --"sale.units") ++ " in stock now" ]
        , label [ class "text-reward-green" ] [ text "CHOSEN AMOUNT" ]
        , input
            [ type_ "text"
            , disabled True
            , value <| String.fromInt 10 -- tSale.units
            , class ""
            ]
            []
        ]


viewDoggo : Model -> Html Msg
viewDoggo model =
    div [ class "flex mb-4" ]
        [ div [ class "w-full bg-green h-50" ]
            [ img [ class "m-16", src "%PUBLIC_URL%/images/transfer-doggo.svg" ] []
            , div [ class "w-27" ]
                [ p [ class "text-center font-medium font-sans text-white not-italic" ]
                    [ text "Hey, this purchase was successful made" ]
                ]
            , div [ ]--class "-mt-8" ]
                [ viewSale

                --   , viewTransferCard model
                ]
            ]
        ]


viewTransferCard : Model -> Html Msg
viewTransferCard model =
    div [ class "flex flex-row max-w-sm rounded overflow-hidden bg-gray-100" ]
        [ div [ class "px-4 py-2 m-2" ]
            [ img [ class "h-8 w-8 rounded-full mx-auto" ] []
            , div [ class "px-6 py-4" ]
                [ span [ class "text-base inline-block bg-black rounded-full px-3 py-1 text-sm font-semibold text-white mr-2" ]
                    [ text "You" ]
                ]
            ]
        , div [ class "px-4 py-2 m-2" ]
            [ img [ class "h-8 w-8 rounded-full mx-auto" ] []
            , div [ class "px-6 py-4" ]
                [ span [ class "inline-block bg-black rounded-full px-3 py-1 text-sm font-semibold text-white mr-2" ]
                    [ text "You" ]
                ]
            ]
        ]


viewCommunity : Model -> Html Msg
viewCommunity model =
    div [ class "flex mb-4" ]
        [ div [ class "w-full bg-white h-50" ]
            [ viewRest "COMMUNITY" "Pulpes"
            , viewRest "DATE" ""
            , viewRest "MESSAGE" ""
            ]
        ]


viewRest : String -> String -> Html Msg
viewRest title content =
    div [ class "" ]
        [ h5 [ class "leading-tight text-reward-green" ]
            [ text title ]
        , p [ class "text-lg font-sans not-italic" ]
            [ text content ]
        ]



-- UPDATE

type Msg
    = CompletedSaleLoad (Result (Graphql.Http.Error (Maybe Sale)) (Maybe Sale))

type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)

update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model user =
    case msg of
        CompletedSaleLoad (Ok sale) ->
            case model.status of
                Loading ->
                    UR.init model

                Loaded _->
                    UR.init model

                LoadFailed _ ->
                    UR.init model

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        CompletedSaleLoad (Err error) ->
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
        CompletedSaleLoad r ->
            [ "CompletedSaleLoad", UR.resultToString r ]
