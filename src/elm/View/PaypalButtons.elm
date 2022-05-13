module View.PaypalButtons exposing
    ( Currency(..)
    , Error(..)
    , currencyFromString
    , currencyToString
    , currencyToSymbol
    , maximumAmount
    , minimumAmount
    , view
    )

import Eos
import Html exposing (Html, node)
import Html.Attributes exposing (attribute, class, id)
import Html.Events exposing (on)
import Json.Decode



-- TYPES


type Error
    = AmountTooSmall
    | AmountTooBig
    | InvalidAmount
    | LoadError Json.Decode.Value
    | UnknownError Json.Decode.Value


type Currency
    = BRL
    | USD


currencyToString : Currency -> String
currencyToString currency =
    case currency of
        BRL ->
            "BRL"

        USD ->
            "USD"


currencyFromString : String -> Maybe Currency
currencyFromString currency =
    case currency of
        "BRL" ->
            Just BRL

        "USD" ->
            Just USD

        _ ->
            Nothing


currencyToSymbol : Currency -> Eos.Symbol
currencyToSymbol currency =
    case currency of
        BRL ->
            Eos.symbolFromString "2,BRL"
                |> Maybe.withDefault Eos.cambiatusSymbol

        USD ->
            Eos.symbolFromString "2,USD"
                |> Maybe.withDefault Eos.cambiatusSymbol



-- VIEW


view :
    List (Html.Attribute msg)
    ->
        { id : String
        , value : Maybe Float
        , currency : Currency
        , onApprove : msg
        , onCancel : msg
        , onError : Error -> msg
        }
    -> Html msg
view attrs options =
    node "paypal-buttons"
        (on "paypal-approve" (Json.Decode.succeed options.onApprove)
            :: on "paypal-cancel" (Json.Decode.succeed options.onCancel)
            :: on "paypal-error"
                (case options.value of
                    Nothing ->
                        Json.Decode.succeed (options.onError InvalidAmount)

                    Just value ->
                        if value < minimumAmount then
                            Json.Decode.succeed (options.onError AmountTooSmall)

                        else if value > maximumAmount then
                            Json.Decode.succeed (options.onError AmountTooBig)

                        else
                            Json.Decode.value
                                |> Json.Decode.map (UnknownError >> options.onError)
                )
            :: on "paypal-load-error"
                (Json.Decode.value
                    |> Json.Decode.map (LoadError >> options.onError)
                )
            :: id options.id
            :: class "z-0"
            :: attribute "elm-value"
                (case options.value of
                    Nothing ->
                        ""

                    Just value ->
                        String.fromFloat value
                )
            :: attribute "elm-currency" (currencyToString options.currency)
            :: attrs
        )
        []



-- EXTERNAL HELPERS


minimumAmount : Float
minimumAmount =
    1


maximumAmount : Float
maximumAmount =
    9999999.99
