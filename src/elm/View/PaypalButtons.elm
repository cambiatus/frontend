module View.PaypalButtons exposing (Error(..), maximumAmount, minimumAmount, view)

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



-- VIEW


view :
    List (Html.Attribute msg)
    ->
        { id : String
        , value : Maybe Float
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
