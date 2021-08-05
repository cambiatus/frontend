module View.PaypalButtons exposing (Error(..), view)

import Html exposing (Html, node)
import Html.Attributes exposing (attribute, id)
import Html.Events exposing (on)
import Json.Decode
import Utils



-- TYPES


type Error
    = AmountTooSmall { enteredAmount : Float, minimumAmount : Float }
    | AmountTooBig { enteredAmount : Float, maximumAmount : Float }
    | UnknownError



-- VIEW


view :
    List (Html.Attribute msg)
    ->
        { id : String
        , value : Float
        , communityName : String
        , onApprove : msg
        , onCancel : msg
        , onError : Error -> msg
        }
    -> Html msg
view attrs options =
    node "paypal-buttons"
        (attribute "elm-value" (Utils.formatFloat options.value 2 False)
            :: attribute "elm-community-name" options.communityName
            :: on "paypal-approve" (Json.Decode.succeed options.onApprove)
            :: on "paypal-cancel" (Json.Decode.succeed options.onCancel)
            :: on "paypal-error"
                (if options.value < paypalMinimumAmount then
                    Json.Decode.succeed
                        (options.onError
                            (AmountTooSmall
                                { enteredAmount = options.value
                                , minimumAmount = paypalMinimumAmount
                                }
                            )
                        )

                 else if options.value > paypalMaximumAmount then
                    Json.Decode.succeed
                        (options.onError
                            (AmountTooBig
                                { enteredAmount = options.value
                                , maximumAmount = paypalMaximumAmount
                                }
                            )
                        )

                 else
                    Json.Decode.succeed (options.onError UnknownError)
                )
            :: id options.id
            :: attrs
        )
        []



-- INTERNAL HELPERS


paypalMinimumAmount : Float
paypalMinimumAmount =
    1


paypalMaximumAmount : Float
paypalMaximumAmount =
    9999999.99
