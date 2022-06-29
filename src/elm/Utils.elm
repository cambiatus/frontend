module Utils exposing
    ( areSameDay
    , decodeTimestamp
    , escSubscription
    , formatFloat
    , formatInt
    , fromDateTime
    , fromMaybeDateTime
    , fromNaiveDateTime
    , onClickNoBubble
    , onClickPreventAll
    , onSubmitPreventAll
    , padInt
    , previousDay
    , slugify
    , spawnMessage
    )

import Browser.Events
import Cambiatus.Scalar exposing (DateTime(..), NaiveDateTime(..))
import Date
import Html
import Html.Events
import Iso8601
import Json.Decode as Decode
import Mask
import Slug
import Task
import Time exposing (Posix)


fromMaybeDateTime : Maybe DateTime -> Posix
fromMaybeDateTime maybeDateTime =
    case maybeDateTime of
        Nothing ->
            Time.millisToPosix 0

        Just dateTime ->
            fromDateTime dateTime


fromDateTime : DateTime -> Posix
fromDateTime (DateTime dateTime) =
    Iso8601.toTime dateTime
        |> Result.withDefault (Time.millisToPosix 0)


fromNaiveDateTime : NaiveDateTime -> Posix
fromNaiveDateTime (NaiveDateTime dateTime) =
    Iso8601.toTime dateTime
        |> Result.withDefault (Time.millisToPosix 0)


areSameDay : Time.Zone -> Posix -> Posix -> Bool
areSameDay timezone first second =
    Date.fromPosix timezone first == Date.fromPosix timezone second


previousDay : Posix -> Posix
previousDay time =
    let
        day =
            -- 1000ms * 60s * 60min * 24h
            1000 * 60 * 60 * 24
    in
    Time.millisToPosix (Time.posixToMillis time - day)


{-| Format a float to separate thousands, and use `,` as a separator for
decimals
-}
formatFloat : Maybe { translators | t : String -> String } -> Int -> Float -> String
formatFloat maybeTranslators decimalCases number =
    Mask.float (Mask.Precisely decimalCases)
        (case maybeTranslators of
            Just { t } ->
                { decimalSeparator = t "decimal_separator"
                , thousandsSeparator = t "thousands_separator"
                }

            Nothing ->
                { decimalSeparator = "."
                , thousandsSeparator = ""
                }
        )
        number


{-| Format an Int to separate thousands.
-}
formatInt : Maybe { translators | t : String -> String } -> Int -> String
formatInt maybeTranslators number =
    formatFloat maybeTranslators 0 (toFloat number)


{-| Pad an Int with zeros to a given length
-}
padInt : Int -> Int -> String
padInt totalLength number =
    let
        currentLength =
            String.fromInt number
                |> String.length

        missingLength =
            totalLength - currentLength
    in
    String.repeat missingLength "0" ++ String.fromInt number


escSubscription : msg -> Sub msg
escSubscription toMsg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Esc" || key == "Escape" then
                    Decode.succeed ()

                else
                    Decode.fail "Expecting Escape key"
            )
        |> Browser.Events.onKeyDown
        |> Sub.map (\_ -> toMsg)


decodeTimestamp : Decode.Decoder Posix
decodeTimestamp =
    Decode.int
        |> Decode.map Time.millisToPosix


{-| Click event listener that stops propagation, but doesn't prevent default
-}
onClickNoBubble : msg -> Html.Attribute msg
onClickNoBubble message =
    Html.Events.custom "click"
        (Decode.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = False
            }
        )


{-| Click event listener that stops propagation and prevents default
-}
onClickPreventAll : msg -> Html.Attribute msg
onClickPreventAll =
    preventAll "click"


{-| Submit event listener that stops propagation and prevents default
-}
onSubmitPreventAll : msg -> Html.Attribute msg
onSubmitPreventAll =
    preventAll "submit"


preventAll : String -> msg -> Html.Attribute msg
preventAll eventName message =
    Html.Events.custom eventName
        (Decode.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = True
            }
        )


spawnMessage : msg -> Cmd msg
spawnMessage msg =
    Task.succeed msg
        |> Task.perform identity


{-| Try to turn a String into a Slug, replacing some accentuated characters.
Use this instead of `Slug.generate`, so we can have better slugs!
-}
slugify : String -> Maybe Slug.Slug
slugify input =
    input
        |> String.replace "á" "a"
        |> String.replace "ã" "a"
        |> String.replace "à" "a"
        |> String.replace "ç" "c"
        |> String.replace "é" "e"
        |> String.replace "í" "i"
        |> String.replace "ñ" "n"
        |> String.replace "ó" "o"
        |> String.replace "õ" "o"
        |> Slug.generate
