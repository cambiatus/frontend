module Utils exposing
    ( decodeEnterKeyDown
    , decodeTimestamp
    , errorToString
    , onClickNoBubble
    , onClickPreventAll
    , posixDateTime
    )

import Cambiatus.Scalar exposing (DateTime(..))
import Graphql.Http
import Graphql.Http.GraphqlError
import Html
import Html.Events
import Iso8601
import Json.Decode as Decode
import Time exposing (Posix)


posixDateTime : Maybe DateTime -> Posix
posixDateTime maybedt =
    case maybedt of
        Nothing ->
            Time.millisToPosix 0

        Just (DateTime s) ->
            case Iso8601.toTime s of
                Ok posix ->
                    posix

                Err _ ->
                    Time.millisToPosix 0


decodeTimestamp : Decode.Decoder Posix
decodeTimestamp =
    Decode.int
        |> Decode.andThen
            (\ms ->
                Decode.succeed <| Time.millisToPosix ms
            )


decodeEnterKeyDown : Decode.Decoder Bool
decodeEnterKeyDown =
    let
        isEnter code =
            code == "Enter"
    in
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\cd ->
                Decode.succeed <| isEnter cd
            )


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
onClickPreventAll message =
    Html.Events.custom "click"
        (Decode.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = True
            }
        )


errorToString : Graphql.Http.Error parsedData -> String
errorToString errorData =
    case errorData of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            graphqlErrors
                |> List.map graphqlErrorToString
                |> String.join "\n"

        Graphql.Http.HttpError _ ->
            "Http Error"


graphqlErrorToString : Graphql.Http.GraphqlError.GraphqlError -> String
graphqlErrorToString error =
    error.message
