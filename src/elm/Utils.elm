module Utils exposing (decodeDate, decodeTimestamp, posixDateTime)

import Bespiral.Scalar exposing (DateTime(..))
import Char
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Iso8601
import Json.Decode as Decode exposing (Decoder, int, nullable, string)
import Task
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


decodeDate : Decoder Posix
decodeDate =
    string
        |> Decode.andThen
            (\s ->
                let
                    dateStr =
                        if String.endsWith "Z" s then
                            s

                        else
                            s ++ "Z"
                in
                case Iso8601.toTime dateStr of
                    Ok posix ->
                        Decode.succeed posix

                    Err e ->
                        Decode.fail "Failed to parse date"
            )


decodeTimestamp : Decode.Decoder Time.Posix
decodeTimestamp =
    Decode.int
        |> Decode.andThen
            (\ms ->
                Decode.succeed <| Time.millisToPosix ms
            )
