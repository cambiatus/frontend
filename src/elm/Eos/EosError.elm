module Eos.EosError exposing (extractFailure)

import Json.Decode as Decode exposing (at, decodeString, field, list, string)


{-| Extracts failure description from the first error message received from the EOS:

    extractFailure "assertion failure with message: Can't vote on already verified claim"
        == "Can't vote on already verified claim"

The result is a string which can be translated and showed to the user.

-}
extractFailure : String -> String
extractFailure json =
    let
        eosErrorMessages =
            decodeString decodeErrorDetails json
    in
    case eosErrorMessages of
        Ok (firstMessage :: _) ->
            case String.split ": " firstMessage of
                _ :: msg :: [] ->
                    msg

                _ ->
                    -- Keep whole message (may be verbose, with no translation, but better than nothing)
                    firstMessage

        _ ->
            "error.unknown"


decodeErrorDetails : Decode.Decoder (List String)
decodeErrorDetails =
    at [ "error", "details" ] <|
        list (field "message" string)
