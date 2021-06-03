port module Log exposing (Kind(..), Log, decodeError, graphqlErrorToKind, impossible, log, map, send)

{-| This is a placeholder API for how we might do logging through
some service like <http://rollbar.com>.

Whenever you see Log.error used in this code base, it means
"Something unexpected happened. This is where we would log an
error to our server with some diagnostic info so we could investigate
what happened later."

-}

import Graphql.Http
import Graphql.Http.GraphqlError
import Http
import Json.Decode as Decode exposing (Value)


port logError : ( String, String ) -> Cmd msg


port logDebug : ( String, Value ) -> Cmd msg


type Kind
    = HttpError Http.Error
    | DecodeError Decode.Error
    | Impossible (List String)
    | GraphqlHttpError Graphql.Http.HttpError
    | GraphqlErrors (List Graphql.Http.GraphqlError.GraphqlError)
    | DebugValue Value


type Log msg
    = Log (LogModel msg)


log : LogModel msg -> Log msg
log =
    Log


type alias LogModel msg =
    { msg : msg
    , kind : Kind
    }


map : (a -> b) -> Log a -> Log b
map transform (Log a) =
    Log
        { msg = transform a.msg
        , kind = a.kind
        }


graphqlErrorToKind : Graphql.Http.Error a -> Kind
graphqlErrorToKind graphqlError_ =
    case graphqlError_ of
        Graphql.Http.HttpError httpError_ ->
            GraphqlHttpError httpError_

        Graphql.Http.GraphqlError _ errors ->
            GraphqlErrors errors


send : (a -> List String) -> Log a -> Cmd msg
send toStrs (Log a) =
    case a.kind of
        HttpError e ->
            httpError e

        DecodeError e ->
            decodeError e

        Impossible str ->
            toStrs a.msg
                ++ str
                |> String.join "."
                |> impossible

        GraphqlHttpError e ->
            toStrs a.msg
                |> String.join "."
                |> graphqlHttpError e

        GraphqlErrors errs ->
            let
                msgs =
                    errs
                        |> List.map (\e -> e.message)
                        |> String.join "\n"
            in
            logError ( "[GraphqlError]", msgs )

        DebugValue val ->
            ( String.join "." (toStrs a.msg)
            , val
            )
                |> logDebug


impossible : String -> Cmd msg
impossible e =
    logError ( "[Impossible Error]", e )


decodeError : Decode.Error -> Cmd msg
decodeError decErr =
    logError ( "[Decode Error]", Decode.errorToString decErr )


httpError : Http.Error -> Cmd msg
httpError httpError_ =
    case httpError_ of
        Http.BadUrl url ->
            logError ( "[Http Error: BadUrl]", url )

        Http.BadBody err ->
            logError ( "[Http Error: BadPayload]", err )

        _ ->
            Cmd.none


graphqlHttpError : Graphql.Http.HttpError -> String -> Cmd msg
graphqlHttpError e str =
    case e of
        Graphql.Http.BadUrl url ->
            logError ( str ++ ".Graphql.Http.BadUrl: ", url )

        Graphql.Http.BadPayload err ->
            logError ( str ++ ".Graphql.Http.BadPayload: ", Decode.errorToString err )

        _ ->
            Cmd.none
