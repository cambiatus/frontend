port module Log exposing
    ( BreadcrumbType(..)
    , Kind(..)
    , Level(..)
    , Log
    , addBreadcrumb
    , decodeError
    , graphqlErrorToKind
    , impossible
    , log
    , map
    , send
    , sendEvent
    )

{-| This is a module to help log data to our Sentry account. The two main things
available are adding breadcrumbs, and sending events to Sentry. In our project,
we use Sentry's JS SDK, which means Sentry is actually handled on the JS side of
our app (and not on Elm). That way, we can also log events from JS, and Sentry
can capture some events (like UI interaction, navigation, etc.) automatically.

Note that adding breadcrumbs **does not send anything to Sentry**. Breadcrumbs
are sent automatically with events. Adding a breadcrumb simply means it will be
shown on the event's information.

An event should be sent whenever we see something unusual, such as a failed
query, or some other kind of error. That will be sent over to Sentry, where we
can see details about the error and analyze further.

-}

import Dict exposing (Dict)
import Eos.Account
import Graphql.Http
import Graphql.Http.GraphqlError
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode



-- PORTS


port logError : ( String, String ) -> Cmd msg


port logDebug : ( String, Value ) -> Cmd msg


port addBreadcrumbPort : Value -> Cmd msg


port logEvent : Value -> Cmd msg



-- TYPES


{-| What kind of error happened
-}
type Kind
    = HttpError Http.Error
    | GraphqlHttpError Graphql.Http.HttpError
    | GraphqlErrors (List Graphql.Http.GraphqlError.GraphqlError)
    | DecodeError Decode.Error
    | Impossible (List String)
    | EncodedError Value
    | ContractError String
    | IncompatibleMsg


{-| Defines the severity of an event. From most important to least:
Fatal > Error > Warning > Info > Debug
-}
type Level
    = Fatal
    | Error
    | Warning
    | Info
    | DebugLevel


{-| Represents the structure of a Sentry breadcrumb. Extracted from the
[sentry docs](https://develop.sentry.dev/sdk/event-payloads/breadcrumbs/):

  - type\_: a [BreadcrumbType](#BreadcrumbType). Different types have different
    ways of being rendered.
  - category: a string separated with `.`, such as `ui.click`. In our
    application, it represents the msg that generated the breadcrumb.
  - message: a human-readable message, which preserves whitespace.
  - data: a `Dict` which will be presented as a key-value table.
  - level: used to emphasize breadcrumbs.

-}
type alias Breadcrumb msg =
    { type_ : BreadcrumbType
    , category : msg
    , message : String
    , data : Dict String Encode.Value
    , level : Level
    }


{-| All the possible types a breadcrumb can be. Extracted from
[the sentry docs](https://develop.sentry.dev/sdk/event-payloads/breadcrumbs/#breadcrumb-types):

  - Default: a generic breadcrumb - typically a log message or user-generated.
  - Debug: typically a log message.
  - Error: an error that ocurred before the exception.
  - Info: information that helps identify the root cause of the issue.
  - Query: a query.

-}
type BreadcrumbType
    = DefaultBreadcrumb
    | DebugBreadcrumb
    | ErrorBreadcrumb
    | InfoBreadcrumb
    | QueryBreadcrumb


{-| An event is what we actually send to Sentry. Descriptive messages, tags and
extras are very important so we can have a better debugging experience! Fields:

  - username: if the error comes from a logged in user, we can include their
    username. This makes it so if a user reports an error, we can search by
    their username on Sentry.
  - message: the title that shows up on Sentry.
  - tags: generic identifiers related to the event, such as
    ("error-type", "eos-transaction"). Used to filter/search through events
  - context: a [`Context`](#Context), which provides extra information.
  - transaction: used annotate the event with its point of failure. In our app,
    we use the `Msg` that caused the error
  - level: used to emphasize events

-}
type alias Event msg =
    { username : Maybe Eos.Account.Name
    , message : String
    , tags : Dict String String
    , context : Context
    , transaction : msg
    , level : Level
    }



-- TODO: Add Kind to Context or Event


{-| Some extra information to be displayed as a key-value table, under a name.
In `extras`, any key is allowed, except for `"type"`.
-}
type alias Context =
    { name : String
    , extras : Dict String Encode.Value
    }


type Log msg
    = Log (LogModel msg)


type alias LogModel msg =
    { msg : msg
    , kind : Kind
    }



-- CMDS


{-| Perform a command to add a Breadcrumb to Sentry. This **does not** send data
over to Sentry, it only add the breadcrumb information on the Sentry object
handled by JS. If you want to send an event to Sentry, use [`send`](#send),
[`impossible`](#impossible) or [`decodeError`](#decodeError)!
-}
addBreadcrumb : (msg -> List String) -> Breadcrumb msg -> Cmd msg
addBreadcrumb msgToString breadcrumb =
    Encode.object
        [ ( "type", breadcrumbTypeToString breadcrumb.type_ |> Encode.string )
        , ( "category", msgToString breadcrumb.category |> String.join "." |> Encode.string )
        , ( "message", Encode.string breadcrumb.message )
        , ( "data", Encode.dict identity identity breadcrumb.data )
        , ( "level", levelToString breadcrumb.level |> Encode.string )
        ]
        |> addBreadcrumbPort


sendEvent : (msg -> List String) -> Event msg -> Cmd msg
sendEvent msgToString event =
    Encode.object
        [ ( "user"
          , case event.username of
                Nothing ->
                    Encode.null

                Just username ->
                    Eos.Account.encodeName username
          )
        , ( "message", Encode.string event.message )
        , ( "tags"
            -- Prepend cambiatus so there's no name conflict
          , Dict.map (\_ tag -> "cambiatus." ++ tag) event.tags
                |> Dict.insert "cambiatus.language" "elm"
                |> Encode.dict identity Encode.string
          )
        , ( "context", encodeContext event.context )
        , ( "transaction", msgToString event.transaction |> String.join "." |> Encode.string )
        , ( "level", levelToString event.level |> Encode.string )
        ]
        |> logEvent


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

        EncodedError val ->
            ( String.join "." (toStrs a.msg)
            , val
            )
                |> logDebug

        ContractError err ->
            ( "[Contract Error] " ++ err ++ "\n", toStrs a.msg |> String.join "." )
                |> logError

        IncompatibleMsg ->
            logError ( "[Incompatible Msg]", toStrs a.msg |> String.join "." )


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



-- EXTERNAL HELPERS


log : LogModel msg -> Log msg
log =
    Log


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



-- INTERNAL HELPERS


levelToString : Level -> String
levelToString level =
    case level of
        Fatal ->
            "fatal"

        Error ->
            "error"

        Warning ->
            "warning"

        Info ->
            "info"

        DebugLevel ->
            "debug"


breadcrumbTypeToString : BreadcrumbType -> String
breadcrumbTypeToString breadcrumbType =
    case breadcrumbType of
        DefaultBreadcrumb ->
            "default"

        DebugBreadcrumb ->
            "debug"

        ErrorBreadcrumb ->
            "error"

        InfoBreadcrumb ->
            "info"

        QueryBreadcrumb ->
            "query"


encodeContext : Context -> Encode.Value
encodeContext context =
    Encode.object
        [ ( "name", Encode.string context.name )
        , ( "extras", Encode.dict identity identity context.extras )
        ]
