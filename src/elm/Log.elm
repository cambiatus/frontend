port module Log exposing
    ( Breadcrumb
    , BreadcrumbType(..)
    , ErrorType(..)
    , Event
    , ExpectedAuthentication(..)
    , Kind(..)
    , Level(..)
    , Log
    , Tag(..)
    , addBreadcrumb
    , fromDecodeError
    , fromGraphqlHttpError
    , fromHttpError
    , fromImpossible
    , graphqlErrorToKind
    , log
    , map
    , mapBreadcrumb
    , mapEvent
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


{-| What kind of authentication was expected
-}
type ExpectedAuthentication
    = ExpectedLoggedIn
    | ExpectedGuest


{-| What kind of error to report
-}
type ErrorType
    = IncompatibleMsg
    | ImpossibleError
    | DecodingError
    | HttpErrorType
    | GraphqlErrorType
    | ContractError


{-| Defines the possible tags. Tags are used to search for events on Sentry.
-}
type Tag
    = TypeTag ErrorType
    | IncompatibleAuthentication ExpectedAuthentication


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
    , tags : List Tag
    , context : Maybe Context
    , transaction : msg
    , level : Level
    }


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
handled by JS. If you want to send an event to Sentry, use
[`sendEvent`](#sendEvent)!

On development, just logs to the console.

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


{-| Perform a command to send an Event to Sentry. It will include all the
breadcrums added since the previous event.

On development, just logs to the console.

-}
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
          , List.map encodeTag event.tags
                |> Dict.fromList
                |> Encode.dict identity identity
          )
        , ( "context"
          , case event.context of
                Nothing ->
                    Encode.null

                Just context ->
                    encodeContext context
          )
        , ( "transaction", msgToString event.transaction |> String.join "." |> Encode.string )
        , ( "level", levelToString event.level |> Encode.string )
        ]
        |> logEvent


send : (a -> List String) -> Log a -> Cmd msg
send toStrs (Log a) =
    case a.kind of
        HttpError _ ->
            Cmd.none

        DecodeError _ ->
            Cmd.none

        Impossible _ ->
            Cmd.none

        GraphqlHttpError _ ->
            Cmd.none

        GraphqlErrors _ ->
            Cmd.none

        EncodedError val ->
            ( String.join "." (toStrs a.msg)
            , val
            )
                |> logDebug



-- EXTERNAL HELPERS


{-| Creates an Event out of an impossible error. This is something that should
be impossible to happen, either because of business rules or because we know
something just can't happen based on the flow of the app.
-}
fromImpossible : msg -> String -> Maybe Eos.Account.Name -> Event msg
fromImpossible transaction message maybeUser =
    { username = maybeUser
    , message = message
    , tags = [ TypeTag ImpossibleError ]
    , context = Nothing
    , transaction = transaction
    , level = Fatal
    }


{-| Creates an Event out of a decoding error. This happens when trying to decode
something, but the data doesn't fit into the decoder provided.
-}
fromDecodeError : msg -> Maybe Eos.Account.Name -> String -> Decode.Error -> Event msg
fromDecodeError transaction maybeUser description error =
    { username = maybeUser
    , message = "Got an error when trying to decode a JSON value"
    , tags = [ TypeTag DecodingError ]
    , context =
        Just
            { name = "Decode error"
            , extras =
                Dict.fromList
                    [ ( "Description", Encode.string description )
                    , ( "Error", encodeDecodingError error )
                    ]
            }
    , transaction = transaction
    , level = Error
    }


{-| Creates an Event out of a HTTP error.
-}
fromHttpError : msg -> Maybe Eos.Account.Name -> String -> Http.Error -> Event msg
fromHttpError transaction maybeUser description error =
    { username = maybeUser
    , message = "Got an error when performing an HTTP request"
    , tags = [ TypeTag HttpErrorType ]
    , context =
        Just
            { name = "HTTP error"
            , extras =
                Dict.fromList
                    [ ( "Description", Encode.string description )
                    , ( "Error", encodeHttpError error )
                    ]
            }
    , transaction = transaction
    , level = Error
    }


{-| Creates an Event out of a GraphQL error.
-}
fromGraphqlHttpError : msg -> Maybe Eos.Account.Name -> String -> Graphql.Http.Error a -> Event msg
fromGraphqlHttpError transaction maybeUser description error =
    { username = maybeUser
    , message = "Got an error when performing a GraphQL request"
    , tags = [ TypeTag GraphqlErrorType ]
    , context =
        Just
            { name = "Graphql error"
            , extras =
                Dict.fromList
                    [ ( "Description", Encode.string description )
                    , ( "Error", encodeGraphqlError error )
                    ]
            }
    , transaction = transaction
    , level = Error
    }


log : LogModel msg -> Log msg
log =
    Log


map : (a -> b) -> Log a -> Log b
map transform (Log a) =
    Log
        { msg = transform a.msg
        , kind = a.kind
        }


mapBreadcrumb : (a -> b) -> Breadcrumb a -> Breadcrumb b
mapBreadcrumb transform breadcrumb =
    { type_ = breadcrumb.type_
    , category = transform breadcrumb.category
    , message = breadcrumb.message
    , data = breadcrumb.data
    , level = breadcrumb.level
    }


mapEvent : (a -> b) -> Event a -> Event b
mapEvent transform event =
    { username = event.username
    , message = event.message
    , tags = event.tags
    , context = event.context
    , transaction = transform event.transaction
    , level = event.level
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


encodeDecodingError : Decode.Error -> Encode.Value
encodeDecodingError error =
    case error of
        Decode.Field fieldName fieldError ->
            Encode.object
                [ ( "type", Encode.string "Decode.Field" )
                , ( "fieldName", Encode.string fieldName )
                , ( "fieldError", encodeDecodingError fieldError )
                ]

        Decode.Index index indexError ->
            Encode.object
                [ ( "type", Encode.string "Decode.Index" )
                , ( "index", Encode.int index )
                , ( "indexError", encodeDecodingError indexError )
                ]

        Decode.OneOf errors ->
            Encode.object
                [ ( "type", Encode.string "Decode.OneOf" )
                , ( "errors", Encode.list encodeDecodingError errors )
                ]

        Decode.Failure failureString failureValue ->
            Encode.object
                [ ( "type", Encode.string "Decode.Failure" )
                , ( "failureString", Encode.string failureString )
                , ( "failureValue", failureValue )
                ]


encodeHttpError : Http.Error -> Encode.Value
encodeHttpError error =
    case error of
        Http.BadUrl url ->
            Encode.object
                [ ( "type", Encode.string "Http.BadUrl" )
                , ( "providedUrl", Encode.string url )
                ]

        Http.Timeout ->
            Encode.object [ ( "type", Encode.string "Http.Timeout" ) ]

        Http.NetworkError ->
            Encode.object [ ( "type", Encode.string "Http.NetworkError" ) ]

        Http.BadStatus status ->
            Encode.object
                [ ( "type", Encode.string "Http.BadStatus" )
                , ( "status", Encode.int status )
                ]

        Http.BadBody body ->
            Encode.object
                [ ( "type", Encode.string "Http.BadBody" )
                , ( "body", Encode.string body )
                ]


encodeGraphqlError : Graphql.Http.Error a -> Encode.Value
encodeGraphqlError error =
    let
        encodeGraphqlInternalError : Graphql.Http.GraphqlError.GraphqlError -> Encode.Value
        encodeGraphqlInternalError internalError =
            Encode.object
                [ ( "message", Encode.string internalError.message )
                , ( "locations"
                  , case internalError.locations of
                        Nothing ->
                            Encode.null

                        Just locations ->
                            Encode.list
                                (\location ->
                                    Encode.object
                                        [ ( "line", Encode.int location.line )
                                        , ( "column", Encode.int location.column )
                                        ]
                                )
                                locations
                  )
                , ( "details", Encode.dict identity identity internalError.details )
                ]
    in
    case error of
        Graphql.Http.GraphqlError possiblyParsedData graphqlErrors ->
            Encode.object
                [ ( "type", Encode.string "Graphql.Http.GraphqlError" )
                , ( "data"
                  , case possiblyParsedData of
                        Graphql.Http.GraphqlError.ParsedData _ ->
                            Encode.object
                                [ ( "status"
                                  , Encode.string "Graphql.Http.GraphqlError.ParsedData"
                                  )
                                ]

                        Graphql.Http.GraphqlError.UnparsedData unparsedData ->
                            Encode.object
                                [ ( "status", Encode.string "Graphql.Http.GraphqlError.UnparsedData" )
                                , ( "unparsedData", unparsedData )
                                ]
                  )
                , ( "errors"
                  , Encode.list encodeGraphqlInternalError graphqlErrors
                  )
                ]

        Graphql.Http.HttpError httpError ->
            case httpError of
                Graphql.Http.BadUrl url ->
                    Encode.object
                        [ ( "type", Encode.string "Graphql.Http.BadUrl" )
                        , ( "providedUrl", Encode.string url )
                        ]

                Graphql.Http.Timeout ->
                    Encode.object [ ( "type", Encode.string "Graphql.Http.Timeout" ) ]

                Graphql.Http.NetworkError ->
                    Encode.object [ ( "type", Encode.string "Graphql.Http.NetworkError" ) ]

                Graphql.Http.BadStatus metadata body ->
                    Encode.object
                        [ ( "type", Encode.string "Graphql.Http.BadStatus" )
                        , ( "metadata"
                          , Encode.object
                                [ ( "url", Encode.string metadata.url )
                                , ( "statusCode", Encode.int metadata.statusCode )
                                , ( "statusText", Encode.string metadata.statusText )
                                , ( "headers", Encode.dict identity Encode.string metadata.headers )
                                ]
                          )
                        , ( "body", Encode.string body )
                        ]

                Graphql.Http.BadPayload jsonError ->
                    Encode.object
                        [ ( "type", Encode.string "Graphql.Http.BadPayload" )
                        , ( "jsonError", encodeDecodingError jsonError )
                        ]


encodeTag : Tag -> ( String, Encode.Value )
encodeTag tag =
    case tag of
        TypeTag errorType ->
            let
                value =
                    case errorType of
                        IncompatibleMsg ->
                            "incompatible msg"

                        ImpossibleError ->
                            "impossible error"

                        DecodingError ->
                            "decoding error"

                        HttpErrorType ->
                            "http error"

                        GraphqlErrorType ->
                            "graphql error"

                        ContractError ->
                            "contract error"
            in
            ( "cambiatus.type", Encode.string value )

        IncompatibleAuthentication expectedAuthentication ->
            let
                value =
                    case expectedAuthentication of
                        ExpectedGuest ->
                            "guest"

                        ExpectedLoggedIn ->
                            "logged in"
            in
            ( "cambiatus.expected-authentication", Encode.string value )
