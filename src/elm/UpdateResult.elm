module UpdateResult exposing
    ( UpdateResult
    , init, addCmd, addMsg, addExt, addPort, logHttpError, logImpossible, logGraphqlError, logDeletionStatusError, toModelCmd
    , fromChild
    , map, mapModel, setModel
    , addBreadcrumb, logDecodingError, logEvent, logIncompatibleMsg, logJsonValue, remoteDataToString, resultToString
    )

{-| This library allows us to have an observable update function which allows us to transmit message from
various model to and from other modules including the Main module. This enables us to request side effects from within modules
while maintaining a flexible codebase.
In a scenario such as displaying a request to sign a transfer or a sale is where the usefulness of this module really shines through


# Definition

@docs UpdateResult


# Common Helpers

@docs init, addCmd, addMsg, addExt, addPort, logHttpError, logImpossible, logGraphqlError, logDeletionStatusError, toModelCmd


# Using components

@docs fromChild


# Mapping UpdateResults

@docs map, mapModel, setModel

-}

import Api.Graphql.DeleteStatus
import Eos.Account
import Graphql.Http
import Http
import Json.Decode as Decode
import Log
import Ports
import RemoteData exposing (RemoteData(..))
import Task



-- DEFINITION


{-| Core data structure that enables this project to have an observable update function in our module.
The data structure contains the following

1.  A model, which is the model in the Main model, which in turn contains a status
    that is comprised of the subModel in question and some metadata.
2.  A list of Cmd messages that will be applied to the model
3.  A list of external messages to be applied to the model and submodel
4.  A list of ports to execute
5.  A list of breadcrumbs to be added to Sentry
6.  A list of events to be sent to Sentry

-}
type alias UpdateResult model msg extMsg =
    { model : model
    , cmds : List (Cmd msg)
    , exts : List extMsg
    , ports : List (Ports.JavascriptOut msg)
    , breadcrumbs : List (Log.Breadcrumb msg)
    , events : List (Log.Event msg)
    }



-- INITIALIZING


{-| Builds an inital UpdateResult dataset from a model, this is widely used when initiating actions, ports and Logs
as within the update function in a module a use only has access to the model hence it is necessary to construct
an UpdateResult dataset out of the model before requesting for commads, ports and log actions
-}
init : model -> UpdateResult model msg extMsg
init model =
    { model = model
    , cmds = []
    , exts = []
    , ports = []
    , breadcrumbs = []
    , events = []
    }


{-| If a component returns an `UpdateResult`, you can use this function to merge
the component's update result with the page's update result.
-}
fromChild :
    (childModel -> model)
    -> (childMsg -> msg)
    -> (childExtMsg -> UpdateResult model msg extMsg -> UpdateResult model msg extMsg)
    -> model
    -> UpdateResult childModel childMsg childExtMsg
    -> UpdateResult model msg extMsg
fromChild addChildModel fromChildMsg fromChildExt parent childResult =
    init parent
        |> addChild childResult
            (\child _ -> addChildModel child)
            fromChildMsg
            fromChildExt


{-| Similar to `fromChild`, but takes an already existant `UpdateResult` from
the parent
-}
addChild :
    UpdateResult childModel childMsg childExtMsg
    -> (childModel -> model -> model)
    -> (childMsg -> msg)
    -> (childExtMsg -> UpdateResult model msg extMsg -> UpdateResult model msg extMsg)
    -> UpdateResult model msg extMsg
    -> UpdateResult model msg extMsg
addChild childResult addChildModel fromChildMsg fromChildExt parentResult =
    let
        applyChildExts parent =
            List.foldl fromChildExt parent childResult.exts
    in
    { model = addChildModel childResult.model parentResult.model
    , cmds =
        List.map (Cmd.map fromChildMsg) childResult.cmds
            ++ parentResult.cmds
    , ports = List.map (Ports.mapAddress fromChildMsg) childResult.ports ++ parentResult.ports
    , breadcrumbs = List.map (Log.mapBreadcrumb fromChildMsg) childResult.breadcrumbs ++ parentResult.breadcrumbs
    , events = List.map (Log.map fromChildMsg) childResult.events ++ parentResult.events
    , exts = parentResult.exts
    }
        |> applyChildExts



-- MAPPING


{-| Applies commands, ports, events and breadcrumbs to an UpdateResult dataset resulting in new UpdateResult dataset with all
the pending actions applied. Useful when moving from one state to another using UpdateResult such as updating the data when
a user has logged in, or when handling a Community message on the Dashboard
-}
map : (subModel -> model) -> (subMsg -> msg) -> (subExtMsg -> UpdateResult model msg extMsg -> UpdateResult model msg extMsg) -> UpdateResult subModel subMsg subExtMsg -> UpdateResult model msg extMsg
map toModel toMsg handleExtMsg updateResult =
    List.foldl
        handleExtMsg
        { model = toModel updateResult.model
        , cmds = [ Cmd.map toMsg (Cmd.batch updateResult.cmds) ]
        , exts = []
        , ports = List.map (Ports.mapAddress toMsg) updateResult.ports
        , breadcrumbs = List.map (Log.mapBreadcrumb toMsg) updateResult.breadcrumbs
        , events = List.map (Log.map toMsg) updateResult.events
        }
        updateResult.exts


{-| Converts the model in an UpdateResult from one model to another, accepts a function that does the conversion and after the conversion it applies
the new model to the UpdateResult and returns the new UpdateResult dataset useful when using an UpdateResult that has been passed into a module
and converting that UpdateResult to a variant the current module can operate on
-}
mapModel : (m -> m2) -> UpdateResult m msg eMsg -> UpdateResult m2 msg eMsg
mapModel transform uResult =
    { model = transform uResult.model
    , cmds = uResult.cmds
    , exts = uResult.exts
    , ports = uResult.ports
    , breadcrumbs = uResult.breadcrumbs
    , events = uResult.events
    }


{-| Applies a model to an UpdateResult, this is particularly useful when dealing operations that involve side effects
such as when handling the result of an image upload
-}
setModel : UpdateResult m msg eMsg -> m2 -> UpdateResult m2 msg eMsg
setModel uResult model =
    { model = model
    , cmds = uResult.cmds
    , exts = uResult.exts
    , ports = uResult.ports
    , breadcrumbs = uResult.breadcrumbs
    , events = uResult.events
    }



-- CONVERTING TO USEFUL TYPES


{-| Converts an UpdateResult into a (model, command) tuple, it accepts a function that handles conversion of
external messages to the tuple, a function that normalizes the messages into a list of strings and finally
an UpdateResult to operate on. Useful in the main module when finally needed the (model, command) tuple for it's update function
-}
toModelCmd : (eMsg -> m -> ( m, Cmd msg )) -> (msg -> List String) -> UpdateResult m msg eMsg -> ( m, Cmd msg )
toModelCmd transformEMsg msgToString uResult =
    case uResult.exts of
        ext :: extTail ->
            let
                ( newM, newCmd ) =
                    transformEMsg ext uResult.model
            in
            toModelCmd
                transformEMsg
                msgToString
                { uResult
                    | model = newM
                    , cmds = uResult.cmds ++ [ newCmd ]
                    , exts = extTail
                }

        [] ->
            ( uResult.model
            , Cmd.batch
                (uResult.cmds
                    ++ List.map (Log.addBreadcrumb msgToString) (List.reverse uResult.breadcrumbs)
                    ++ List.map (Ports.javascriptOutCmd msgToString) uResult.ports
                    ++ List.map (Log.send msgToString) (List.reverse uResult.events)
                )
            )



-- PIPELINE HELPERS


{-| Add a Command msg to the list of msgs in an UpdateResult, useful when asking for effects, such as a network
request
-}
addCmd : Cmd msg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
addCmd cmd uResult =
    { uResult | cmds = uResult.cmds ++ [ cmd ] }


{-| Add a msg to the list of msgs in an UpdateResult, useful to reduce duplication,
like when performing multiple requests that produce the same msg
-}
addMsg : msg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
addMsg msg uResult =
    { uResult | cmds = uResult.cmds ++ [ Task.succeed () |> Task.perform (\_ -> msg) ] }


{-| Adds an external command to the list of commands in an UpdateResult, this is uselful when needing
commands from another module, such as checking if a particular auth mechanism is present or asking a user
to sign a transaction
-}
addExt : eMsg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
addExt eMsg uResult =
    { uResult | exts = uResult.exts ++ [ eMsg ] }


{-| Adds a an outgoing port request to an UpdateResult
-}
addPort : Ports.JavascriptOutModel msg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
addPort port_ uResult =
    { uResult | ports = uResult.ports ++ [ Ports.javascriptOut port_ ] }



-- LOGGING


{-| Add a breadcrumb to error reporting. On development, prints it to the
console, and on production adds a breadcrumb to the next event
-}
addBreadcrumb : Log.Breadcrumb msg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
addBreadcrumb breadcrumb uResult =
    { uResult | breadcrumbs = breadcrumb :: uResult.breadcrumbs }


{-| Add an event to be logged. On development, prints it to the console, and on
production sends an event to Sentry. Usually you can use auxiliary functions
such as `logImpossible`, `logDecodingError`, `logHttpError`, `logGraphqlError`,
`logJsonValue` and `logIncompatibleMsg`
-}
logEvent : Log.Event msg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logEvent event uResult =
    { uResult | events = event :: uResult.events }


{-| Send an Event to Sentry so we can debug later. Should be used when something
broke on the business rules, or when we know something can't happen based on the
flow of the app, e.g. being in a page that requires a Community, but not having
the Community available
-}
logImpossible : msg -> String -> Maybe Eos.Account.Name -> Log.Location -> List Log.Context -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logImpossible transaction message maybeUser location contexts =
    Log.fromImpossible transaction message maybeUser location contexts
        |> logEvent


{-| Send an Event to Sentry so we can debug later. Should be used when trying to
decode a JSON value, but getting a Json.Decode.Error.
-}
logDecodingError : msg -> Maybe Eos.Account.Name -> String -> Log.Location -> List Log.Context -> Decode.Error -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logDecodingError transaction maybeUser description location contexts error =
    Log.fromDecodeError transaction maybeUser description location contexts error
        |> logEvent


{-| Send an Event to Sentry so we can debug later. Should be used when
attempting to perform an Http request and getting an Http.Error.
-}
logHttpError : msg -> Maybe Eos.Account.Name -> String -> Log.Location -> List Log.Context -> Http.Error -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logHttpError transaction maybeUser description location contexts error =
    Log.fromHttpError transaction maybeUser description location contexts error
        |> logEvent


{-| Send an Event to Sentry so we can debug later. Should be used when
attempting to perform a GraphQL request and getting a Graphql.Http.Error.
-}
logGraphqlError : msg -> Maybe Eos.Account.Name -> String -> Log.Location -> List Log.Context -> Graphql.Http.Error a -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logGraphqlError transaction maybeUser description location contexts error =
    Log.fromGraphqlHttpError transaction maybeUser description location contexts error
        |> logEvent


{-| Send an Event to Sentry so we can debug later. Should be used when we get
some sort of error encoded as a JSON value.

**Note**: If you just want to log extra information, use something else and add
that information in a `Log.Context`.

-}
logJsonValue : msg -> Maybe Eos.Account.Name -> String -> Log.Location -> List Log.Context -> Decode.Value -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logJsonValue transaction maybeUser message location contexts jsonValue =
    Log.fromJsonValue transaction maybeUser message location contexts jsonValue
        |> logEvent


{-| Send an Event to Sentry so we can debug later. Should be used when
pattern-matching two values at once (usually Model and Msg on update functions),
and they're in a configuration that doesn't match (usually pattern-matched as
`(_, _)`)
-}
logIncompatibleMsg : msg -> Maybe Eos.Account.Name -> Log.Location -> List Log.Context -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logIncompatibleMsg transaction maybeUser location contexts =
    Log.fromIncompatibleMsg transaction maybeUser location contexts
        |> logEvent


{-| Send an Event to Sentry so we can debug later. Should be used when deleting
something with the GraphQL API, but getting an error from Api.Graphql.DeleteStatus.
-}
logDeletionStatusError : msg -> Maybe Eos.Account.Name -> Log.Location -> List Log.Context -> Api.Graphql.DeleteStatus.ErrorReason -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logDeletionStatusError transaction maybeUser location contexts reason =
    Log.fromDeletionStatusError transaction maybeUser location contexts reason
        |> logEvent



-- EXTERNAL HELPERS


{-| Converts a Result Dataset into a string usable by UpdateResult
-}
resultToString : Result x a -> String
resultToString r =
    case r of
        Ok _ ->
            "Ok"

        Err _ ->
            "Err"


{-| Converts a RemoteData Dataset into a string usable by UpdateResult
-}
remoteDataToString : RemoteData e a -> String
remoteDataToString r =
    case r of
        NotAsked ->
            "NotAsked"

        Loading ->
            "Loading"

        Success _ ->
            "Success"

        Failure _ ->
            "Failure"
