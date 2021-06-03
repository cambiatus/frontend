module UpdateResult exposing
    ( UpdateResult
    , addCmd
    , addExt
    , addMsg
    , addPort
    , init
    , logDebugValue
    , logDecodeError
    , logGraphqlError
    , logHttpError
    , logImpossible
    , map
    , mapModel
    , remoteDataToString
    , resultToString
    , setModel
    , toModelCmd
    )

{- This library allows us to have an observable update function which allows us to transmit message from
   various model to and fro other modules including the Main module. This enables us to request side effects from within modules
   while maintaining a flexible codebase.
   In a scenario such as displaying a request to sign a transfer or a sale is where the usefulness of this module really shines through

   # Definition
   @docs UpdateResult

   # Common Helpers
   @docs init, addCmd, addMsg, addExt, addPort,  logHttpError, logImpossible, logGraphqlError, toModelCmd

   # Mapping UpdateResults
   @docs map, mapModel, setModel

-}

import Graphql.Http
import Http
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Log exposing (Log)
import Ports
import RemoteData exposing (RemoteData(..))
import Task


{-| Core data structure that enables this project to have an observable update function in our module.
The data structure contains the following

1.  A model, which is the model in the Main model, which in turn contains a status
    that is comprised of the subModel in question and some metadata.
2.  A list of Cmd messages that will be applied to the model
3.  A list of external messages to be applied to the model and submodel
4.  A list of ports to execute
5.  A list of log messages to be applied

-}
type alias UpdateResult model msg extMsg =
    { model : model
    , cmds : List (Cmd msg)
    , exts : List extMsg
    , ports : List (Ports.JavascriptOut msg)
    , logs : List (Log msg)
    }


{-| Applies commands, ports and Logs messages to an UpdateResult dataset resulting in new UpdateResult dataset with all
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
        , logs = List.map (Log.map toMsg) updateResult.logs
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
    , logs = uResult.logs
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
    , logs = uResult.logs
    }


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
    , logs = []
    }


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
                    ++ List.map (Ports.javascriptOutCmd msgToString) uResult.ports
                    ++ List.map (Log.send msgToString) uResult.logs
                )
            )


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


{-| Add a log msg to a command an UpdateResult
-}
addLog : Log msg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
addLog log uResult =
    { uResult | logs = uResult.logs ++ [ log ] }


{-| Logs out an httpError to the development console in dev env or to Error reporting in
production
-}
logHttpError : msg -> Http.Error -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logHttpError msg httpError uResult =
    addLog
        (Log.log { msg = msg, kind = Log.HttpError httpError })
        uResult


{-| Logs an Impossible state the development console in the development environment or does an Incident report
in production
-}
logImpossible : msg -> List String -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logImpossible msg descr uResult =
    addLog
        (Log.log { msg = msg, kind = Log.Impossible descr })
        uResult


{-| Logs a decoding error to the development console in the development environment or does an Incident report
in production
-}
logDecodeError : msg -> Decode.Error -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logDecodeError msg descr uResult =
    addLog
        (Log.log { msg = msg, kind = Log.DecodeError descr })
        uResult


{-| Logs a Graphql error the development console in the development environment or does an Incident report
in production
-}
logGraphqlError : msg -> Graphql.Http.Error a -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logGraphqlError msg graphqlError uResult =
    addLog
        (Log.log { msg = msg, kind = Log.graphqlErrorToKind graphqlError })
        uResult


{-| Logs a JSON value the development console in the development environment
-}
logDebugValue : msg -> Value -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logDebugValue msg val uResult =
    addLog
        (Log.log { msg = msg, kind = Log.DebugValue val })
        uResult


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
