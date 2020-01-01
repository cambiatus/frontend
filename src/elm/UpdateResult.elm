module UpdateResult exposing (UpdateResult, addCmd, addExt, addLog, addPort, init, logDebugValue, logDecodeError, logGraphqlError, logHttpError, logImpossible, map, mapModel, resultToString, setModel, toModelCmd)

import Graphql.Http
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Log exposing (Log)
import Ports


type alias UpdateResult model msg extMsg =
    { model : model
    , cmds : List (Cmd msg)
    , exts : List extMsg
    , ports : List (Ports.JavascriptOut msg)
    , logs : List (Log msg)
    }


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





mapModel : (m -> m2) -> UpdateResult m msg eMsg -> UpdateResult m2 msg eMsg
mapModel transform uResult =
    { model = transform uResult.model
    , cmds = uResult.cmds
    , exts = uResult.exts
    , ports = uResult.ports
    , logs = uResult.logs
    }


setModel : UpdateResult m msg eMsg -> m2 -> UpdateResult m2 msg eMsg
setModel uResult model =
    { model = model
    , cmds = uResult.cmds
    , exts = uResult.exts
    , ports = uResult.ports
    , logs = uResult.logs
    }


init : model -> UpdateResult model msg extMsg
init model =
    { model = model
    , cmds = []
    , exts = []
    , ports = []
    , logs = []
    }


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


addCmd : Cmd msg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
addCmd cmd uResult =
    { uResult | cmds = uResult.cmds ++ [ cmd ] }


addExt : eMsg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
addExt eMsg uResult =
    { uResult | exts = uResult.exts ++ [ eMsg ] }


addPort : Ports.JavascriptOutModel msg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
addPort port_ uResult =
    { uResult | ports = uResult.ports ++ [ Ports.javascriptOut port_ ] }


addLog : Log msg -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
addLog log uResult =
    { uResult | logs = uResult.logs ++ [ log ] }


logHttpError : msg -> Http.Error -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logHttpError msg httpError uResult =
    addLog
        (Log.log { msg = msg, kind = Log.HttpError httpError })
        uResult


logImpossible : msg -> List String -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logImpossible msg descr uResult =
    addLog
        (Log.log { msg = msg, kind = Log.Impossible descr })
        uResult


logDecodeError : msg -> Decode.Error -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logDecodeError msg descr uResult =
    addLog
        (Log.log { msg = msg, kind = Log.DecodeError descr })
        uResult


logGraphqlError : msg -> Graphql.Http.Error a -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logGraphqlError msg graphqlError uResult =
    addLog
        (Log.log { msg = msg, kind = Log.graphqlErrorToKind graphqlError })
        uResult


logDebugValue : msg -> Value -> UpdateResult m msg eMsg -> UpdateResult m msg eMsg
logDebugValue msg val uResult =
    addLog
        (Log.log { msg = msg, kind = Log.DebugValue val })
        uResult


resultToString : Result x a -> String
resultToString r =
    case r of
        Ok _ ->
            "Ok"

        Err _ ->
            "Err"
