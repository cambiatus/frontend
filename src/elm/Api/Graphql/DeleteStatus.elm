module Api.Graphql.DeleteStatus exposing (DeleteStatus(..), ErrorReason(..), selectionSet)

import Cambiatus.Object
import Cambiatus.Object.DeleteStatus
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)


type DeleteStatus
    = Deleted
    | Error ErrorReason


type ErrorReason
    = ApiError String
    | InvalidStatus
    | UnknownError


selectionSet :
    (SelectionSet DeleteStatus Cambiatus.Object.DeleteStatus
     -> SelectionSet (Maybe DeleteStatus) typeLock
    )
    -> SelectionSet DeleteStatus typeLock
selectionSet fn =
    fn internalSelectionSet
        |> SelectionSet.map (Maybe.withDefault (Error UnknownError))


internalSelectionSet : SelectionSet DeleteStatus Cambiatus.Object.DeleteStatus
internalSelectionSet =
    SelectionSet.succeed
        (\reason status ->
            case status of
                "success" ->
                    Deleted

                "error" ->
                    Error (ApiError reason)

                _ ->
                    Error InvalidStatus
        )
        |> SelectionSet.with Cambiatus.Object.DeleteStatus.reason
        |> SelectionSet.with Cambiatus.Object.DeleteStatus.status
