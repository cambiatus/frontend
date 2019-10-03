-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Bespiral.Object.NotificationHistory exposing (id, insertedAt, isRead, payload, recipient, recipientId, type_, updatedAt)

import Bespiral.InputObject
import Bespiral.Interface
import Bespiral.Object
import Bespiral.Scalar
import Bespiral.ScalarCodecs
import Bespiral.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


id : SelectionSet Int Bespiral.Object.NotificationHistory
id =
    Object.selectionForField "Int" "id" [] Decode.int


insertedAt : SelectionSet Bespiral.ScalarCodecs.DateTime Bespiral.Object.NotificationHistory
insertedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "insertedAt" [] (Bespiral.ScalarCodecs.codecs |> Bespiral.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


isRead : SelectionSet Bool Bespiral.Object.NotificationHistory
isRead =
    Object.selectionForField "Bool" "isRead" [] Decode.bool


payload : SelectionSet decodesTo Bespiral.Union.NotificationType -> SelectionSet decodesTo Bespiral.Object.NotificationHistory
payload object_ =
    Object.selectionForCompositeField "payload" [] object_ identity


recipient : SelectionSet decodesTo Bespiral.Object.Profile -> SelectionSet decodesTo Bespiral.Object.NotificationHistory
recipient object_ =
    Object.selectionForCompositeField "recipient" [] object_ identity


recipientId : SelectionSet String Bespiral.Object.NotificationHistory
recipientId =
    Object.selectionForField "String" "recipientId" [] Decode.string


type_ : SelectionSet String Bespiral.Object.NotificationHistory
type_ =
    Object.selectionForField "String" "type" [] Decode.string


updatedAt : SelectionSet Bespiral.ScalarCodecs.DateTime Bespiral.Object.NotificationHistory
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Bespiral.ScalarCodecs.codecs |> Bespiral.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)
