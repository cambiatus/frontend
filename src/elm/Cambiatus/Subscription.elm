-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Cambiatus.Subscription exposing (..)

import Cambiatus.InputObject
import Cambiatus.Interface
import Cambiatus.Object
import Cambiatus.Scalar
import Cambiatus.ScalarCodecs
import Cambiatus.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)


type alias NewcommunityRequiredArguments =
    { input : Cambiatus.InputObject.NewCommunityInput }


{-| A subscription for new community addition
-}
newcommunity : NewcommunityRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.Community -> SelectionSet decodesTo RootSubscription
newcommunity requiredArgs object_ =
    Object.selectionForCompositeField "newcommunity" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeNewCommunityInput ] object_ identity


{-| A subscription for sale history
-}
saleHistoryOperation : SelectionSet decodesTo Cambiatus.Object.SaleHistory -> SelectionSet (Maybe decodesTo) RootSubscription
saleHistoryOperation object_ =
    Object.selectionForCompositeField "saleHistoryOperation" [] object_ (identity >> Decode.nullable)


{-| A subscription to resolve operations on the sales table
-}
salesOperation : SelectionSet decodesTo Cambiatus.Object.Sale -> SelectionSet (Maybe decodesTo) RootSubscription
salesOperation object_ =
    Object.selectionForCompositeField "salesOperation" [] object_ (identity >> Decode.nullable)


{-| A subscription for transfers
-}
transfers : SelectionSet decodesTo Cambiatus.Object.Transfer -> SelectionSet (Maybe decodesTo) RootSubscription
transfers object_ =
    Object.selectionForCompositeField "transfers" [] object_ (identity >> Decode.nullable)


type alias TransfersucceedRequiredArguments =
    { input : Cambiatus.InputObject.TransferSucceedInput }


transfersucceed : TransfersucceedRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.Transfer -> SelectionSet decodesTo RootSubscription
transfersucceed requiredArgs object_ =
    Object.selectionForCompositeField "transfersucceed" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeTransferSucceedInput ] object_ identity


type alias UnreadsRequiredArguments =
    { input : Cambiatus.InputObject.UnreadNotificationsSubscriptionInput }


{-| A subscription for the number of unread notifications
-}
unreads : UnreadsRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.UnreadNotifications -> SelectionSet decodesTo RootSubscription
unreads requiredArgs object_ =
    Object.selectionForCompositeField "unreads" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeUnreadNotificationsSubscriptionInput ] object_ identity
