-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Cambiatus.Query exposing (..)

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


type alias ClaimRequiredArguments =
    { input : Cambiatus.InputObject.ClaimInput }


{-| A single claim
-}
claim : ClaimRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.Claim -> SelectionSet decodesTo RootQuery
claim requiredArgs object_ =
    Object.selectionForCompositeField "claim" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeClaimInput ] object_ identity


type alias ClaimsAnalysisOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


type alias ClaimsAnalysisRequiredArguments =
    { input : Cambiatus.InputObject.ClaimsAnalysisInput }


{-| A list of claims
-}
claimsAnalysis : (ClaimsAnalysisOptionalArguments -> ClaimsAnalysisOptionalArguments) -> ClaimsAnalysisRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.ClaimConnection -> SelectionSet (Maybe decodesTo) RootQuery
claimsAnalysis fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "claimsAnalysis" (optionalArgs ++ [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeClaimsAnalysisInput ]) object_ (identity >> Decode.nullable)


type alias ClaimsAnalysisHistoryOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


type alias ClaimsAnalysisHistoryRequiredArguments =
    { input : Cambiatus.InputObject.ClaimAnalysisHistoryInput }


claimsAnalysisHistory : (ClaimsAnalysisHistoryOptionalArguments -> ClaimsAnalysisHistoryOptionalArguments) -> ClaimsAnalysisHistoryRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.ClaimConnection -> SelectionSet (Maybe decodesTo) RootQuery
claimsAnalysisHistory fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "claimsAnalysisHistory" (optionalArgs ++ [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeClaimAnalysisHistoryInput ]) object_ (identity >> Decode.nullable)


{-| A list of communities in Cambiatus
-}
communities : SelectionSet decodesTo Cambiatus.Object.Community -> SelectionSet (List decodesTo) RootQuery
communities object_ =
    Object.selectionForCompositeField "communities" [] object_ (identity >> Decode.list)


type alias CommunityRequiredArguments =
    { symbol : String }


{-| A single community
-}
community : CommunityRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.Community -> SelectionSet (Maybe decodesTo) RootQuery
community requiredArgs object_ =
    Object.selectionForCompositeField "community" [ Argument.required "symbol" requiredArgs.symbol Encode.string ] object_ (identity >> Decode.nullable)


type alias InviteRequiredArguments =
    { input : Cambiatus.InputObject.InviteInput }


{-| An invite
-}
invite : InviteRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.Invite -> SelectionSet (Maybe decodesTo) RootQuery
invite requiredArgs object_ =
    Object.selectionForCompositeField "invite" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeInviteInput ] object_ (identity >> Decode.nullable)


type alias NotificationHistoryRequiredArguments =
    { account : String }


notificationHistory : NotificationHistoryRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.NotificationHistory -> SelectionSet (List decodesTo) RootQuery
notificationHistory requiredArgs object_ =
    Object.selectionForCompositeField "notificationHistory" [ Argument.required "account" requiredArgs.account Encode.string ] object_ (identity >> Decode.list)


type alias ObjectiveRequiredArguments =
    { input : Cambiatus.InputObject.ObjectiveInput }


{-| A single objective
-}
objective : ObjectiveRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.Objective -> SelectionSet (Maybe decodesTo) RootQuery
objective requiredArgs object_ =
    Object.selectionForCompositeField "objective" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeObjectiveInput ] object_ (identity >> Decode.nullable)


type alias ProfileRequiredArguments =
    { input : Cambiatus.InputObject.ProfileInput }


{-| A users profile
-}
profile : ProfileRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.Profile -> SelectionSet (Maybe decodesTo) RootQuery
profile requiredArgs object_ =
    Object.selectionForCompositeField "profile" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeProfileInput ] object_ (identity >> Decode.nullable)


type alias SaleRequiredArguments =
    { input : Cambiatus.InputObject.SaleInput }


{-| A single sale from Cambiatus
-}
sale : SaleRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.Sale -> SelectionSet (Maybe decodesTo) RootQuery
sale requiredArgs object_ =
    Object.selectionForCompositeField "sale" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeSaleInput ] object_ (identity >> Decode.nullable)


{-| A list of sale history
-}
saleHistory : SelectionSet decodesTo Cambiatus.Object.SaleHistory -> SelectionSet (Maybe (List (Maybe decodesTo))) RootQuery
saleHistory object_ =
    Object.selectionForCompositeField "saleHistory" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias SalesRequiredArguments =
    { input : Cambiatus.InputObject.SalesInput }


{-| A list of sales in Cambiatus
-}
sales : SalesRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.Sale -> SelectionSet (List decodesTo) RootQuery
sales requiredArgs object_ =
    Object.selectionForCompositeField "sales" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeSalesInput ] object_ (identity >> Decode.list)


type alias TransferRequiredArguments =
    { input : Cambiatus.InputObject.TransferInput }


{-| A single Transfer
-}
transfer : TransferRequiredArguments -> SelectionSet decodesTo Cambiatus.Object.Transfer -> SelectionSet (Maybe decodesTo) RootQuery
transfer requiredArgs object_ =
    Object.selectionForCompositeField "transfer" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeTransferInput ] object_ (identity >> Decode.nullable)
