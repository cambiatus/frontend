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


{-| [Auth required] A single claim
-}
claim :
    ClaimRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.Claim
    -> SelectionSet decodesTo RootQuery
claim requiredArgs object_ =
    Object.selectionForCompositeField "claim" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeClaimInput ] object_ identity


type alias ClaimsAnalysisOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , filter : OptionalArgument Cambiatus.InputObject.ClaimAnalysisFilter
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


type alias ClaimsAnalysisRequiredArguments =
    { communityId : String }


{-| [Auth required] A list of claims
-}
claimsAnalysis :
    (ClaimsAnalysisOptionalArguments -> ClaimsAnalysisOptionalArguments)
    -> ClaimsAnalysisRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.ClaimConnection
    -> SelectionSet (Maybe decodesTo) RootQuery
claimsAnalysis fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, filter = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "filter" filledInOptionals.filter Cambiatus.InputObject.encodeClaimAnalysisFilter, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "claimsAnalysis" (optionalArgs ++ [ Argument.required "communityId" requiredArgs.communityId Encode.string ]) object_ (identity >> Decode.nullable)


type alias ClaimsAnalysisHistoryOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , filter : OptionalArgument Cambiatus.InputObject.ClaimAnalysisHistoryFilter
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


type alias ClaimsAnalysisHistoryRequiredArguments =
    { communityId : String }


claimsAnalysisHistory :
    (ClaimsAnalysisHistoryOptionalArguments -> ClaimsAnalysisHistoryOptionalArguments)
    -> ClaimsAnalysisHistoryRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.ClaimConnection
    -> SelectionSet (Maybe decodesTo) RootQuery
claimsAnalysisHistory fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, filter = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "filter" filledInOptionals.filter Cambiatus.InputObject.encodeClaimAnalysisHistoryFilter, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "claimsAnalysisHistory" (optionalArgs ++ [ Argument.required "communityId" requiredArgs.communityId Encode.string ]) object_ (identity >> Decode.nullable)


{-| [Auth required] A list of communities in Cambiatus
-}
communities :
    SelectionSet decodesTo Cambiatus.Object.Community
    -> SelectionSet (List decodesTo) RootQuery
communities object_ =
    Object.selectionForCompositeField "communities" [] object_ (identity >> Decode.list)


type alias CommunityOptionalArguments =
    { subdomain : OptionalArgument String
    , symbol : OptionalArgument String
    }


{-| [Auth required] A single community
-}
community :
    (CommunityOptionalArguments -> CommunityOptionalArguments)
    -> SelectionSet decodesTo Cambiatus.Object.Community
    -> SelectionSet (Maybe decodesTo) RootQuery
community fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { subdomain = Absent, symbol = Absent }

        optionalArgs =
            [ Argument.optional "subdomain" filledInOptionals.subdomain Encode.string, Argument.optional "symbol" filledInOptionals.symbol Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "community" optionalArgs object_ (identity >> Decode.nullable)


type alias CommunityPreviewOptionalArguments =
    { subdomain : OptionalArgument String
    , symbol : OptionalArgument String
    }


{-| Community Preview, public data available for all communities
-}
communityPreview :
    (CommunityPreviewOptionalArguments -> CommunityPreviewOptionalArguments)
    -> SelectionSet decodesTo Cambiatus.Object.CommunityPreview
    -> SelectionSet (Maybe decodesTo) RootQuery
communityPreview fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { subdomain = Absent, symbol = Absent }

        optionalArgs =
            [ Argument.optional "subdomain" filledInOptionals.subdomain Encode.string, Argument.optional "symbol" filledInOptionals.symbol Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "communityPreview" optionalArgs object_ (identity >> Decode.nullable)


type alias CountryRequiredArguments =
    { input : Cambiatus.InputObject.CountryInput }


{-| List of supported countries
-}
country :
    CountryRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.Country
    -> SelectionSet (Maybe decodesTo) RootQuery
country requiredArgs object_ =
    Object.selectionForCompositeField "country" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeCountryInput ] object_ (identity >> Decode.nullable)


type alias DomainAvailableRequiredArguments =
    { domain : String }


{-| [Auth required] Informs if a domain is available or not under Cambiatus
-}
domainAvailable :
    DomainAvailableRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.Exists
    -> SelectionSet (Maybe decodesTo) RootQuery
domainAvailable requiredArgs object_ =
    Object.selectionForCompositeField "domainAvailable" [ Argument.required "domain" requiredArgs.domain Encode.string ] object_ (identity >> Decode.nullable)


type alias InviteRequiredArguments =
    { id : String }


{-| An invite
-}
invite :
    InviteRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.Invite
    -> SelectionSet (Maybe decodesTo) RootQuery
invite requiredArgs object_ =
    Object.selectionForCompositeField "invite" [ Argument.required "id" requiredArgs.id Encode.string ] object_ (identity >> Decode.nullable)


{-| [Auth required] User's notifications
-}
notificationHistory :
    SelectionSet decodesTo Cambiatus.Object.NotificationHistory
    -> SelectionSet (List decodesTo) RootQuery
notificationHistory object_ =
    Object.selectionForCompositeField "notificationHistory" [] object_ (identity >> Decode.list)


type alias ObjectiveRequiredArguments =
    { input : Cambiatus.InputObject.ObjectiveInput }


{-| [Auth required] A single objective
-}
objective :
    ObjectiveRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.Objective
    -> SelectionSet (Maybe decodesTo) RootQuery
objective requiredArgs object_ =
    Object.selectionForCompositeField "objective" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeObjectiveInput ] object_ (identity >> Decode.nullable)


type alias ProductRequiredArguments =
    { id : Cambiatus.ScalarCodecs.CustomId }


{-| [Auth required] Gets a single product
-}
product :
    ProductRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.Product
    -> SelectionSet (Maybe decodesTo) RootQuery
product requiredArgs object_ =
    Object.selectionForCompositeField "product" [ Argument.required "id" requiredArgs.id (Cambiatus.ScalarCodecs.codecs |> Cambiatus.Scalar.unwrapEncoder .codecCustomId) ] object_ (identity >> Decode.nullable)


type alias ProductsOptionalArguments =
    { filters : OptionalArgument Cambiatus.InputObject.ProductsFilterInput }


type alias ProductsRequiredArguments =
    { communityId : String }


{-| [Auth required] Products in a community
-}
products :
    (ProductsOptionalArguments -> ProductsOptionalArguments)
    -> ProductsRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.Product
    -> SelectionSet (List decodesTo) RootQuery
products fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { filters = Absent }

        optionalArgs =
            [ Argument.optional "filters" filledInOptionals.filters Cambiatus.InputObject.encodeProductsFilterInput ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "products" (optionalArgs ++ [ Argument.required "communityId" requiredArgs.communityId Encode.string ]) object_ (identity >> Decode.list)


type alias SearchRequiredArguments =
    { communityId : String }


{-| [Auth required] Searches the community for a product or action
-}
search :
    SearchRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.SearchResult
    -> SelectionSet decodesTo RootQuery
search requiredArgs object_ =
    Object.selectionForCompositeField "search" [ Argument.required "communityId" requiredArgs.communityId Encode.string ] object_ identity


type alias TransferRequiredArguments =
    { input : Cambiatus.InputObject.TransferInput }


{-| [Auth required] A single Transfer
-}
transfer :
    TransferRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.Transfer
    -> SelectionSet (Maybe decodesTo) RootQuery
transfer requiredArgs object_ =
    Object.selectionForCompositeField "transfer" [ Argument.required "input" requiredArgs.input Cambiatus.InputObject.encodeTransferInput ] object_ (identity >> Decode.nullable)


type alias UserRequiredArguments =
    { account : String }


{-| [Auth required] A user
-}
user :
    UserRequiredArguments
    -> SelectionSet decodesTo Cambiatus.Object.User
    -> SelectionSet (Maybe decodesTo) RootQuery
user requiredArgs object_ =
    Object.selectionForCompositeField "user" [ Argument.required "account" requiredArgs.account Encode.string ] object_ (identity >> Decode.nullable)
