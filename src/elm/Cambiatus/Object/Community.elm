-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Cambiatus.Object.Community exposing (..)

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
import Json.Decode as Decode


orderCount : SelectionSet Int Cambiatus.Object.Community
orderCount =
    Object.selectionForField "Int" "orderCount" [] Decode.int


members :
    SelectionSet decodesTo Cambiatus.Object.User
    -> SelectionSet (List decodesTo) Cambiatus.Object.Community
members object_ =
    Object.selectionForCompositeField "members" [] object_ (identity >> Decode.list)


transferCount : SelectionSet Int Cambiatus.Object.Community
transferCount =
    Object.selectionForField "Int" "transferCount" [] Decode.int


createdAt : SelectionSet Cambiatus.ScalarCodecs.DateTime Cambiatus.Object.Community
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Cambiatus.ScalarCodecs.codecs |> Cambiatus.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


issuer : SelectionSet (Maybe String) Cambiatus.Object.Community
issuer =
    Object.selectionForField "(Maybe String)" "issuer" [] (Decode.string |> Decode.nullable)


logo : SelectionSet String Cambiatus.Object.Community
logo =
    Object.selectionForField "String" "logo" [] Decode.string


objectives :
    SelectionSet decodesTo Cambiatus.Object.Objective
    -> SelectionSet (List decodesTo) Cambiatus.Object.Community
objectives object_ =
    Object.selectionForCompositeField "objectives" [] object_ (identity >> Decode.list)


hasShop : SelectionSet Bool Cambiatus.Object.Community
hasShop =
    Object.selectionForField "Bool" "hasShop" [] Decode.bool


createdEosAccount : SelectionSet String Cambiatus.Object.Community
createdEosAccount =
    Object.selectionForField "String" "createdEosAccount" [] Decode.string


minBalance : SelectionSet (Maybe Float) Cambiatus.Object.Community
minBalance =
    Object.selectionForField "(Maybe Float)" "minBalance" [] (Decode.float |> Decode.nullable)


createdTx : SelectionSet String Cambiatus.Object.Community
createdTx =
    Object.selectionForField "String" "createdTx" [] Decode.string


subdomain :
    SelectionSet decodesTo Cambiatus.Object.Subdomain
    -> SelectionSet (Maybe decodesTo) Cambiatus.Object.Community
subdomain object_ =
    Object.selectionForCompositeField "subdomain" [] object_ (identity >> Decode.nullable)


mints :
    SelectionSet decodesTo Cambiatus.Object.Mint
    -> SelectionSet (List decodesTo) Cambiatus.Object.Community
mints object_ =
    Object.selectionForCompositeField "mints" [] object_ (identity >> Decode.list)


supply : SelectionSet (Maybe Float) Cambiatus.Object.Community
supply =
    Object.selectionForField "(Maybe Float)" "supply" [] (Decode.float |> Decode.nullable)


creator : SelectionSet String Cambiatus.Object.Community
creator =
    Object.selectionForField "String" "creator" [] Decode.string


invitedReward : SelectionSet Float Cambiatus.Object.Community
invitedReward =
    Object.selectionForField "Float" "invitedReward" [] Decode.float


actionCount : SelectionSet Int Cambiatus.Object.Community
actionCount =
    Object.selectionForField "Int" "actionCount" [] Decode.int


productCount : SelectionSet Int Cambiatus.Object.Community
productCount =
    Object.selectionForField "Int" "productCount" [] Decode.int


description : SelectionSet String Cambiatus.Object.Community
description =
    Object.selectionForField "String" "description" [] Decode.string


inviterReward : SelectionSet Float Cambiatus.Object.Community
inviterReward =
    Object.selectionForField "Float" "inviterReward" [] Decode.float


type alias TransfersOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


transfers :
    (TransfersOptionalArguments -> TransfersOptionalArguments)
    -> SelectionSet decodesTo Cambiatus.Object.TransferConnection
    -> SelectionSet (Maybe decodesTo) Cambiatus.Object.Community
transfers fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "transfers" optionalArgs object_ (identity >> Decode.nullable)


orders :
    SelectionSet decodesTo Cambiatus.Object.Order
    -> SelectionSet (List decodesTo) Cambiatus.Object.Community
orders object_ =
    Object.selectionForCompositeField "orders" [] object_ (identity >> Decode.list)


claimCount : SelectionSet Int Cambiatus.Object.Community
claimCount =
    Object.selectionForField "Int" "claimCount" [] Decode.int


hasKyc : SelectionSet Bool Cambiatus.Object.Community
hasKyc =
    Object.selectionForField "Bool" "hasKyc" [] Decode.bool


memberCount : SelectionSet Int Cambiatus.Object.Community
memberCount =
    Object.selectionForField "Int" "memberCount" [] Decode.int


hasObjectives : SelectionSet Bool Cambiatus.Object.Community
hasObjectives =
    Object.selectionForField "Bool" "hasObjectives" [] Decode.bool


symbol : SelectionSet String Cambiatus.Object.Community
symbol =
    Object.selectionForField "String" "symbol" [] Decode.string


autoInvite : SelectionSet Bool Cambiatus.Object.Community
autoInvite =
    Object.selectionForField "Bool" "autoInvite" [] Decode.bool


{-| List of users that are claim validators
-}
validators :
    SelectionSet decodesTo Cambiatus.Object.User
    -> SelectionSet (List decodesTo) Cambiatus.Object.Community
validators object_ =
    Object.selectionForCompositeField "validators" [] object_ (identity >> Decode.list)


maxSupply : SelectionSet (Maybe Float) Cambiatus.Object.Community
maxSupply =
    Object.selectionForField "(Maybe Float)" "maxSupply" [] (Decode.float |> Decode.nullable)


type_ : SelectionSet (Maybe String) Cambiatus.Object.Community
type_ =
    Object.selectionForField "(Maybe String)" "type" [] (Decode.string |> Decode.nullable)


name : SelectionSet String Cambiatus.Object.Community
name =
    Object.selectionForField "String" "name" [] Decode.string


createdBlock : SelectionSet Int Cambiatus.Object.Community
createdBlock =
    Object.selectionForField "Int" "createdBlock" [] Decode.int
