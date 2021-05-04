-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Cambiatus.Object.CommunityPreview exposing (..)

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


autoInvite : SelectionSet Bool Cambiatus.Object.CommunityPreview
autoInvite =
    Object.selectionForField "Bool" "autoInvite" [] Decode.bool


description : SelectionSet String Cambiatus.Object.CommunityPreview
description =
    Object.selectionForField "String" "description" [] Decode.string


hasKyc : SelectionSet Bool Cambiatus.Object.CommunityPreview
hasKyc =
    Object.selectionForField "Bool" "hasKyc" [] Decode.bool


hasObjectives : SelectionSet Bool Cambiatus.Object.CommunityPreview
hasObjectives =
    Object.selectionForField "Bool" "hasObjectives" [] Decode.bool


hasShop : SelectionSet Bool Cambiatus.Object.CommunityPreview
hasShop =
    Object.selectionForField "Bool" "hasShop" [] Decode.bool


logo : SelectionSet String Cambiatus.Object.CommunityPreview
logo =
    Object.selectionForField "String" "logo" [] Decode.string


memberCount : SelectionSet Int Cambiatus.Object.CommunityPreview
memberCount =
    Object.selectionForField "Int" "memberCount" [] Decode.int


name : SelectionSet String Cambiatus.Object.CommunityPreview
name =
    Object.selectionForField "String" "name" [] Decode.string


subdomain :
    SelectionSet decodesTo Cambiatus.Object.Subdomain
    -> SelectionSet (Maybe decodesTo) Cambiatus.Object.CommunityPreview
subdomain object_ =
    Object.selectionForCompositeField "subdomain" [] object_ (identity >> Decode.nullable)


symbol : SelectionSet String Cambiatus.Object.CommunityPreview
symbol =
    Object.selectionForField "String" "symbol" [] Decode.string


uploads :
    SelectionSet decodesTo Cambiatus.Object.Upload
    -> SelectionSet (Maybe (List (Maybe decodesTo))) Cambiatus.Object.CommunityPreview
uploads object_ =
    Object.selectionForCompositeField "uploads" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


website : SelectionSet (Maybe String) Cambiatus.Object.CommunityPreview
website =
    Object.selectionForField "(Maybe String)" "website" [] (Decode.string |> Decode.nullable)
