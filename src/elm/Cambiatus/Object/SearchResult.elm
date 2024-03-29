-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Cambiatus.Object.SearchResult exposing (..)

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


type alias ActionsOptionalArguments =
    { query : OptionalArgument String }


actions :
    (ActionsOptionalArguments -> ActionsOptionalArguments)
    -> SelectionSet decodesTo Cambiatus.Object.Action
    -> SelectionSet (List decodesTo) Cambiatus.Object.SearchResult
actions fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { query = Absent }

        optionalArgs =
            [ Argument.optional "query" filledInOptionals.query Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "actions" optionalArgs object_ (identity >> Decode.list)


type alias MembersOptionalArguments =
    { filters : OptionalArgument Cambiatus.InputObject.MembersFilterInput }


members :
    (MembersOptionalArguments -> MembersOptionalArguments)
    -> SelectionSet decodesTo Cambiatus.Object.User
    -> SelectionSet (List decodesTo) Cambiatus.Object.SearchResult
members fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { filters = Absent }

        optionalArgs =
            [ Argument.optional "filters" filledInOptionals.filters Cambiatus.InputObject.encodeMembersFilterInput ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "members" optionalArgs object_ (identity >> Decode.list)


type alias ProductsOptionalArguments =
    { query : OptionalArgument String }


products :
    (ProductsOptionalArguments -> ProductsOptionalArguments)
    -> SelectionSet decodesTo Cambiatus.Object.Product
    -> SelectionSet (List decodesTo) Cambiatus.Object.SearchResult
products fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { query = Absent }

        optionalArgs =
            [ Argument.optional "query" filledInOptionals.query Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "products" optionalArgs object_ (identity >> Decode.list)
