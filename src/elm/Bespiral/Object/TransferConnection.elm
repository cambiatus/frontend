-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Bespiral.Object.TransferConnection exposing (edges, fetchedCount, pageInfo, totalCount)

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


edges : SelectionSet decodesTo Bespiral.Object.TransferEdge -> SelectionSet (Maybe (List (Maybe decodesTo))) Bespiral.Object.TransferConnection
edges object_ =
    Object.selectionForCompositeField "edges" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


fetchedCount : SelectionSet (Maybe Int) Bespiral.Object.TransferConnection
fetchedCount =
    Object.selectionForField "(Maybe Int)" "fetchedCount" [] (Decode.int |> Decode.nullable)


pageInfo : SelectionSet decodesTo Bespiral.Object.PageInfo -> SelectionSet decodesTo Bespiral.Object.TransferConnection
pageInfo object_ =
    Object.selectionForCompositeField "pageInfo" [] object_ identity


totalCount : SelectionSet (Maybe Int) Bespiral.Object.TransferConnection
totalCount =
    Object.selectionForField "(Maybe Int)" "totalCount" [] (Decode.int |> Decode.nullable)
