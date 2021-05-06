-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Cambiatus.Object.Upload exposing (..)

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


insertedAt : SelectionSet (Maybe Cambiatus.ScalarCodecs.NaiveDateTime) Cambiatus.Object.Upload
insertedAt =
    Object.selectionForField "(Maybe ScalarCodecs.NaiveDateTime)" "insertedAt" [] (Cambiatus.ScalarCodecs.codecs |> Cambiatus.Scalar.unwrapCodecs |> .codecNaiveDateTime |> .decoder |> Decode.nullable)


updatedAt : SelectionSet Cambiatus.ScalarCodecs.NaiveDateTime Cambiatus.Object.Upload
updatedAt =
    Object.selectionForField "ScalarCodecs.NaiveDateTime" "updatedAt" [] (Cambiatus.ScalarCodecs.codecs |> Cambiatus.Scalar.unwrapCodecs |> .codecNaiveDateTime |> .decoder)


url : SelectionSet String Cambiatus.Object.Upload
url =
    Object.selectionForField "String" "url" [] Decode.string
