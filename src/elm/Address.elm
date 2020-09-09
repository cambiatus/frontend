module Address exposing (City, Country, Neighborhood, State, countryQuery)

import Cambiatus.Object
import Cambiatus.Object.City
import Cambiatus.Object.Country
import Cambiatus.Object.Neighborhood
import Cambiatus.Object.State
import Cambiatus.Query
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet, with)


type alias Country =
    { name : String
    , states : List State
    }


type alias State =
    { name : String
    , cities : List City
    }


type alias City =
    { name : String
    , neighborhoods : List Neighborhood
    }


type alias Neighborhood =
    { name : String }


countrySelectionSet : SelectionSet Country Cambiatus.Object.Country
countrySelectionSet =
    Graphql.SelectionSet.succeed Country
        |> with Cambiatus.Object.Country.name
        |> with (Cambiatus.Object.Country.states stateSelectionSet)


stateSelectionSet : SelectionSet State Cambiatus.Object.State
stateSelectionSet =
    Graphql.SelectionSet.succeed State
        |> with Cambiatus.Object.State.name
        |> with (Cambiatus.Object.State.cities citySelectionSet)


citySelectionSet : SelectionSet City Cambiatus.Object.City
citySelectionSet =
    Graphql.SelectionSet.succeed City
        |> with Cambiatus.Object.City.name
        |> with (Cambiatus.Object.City.neighborhoods neighborhoodSelectionSet)


neighborhoodSelectionSet : SelectionSet Neighborhood Cambiatus.Object.Neighborhood
neighborhoodSelectionSet =
    Graphql.SelectionSet.succeed Neighborhood
        |> with Cambiatus.Object.Neighborhood.name


countryQuery : String -> SelectionSet (Maybe Country) RootQuery
countryQuery name =
    Cambiatus.Query.country { input = { name = name } } countrySelectionSet
