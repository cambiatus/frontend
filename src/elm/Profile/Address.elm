module Profile.Address exposing
    ( Address
    , selectionSet
    )

import Cambiatus.Object
import Cambiatus.Object.Address as Address
import Cambiatus.Object.City as City
import Cambiatus.Object.Country as Country
import Cambiatus.Object.Neighborhood as Neighborhood
import Cambiatus.Object.State as State
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)



-- ADDRESS


type alias Address =
    { country : String
    , state : String
    , city : String
    , neighborhood : String
    , street : String
    , number : String
    , zip : String
    }


selectionSet : SelectionSet Address Cambiatus.Object.Address
selectionSet =
    SelectionSet.succeed Address
        |> with
            (Address.country countrySelectionSet
                |> SelectionSet.map .name
            )
        |> with
            (Address.state stateSelectionSet
                |> SelectionSet.map .name
            )
        |> with
            (Address.city citySelectionSet
                |> SelectionSet.map .name
            )
        |> with
            (Address.neighborhood neighborhoodSelectionSet
                |> SelectionSet.map .name
            )
        |> with Address.street
        |> with
            (Address.number
                |> SelectionSet.map (Maybe.withDefault "")
            )
        |> with Address.zip



-- COUNTRY


type alias Country =
    { name : String }


countrySelectionSet : SelectionSet Country Cambiatus.Object.Country
countrySelectionSet =
    SelectionSet.succeed Country
        |> with Country.name



-- STATE


type alias State =
    { name : String }


stateSelectionSet : SelectionSet State Cambiatus.Object.State
stateSelectionSet =
    SelectionSet.succeed State
        |> with State.name



-- CITY


type alias City =
    { name : String }


citySelectionSet : SelectionSet City Cambiatus.Object.City
citySelectionSet =
    SelectionSet.succeed City
        |> with City.name



-- NEIGHBORHOOD


type alias Neighborhood =
    { name : String }


neighborhoodSelectionSet : SelectionSet Neighborhood Cambiatus.Object.Neighborhood
neighborhoodSelectionSet =
    SelectionSet.succeed Neighborhood
        |> with Neighborhood.name
