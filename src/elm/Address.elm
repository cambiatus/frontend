module Address exposing
    ( ProfileAddress
    , decode
    , selectionSet
    )

import Cambiatus.Object
import Cambiatus.Object.Address as Address
import Cambiatus.Object.City as City
import Cambiatus.Object.Country as Country
import Cambiatus.Object.Neighborhood as Neighborhood
import Cambiatus.Object.State as State
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode exposing (Decoder, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)



-- ADDRESS


type alias ProfileAddress =
    { country : Country
    , state : State
    , city : City
    , neighborhood : Neighborhood
    , street : String
    , number : Maybe String
    , zip : String
    }


selectionSet : SelectionSet ProfileAddress Cambiatus.Object.Address
selectionSet =
    SelectionSet.succeed ProfileAddress
        |> with (Address.country countrySelectionSet)
        |> with (Address.state stateSelectionSet)
        |> with (Address.neighborhood neighborhoodSelectionSet)
        |> with (Address.city citySelectionSet)
        |> with Address.street
        |> with Address.number
        |> with Address.zip


decode : Decoder ProfileAddress
decode =
    Decode.succeed ProfileAddress
        |> required "country" decodeCountry
        |> required "state" decodeState
        |> required "city" decodeCity
        |> required "neighborhood" decodeNeighborhood
        |> required "street" string
        |> optional "number" (nullable string) Nothing
        |> required "zip" string



-- COUNTRY


type alias Country =
    { name : String }


countrySelectionSet : SelectionSet Country Cambiatus.Object.Country
countrySelectionSet =
    SelectionSet.succeed Country
        |> with Country.name


decodeCountry : Decoder Country
decodeCountry =
    Decode.succeed Country
        |> required "name" string



-- STATE


type alias State =
    { name : String }


stateSelectionSet : SelectionSet State Cambiatus.Object.State
stateSelectionSet =
    SelectionSet.succeed State
        |> with State.name


decodeState : Decoder State
decodeState =
    Decode.succeed State
        |> required "name" string



-- CITY


type alias City =
    { name : String }


citySelectionSet : SelectionSet City Cambiatus.Object.City
citySelectionSet =
    SelectionSet.succeed City
        |> with City.name


decodeCity : Decoder City
decodeCity =
    Decode.succeed City
        |> required "name" string



-- NEIGHBORHOOD


type alias Neighborhood =
    { name : String }


neighborhoodSelectionSet : SelectionSet Neighborhood Cambiatus.Object.Neighborhood
neighborhoodSelectionSet =
    SelectionSet.succeed Neighborhood
        |> with Neighborhood.name


decodeNeighborhood : Decoder Neighborhood
decodeNeighborhood =
    Decode.succeed Neighborhood
        |> required "name" string
