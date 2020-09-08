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
    { country : AddressCountry
    , state : AddressState
    , city : AddressCity
    , neighborhood : AddressNeighborhood
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


type alias AddressCountry =
    { name : String }


countrySelectionSet : SelectionSet AddressCountry Cambiatus.Object.Country
countrySelectionSet =
    SelectionSet.succeed AddressCountry
        |> with Country.name


decodeCountry : Decoder AddressCountry
decodeCountry =
    Decode.succeed AddressCountry
        |> required "name" string



-- STATE


type alias AddressState =
    { name : String }


stateSelectionSet : SelectionSet AddressState Cambiatus.Object.State
stateSelectionSet =
    SelectionSet.succeed AddressState
        |> with State.name


decodeState : Decoder AddressState
decodeState =
    Decode.succeed AddressState
        |> required "name" string



-- CITY


type alias AddressCity =
    { name : String }


citySelectionSet : SelectionSet AddressCity Cambiatus.Object.City
citySelectionSet =
    SelectionSet.succeed AddressCity
        |> with City.name


decodeCity : Decoder AddressCity
decodeCity =
    Decode.succeed AddressCity
        |> required "name" string



-- NEIGHBORHOOD


type alias AddressNeighborhood =
    { name : String }


neighborhoodSelectionSet : SelectionSet AddressNeighborhood Cambiatus.Object.Neighborhood
neighborhoodSelectionSet =
    SelectionSet.succeed AddressNeighborhood
        |> with Neighborhood.name


decodeNeighborhood : Decoder AddressNeighborhood
decodeNeighborhood =
    Decode.succeed AddressNeighborhood
        |> required "name" string
