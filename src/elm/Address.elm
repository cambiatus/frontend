module Address exposing
    ( ProfileAddress
    , decode
    , selectionSet
    )

import Cambiatus.Object exposing (Address(..))
import Cambiatus.Object.Address as Address
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode exposing (Decoder, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)


type alias ProfileAddress =
    { zip : String
    , street : String
    , number : Maybe String
    }


selectionSet : SelectionSet ProfileAddress Cambiatus.Object.Address
selectionSet =
    SelectionSet.succeed ProfileAddress
        |> with Address.zip
        |> with Address.street
        |> with Address.number


decode : Decoder ProfileAddress
decode =
    Decode.succeed ProfileAddress
        |> required "zip" string
        |> required "street" string
        |> optional "number" (nullable string) Nothing
