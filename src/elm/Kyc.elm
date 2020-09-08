module Kyc exposing
    ( ProfileKyc
    , decode
    , selectionSet
    )

import Cambiatus.Object
import Cambiatus.Object.KycData as KycData
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline exposing (required)


type alias ProfileKyc =
    { userType : String
    , documentType : String
    , document : String
    , phone : String
    , isVerified : Bool
    }


selectionSet : SelectionSet ProfileKyc Cambiatus.Object.KycData
selectionSet =
    SelectionSet.succeed ProfileKyc
        |> with KycData.userType
        |> with KycData.documentType
        |> with KycData.document
        |> with KycData.phone
        |> with KycData.isVerified


decode : Decoder ProfileKyc
decode =
    Decode.succeed ProfileKyc
        |> required "userType" string
        |> required "documentType" string
        |> required "document" string
        |> required "phone" string
        |> required "isVerified" bool
