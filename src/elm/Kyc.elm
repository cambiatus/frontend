module Kyc exposing
    ( ProfileKyc
    , selectionSet
    )

import Cambiatus.Object
import Cambiatus.Object.KycData as KycData
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)


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
