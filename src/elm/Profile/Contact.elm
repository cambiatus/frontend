module Profile.Contact exposing (Contact, decode, encode)

import Cambiatus.Enum.ContactType as ContactType exposing (ContactType)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Contact =
    { contactType : ContactType
    , contact : String
    }


decode : Decoder Contact
decode =
    Decode.map2 Contact
        ContactType.decoder
        Decode.string


encode : Contact -> Encode.Value
encode { contactType, contact } =
    Encode.object
        [ ( "type", Encode.string (ContactType.toString contactType) )
        , ( "externalId", Encode.string contact )
        ]
