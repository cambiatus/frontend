module Profile.Contact exposing
    ( Contact
    , contactTypeFromString
    , decode
    , encode
    , usesPhone
    , usesPhoneFromString
    )

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


usesPhoneFromString : String -> Bool
usesPhoneFromString contactTypeString =
    contactTypeFromString contactTypeString
        |> Maybe.map usesPhone
        |> Maybe.withDefault False


contactTypeFromString : String -> Maybe ContactType
contactTypeFromString contactTypeString =
    String.toUpper contactTypeString
        |> ContactType.fromString


usesPhone : ContactType -> Bool
usesPhone contactType =
    List.member contactType [ ContactType.Whatsapp, ContactType.Phone ]
