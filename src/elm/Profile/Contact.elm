module Profile.Contact exposing
    ( Contact
    , Normalized
    , decode
    , encode
    , normalize
    , selectionSet
    , unWrap
    , usesPhone
    )

import Cambiatus.Enum.ContactType as ContactType exposing (ContactType)
import Cambiatus.Object
import Cambiatus.Object.Contact
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- MODEL


type alias Contact =
    { contactType : ContactType
    , contact : String
    }


{-| The contact string must be formatted in a specific way to be sent to the
backend, so we have this type to ensure the information is normalized
-}
type Normalized
    = Normalized Contact



-- JSON/GRAPHQL
-- All the data sent to/received from the backend should be normalized


decode : Decoder Normalized
decode =
    Decode.map2 (\contactType contact -> Normalized { contactType = contactType, contact = contact })
        ContactType.decoder
        Decode.string


encode : Normalized -> Encode.Value
encode (Normalized { contactType, contact }) =
    Encode.object
        [ ( "type", Encode.string (ContactType.toString contactType) )
        , ( "externalId", Encode.string contact )
        ]


selectionSet : SelectionSet (Maybe Normalized) Cambiatus.Object.Contact
selectionSet =
    SelectionSet.succeed
        (\maybeType maybeExternalId ->
            case ( maybeType, maybeExternalId ) of
                ( Just type_, Just externalId ) ->
                    Contact type_ externalId |> Normalized |> Just

                _ ->
                    Nothing
        )
        |> with Cambiatus.Object.Contact.type_
        |> with Cambiatus.Object.Contact.externalId



-- UTILITIES


usesPhone : ContactType -> Bool
usesPhone contactType =
    List.member contactType [ ContactType.Whatsapp, ContactType.Phone ]



-- NORMALIZING


countryCode : String -> String
countryCode country =
    case String.toLower country of
        "brasil" ->
            "55"

        "argentina" ->
            "54"

        "costa_rica" ->
            "506"

        _ ->
            ""


normalize : String -> Contact -> Normalized
normalize country { contactType, contact } =
    Normalized
        { contactType = contactType
        , contact =
            case contactType of
                ContactType.Instagram ->
                    "https://instagram.com/" ++ contact

                ContactType.Phone ->
                    countryCode country ++ contact

                ContactType.Telegram ->
                    "https://t.me/" ++ contact

                ContactType.Whatsapp ->
                    countryCode country ++ contact
        }


unWrap : Normalized -> Contact
unWrap (Normalized contact) =
    contact
