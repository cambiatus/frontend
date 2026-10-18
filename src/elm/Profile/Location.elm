module Profile.Location exposing
    ( Locality, localitySelectionSet
    , Country(..), countryCode
    , Place, emptyPlace, placeFromSaved, isPicked, previewLabel, municipalityRow
    , cleanText, BairroTextError(..), validateBairroText
    , Address, emptyAddress, isAddressEmpty, AddressError(..), validateAddress, looksLikeAddress
    , normalizeCep, PostalLookup, postalLookupQuery, applyLookup, foundLine
    , Saved, EditorData, editorSelectionSet
    , SaveFields, noSaveFields, legacySaveFields, saveFields, needsPostalCode
    )

{-| Everything the profile editor needs to know about where a member is and
where their orders are delivered: the types, the selection sets, and the
fields of the save mutation.

Only the editor uses the S5f-api fields (`locationPlace`, `deliveryAddress`,
`releasedFeatures`), so a backend that doesn't have them breaks nothing but the
editor.


# Places from the API

@docs Locality, localitySelectionSet


# The place a member picks

@docs Country, countryCode
@docs Place, emptyPlace, placeFromSaved, isPicked, previewLabel, municipalityRow


# Typed bairros

@docs cleanText, BairroTextError, validateBairroText


# Delivery address

@docs Address, emptyAddress, isAddressEmpty, AddressError, validateAddress, looksLikeAddress


# CEP lookup

@docs normalizeCep, PostalLookup, postalLookupQuery, applyLookup, foundLine


# Loading

@docs Saved, EditorData, editorSelectionSet


# Saving

@docs SaveFields, noSaveFields, legacySaveFields, saveFields, needsPostalCode

-}

import Cambiatus.InputObject
import Cambiatus.Object
import Cambiatus.Object.Community as Community
import Cambiatus.Object.DeliveryAddress as DeliveryAddress
import Cambiatus.Object.Locality as Locality
import Cambiatus.Object.LocationPlace as LocationPlace
import Cambiatus.Object.PostalCodeAddress as PostalCodeAddress
import Cambiatus.Object.User as User
import Cambiatus.Query
import Dict
import Eos
import Eos.Account
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode
import Regex



-- LOCALITIES


{-| A município (or cantón) or a bairro (or distrito), as `localities` and
`locationPlace` return it. `kind` and `country` stay strings, so a value the
backend adds later can't break the decoder.
-}
type alias Locality =
    { id : Int
    , name : String
    , kind : String
    , country : String
    , stateCode : Maybe String
    , code : Maybe String
    }


localitySelectionSet : SelectionSet Locality Cambiatus.Object.Locality
localitySelectionSet =
    SelectionSet.succeed Locality
        |> with Locality.id
        |> with Locality.name
        |> with Locality.kind
        |> with Locality.country
        |> with Locality.stateCode
        |> with Locality.code



-- COUNTRY


type Country
    = Brazil
    | CostaRica
    | Other


countryFromCode : String -> Country
countryFromCode code =
    case code of
        "BR" ->
            Brazil

        "CR" ->
            CostaRica

        _ ->
            Other


{-| The `country` argument of `localities`; `Nothing` for free text
-}
countryCode : Country -> Maybe String
countryCode country =
    case country of
        Brazil ->
            Just "BR"

        CostaRica ->
            Just "CR"

        Other ->
            Nothing



-- PLACE


{-| What the member has picked (or typed) as the public place
-}
type alias Place =
    { country : Country
    , municipality : Maybe Locality
    , bairro : Maybe Locality
    , bairroText : String
    , otherText : String
    }


emptyPlace : Place
emptyPlace =
    { country = Brazil
    , municipality = Nothing
    , bairro = Nothing
    , bairroText = ""
    , otherText = ""
    }


{-| What the API stores for the place behind `location`
-}
type alias Saved =
    { municipality : Locality
    , bairro : Maybe Locality
    , bairroText : Maybe String
    , label : String
    }


{-| The place the editor starts with. A member whose text the backend couldn't
resolve starts in "Outro país" with that text, so nothing they wrote is hidden
-- unless the text is an address, which belongs in the private address.
-}
placeFromSaved : { storedText : String, saved : Maybe Saved } -> Place
placeFromSaved { storedText, saved } =
    case saved of
        Just { municipality, bairro, bairroText } ->
            case countryFromCode municipality.country of
                Other ->
                    -- A world city has no picker here: it shows as text, and
                    -- saving without touching it sends no place at all
                    otherPlace storedText

                country ->
                    { country = country
                    , municipality = Just municipality
                    , bairro = bairro
                    , bairroText = Maybe.withDefault "" bairroText
                    , otherText = ""
                    }

        Nothing ->
            if cleanText storedText == "" || looksLikeAddress storedText then
                emptyPlace

            else
                otherPlace storedText


otherPlace : String -> Place
otherPlace storedText =
    { emptyPlace | country = Other, otherText = cleanText storedText }


{-| Whether the place is a picked município or cantón, the only kind of place a
delivery address can be filed under
-}
isPicked : Place -> Bool
isPicked place =
    place.country /= Other && place.municipality /= Nothing


{-| The line that will show on the member's profile, the same text the backend
composes from the same pick
-}
previewLabel : Place -> Maybe String
previewLabel place =
    case place.country of
        Other ->
            nonEmptyText place.otherText

        _ ->
            place.municipality
                |> Maybe.map
                    (\municipality ->
                        let
                            stateCode =
                                Maybe.withDefault "" municipality.stateCode

                            base =
                                case place.country of
                                    CostaRica ->
                                        municipality.name ++ " - " ++ provinceName stateCode ++ ", Costa Rica"

                                    _ ->
                                        municipality.name ++ " - " ++ stateCode

                            bairroName =
                                case place.bairro of
                                    Just bairro ->
                                        Just bairro.name

                                    Nothing ->
                                        nonEmptyText place.bairroText
                        in
                        case bairroName of
                            Just name ->
                                name ++ ", " ++ base

                            Nothing ->
                                base
                    )


{-| How a município or cantón shows in a list: "{município} – {UF}" for Brazil,
"{cantón} – {provincia}" for Costa Rica
-}
municipalityRow : Country -> Locality -> String
municipalityRow country municipality =
    case ( country, municipality.stateCode ) of
        ( CostaRica, Just code ) ->
            municipality.name ++ " – " ++ provinceName code

        ( _, Just code ) ->
            municipality.name ++ " – " ++ code

        ( _, Nothing ) ->
            municipality.name


{-| Costa Rica's province from the code `localities` sends as `stateCode`
-}
provinceName : String -> String
provinceName code =
    case code of
        "A" ->
            "Alajuela"

        "C" ->
            "Cartago"

        "G" ->
            "Guanacaste"

        "H" ->
            "Heredia"

        "L" ->
            "Limón"

        "P" ->
            "Puntarenas"

        "SJ" ->
            "San José"

        other ->
            other



-- TYPED BAIRROS


{-| The cleaned text, or `Nothing` when nothing is left of it
-}
nonEmptyText : String -> Maybe String
nonEmptyText text =
    case cleanText text of
        "" ->
            Nothing

        cleaned ->
            Just cleaned


{-| Trims and collapses every run of whitespace to one space, as the backend does
-}
cleanText : String -> String
cleanText text =
    text
        |> String.words
        |> String.join " "


type BairroTextError
    = BairroTooLong
    | BairroNotPlain


maxBairroLength : Int
maxBairroLength =
    60


{-| The same checks the backend makes on a typed bairro, so most mistakes show
before the save: at most 60 characters, and not an e-mail, a link, a phone
number or an address. The backend still has the last word.
-}
validateBairroText : String -> Result BairroTextError String
validateBairroText raw =
    let
        cleaned =
            cleanText raw
    in
    if String.length cleaned > maxBairroLength then
        Err BairroTooLong

    else if
        String.contains "@" cleaned
            || Regex.contains urlRegex cleaned
            || Regex.contains cepRegex cleaned
            || Regex.contains phoneRegex cleaned
            || looksLikeAddress cleaned
    then
        Err BairroNotPlain

    else
        Ok cleaned


urlRegex : Regex.Regex
urlRegex =
    Regex.fromStringWith { caseInsensitive = True, multiline = False } "(https?://|www\\.)\\S+"
        |> Maybe.withDefault Regex.never


cepRegex : Regex.Regex
cepRegex =
    Regex.fromString "(^|\\s)\\d{5}-?\\d{3}($|\\s)"
        |> Maybe.withDefault Regex.never


phoneRegex : Regex.Regex
phoneRegex =
    Regex.fromString "(\\d[\\s().-]*){8,}"
        |> Maybe.withDefault Regex.never


streetWordRegex : Regex.Regex
streetWordRegex =
    Regex.fromStringWith { caseInsensitive = True, multiline = False }
        "(^|\\s)(rua|r\\.|avenida|av\\.?|travessa|tv\\.|alameda|estrada|rodovia|praça|praca|calle)(\\s|$)"
        |> Maybe.withDefault Regex.never


{-| Whether a text reads as a street address (a street word, or a CEP) rather
than a place name. Used to offer "Seu texto anterior" above the private address.
-}
looksLikeAddress : String -> Bool
looksLikeAddress text =
    let
        hasDigit =
            String.any Char.isDigit text
    in
    Regex.contains cepRegex text
        || (hasDigit && Regex.contains streetWordRegex text)
        || Regex.contains digitAfterCommaRegex text


digitAfterCommaRegex : Regex.Regex
digitAfterCommaRegex =
    Regex.fromString ",\\s*\\d+"
        |> Maybe.withDefault Regex.never



-- DELIVERY ADDRESS


{-| What the member types in the private address fields. For Costa Rica `street`
holds the otras señas and `number` stays empty.
-}
type alias Address =
    { street : String
    , number : String
    , complement : String
    , postalCode : String
    }


emptyAddress : Address
emptyAddress =
    { street = ""
    , number = ""
    , complement = ""
    , postalCode = ""
    }


isAddressEmpty : Address -> Bool
isAddressEmpty address =
    normalizeAddress address == emptyAddress


normalizeAddress : Address -> Address
normalizeAddress address =
    { street = cleanText address.street
    , number = cleanText address.number
    , complement = cleanText address.complement
    , postalCode = String.filter Char.isAlphaNum address.postalCode
    }


type AddressError
    = StreetRequired
    | NumberRequired
    | PostalCodeInvalid
    | PostalCodeRequired


{-| The address to send (`Nothing` when every field is empty, which means "no
address"), or what is wrong with it. Brazil needs street and number, and a CEP of
8 digits when there is one; Costa Rica needs the otras señas, and a código postal
of 5 digits when there is one.
-}
validateAddress : Country -> Address -> Result (List AddressError) (Maybe Address)
validateAddress country address =
    let
        normalized =
            normalizeAddress address

        expectedLength =
            case country of
                CostaRica ->
                    5

                _ ->
                    8

        postalCodeValid =
            String.all Char.isDigit normalized.postalCode
                && (String.length normalized.postalCode == expectedLength)

        errors =
            List.filterMap identity
                [ errorIf (normalized.street == "") StreetRequired
                , errorIf (country /= CostaRica && normalized.number == "") NumberRequired
                , errorIf (country == Other && normalized.postalCode == "") PostalCodeRequired
                , errorIf (normalized.postalCode /= "" && not postalCodeValid) PostalCodeInvalid
                ]
    in
    if isAddressEmpty address then
        Ok Nothing

    else if List.isEmpty errors then
        Ok (Just (dropUnusedFields country normalized))

    else
        Err errors


errorIf : Bool -> AddressError -> Maybe AddressError
errorIf condition error =
    if condition then
        Just error

    else
        Nothing


{-| A Costa Rica address has no number or complement: the otras señas say it all
-}
dropUnusedFields : Country -> Address -> Address
dropUnusedFields country address =
    case country of
        CostaRica ->
            { address | number = "", complement = "" }

        _ ->
            address



-- CEP LOOKUP


{-| The eight digits of a CEP, or `Nothing` when the text is not a CEP. Dots,
hyphens and spaces are ignored; anything else (letters, a phone number's plus)
means it is not a CEP. Same rule as the backend's `PostalCode.normalize`.
-}
normalizeCep : String -> Maybe String
normalizeCep text =
    let
        digits =
            String.filter Char.isDigit text

        hasStrangeCharacters =
            String.any (\char -> not (Char.isDigit char || List.member char [ '.', '-', ' ' ])) text
    in
    if String.length digits == 8 && not hasStrangeCharacters then
        Just digits

    else
        Nothing


{-| What `postalCodeLookup` found. `bairro` and `municipality` are the gazetteer
rows the backend matched; `bairroText` is the name when there is no row.
-}
type alias PostalLookup =
    { postalCode : String
    , street : Maybe String
    , bairroText : Maybe String
    , bairro : Maybe Locality
    , municipality : Maybe Locality
    , stateCode : Maybe String
    }


postalLookupQuery : String -> SelectionSet (Maybe PostalLookup) RootQuery
postalLookupQuery digits =
    Cambiatus.Query.postalCodeLookup { postalCode = digits }
        (SelectionSet.succeed PostalLookup
            |> with PostalCodeAddress.postalCode
            |> with PostalCodeAddress.street
            |> with PostalCodeAddress.bairroText
            |> with (PostalCodeAddress.bairro localitySelectionSet)
            |> with (PostalCodeAddress.municipality localitySelectionSet)
            |> with PostalCodeAddress.stateCode
        )


{-| Folds a found CEP into the place and the address: the município, the bairro
(the row when there is one, else the name as text) and the street. A CEP that
covers a whole town has no street, so the member's own text stays. The município
is only replaced when the lookup resolved one.
-}
applyLookup : PostalLookup -> { place : Place, address : Address } -> { place : Place, address : Address }
applyLookup lookup { place, address } =
    let
        withMunicipality =
            case lookup.municipality of
                Just municipality ->
                    { place
                        | country = Brazil
                        , municipality = Just municipality
                        , bairro = lookup.bairro
                        , bairroText =
                            case lookup.bairro of
                                Just _ ->
                                    ""

                                Nothing ->
                                    lookup.bairroText |> Maybe.map cleanText |> Maybe.withDefault ""
                    }

                Nothing ->
                    place
    in
    { place = withMunicipality
    , address =
        { address
            | street = lookup.street |> Maybe.map cleanText |> nonEmptyOr address.street
            , postalCode = lookup.postalCode
        }
    }


nonEmptyOr : String -> Maybe String -> String
nonEmptyOr fallback maybeText =
    case maybeText of
        Just text ->
            if text == "" then
                fallback

            else
                text

        Nothing ->
            fallback


{-| "Rua X, Bairro Y, Cidade - UF", from what the fields say now so an edit
shows. Empty parts are left out.
-}
foundLine : { street : String, place : Place, stateCode : Maybe String } -> String
foundLine { street, place, stateCode } =
    let
        bairroName =
            case place.bairro of
                Just bairro ->
                    bairro.name

                Nothing ->
                    cleanText place.bairroText

        cityName =
            place.municipality |> Maybe.map .name |> Maybe.withDefault ""

        line =
            [ cleanText street, bairroName, cityName ]
                |> List.filter (not << String.isEmpty)
                |> String.join ", "
    in
    case stateCode of
        Just code ->
            if line == "" then
                line

            else
                line ++ " - " ++ code

        Nothing ->
            line



-- LOADING


{-| What the editor loads about the member, plus the release keys the member holds
-}
type alias EditorData =
    { storedText : String
    , saved : Maybe Saved
    , address : Maybe Address
    , releasedFeatures : List String
    }


{-| The editor's own query: `user(account:)` with the S5f-api fields, and the
release keys of the community. Nothing else asks for these fields.
-}
editorSelectionSet : { account : Eos.Account.Name, symbol : Eos.Symbol } -> SelectionSet (Maybe EditorData) RootQuery
editorSelectionSet { account, symbol } =
    SelectionSet.map2
        (\maybeUser maybeFeatures ->
            Maybe.map
                (\user ->
                    { storedText = user.storedText
                    , saved = user.saved
                    , address = user.address
                    , releasedFeatures = Maybe.withDefault [] maybeFeatures
                    }
                )
                maybeUser
        )
        (Cambiatus.Query.user { account = Eos.Account.nameToString account } userSelectionSet)
        (Cambiatus.Query.community (\optionals -> { optionals | symbol = Present (Eos.symbolToString symbol) })
            Community.releasedFeatures
        )


userSelectionSet :
    SelectionSet
        { storedText : String
        , saved : Maybe Saved
        , address : Maybe Address
        }
        Cambiatus.Object.User
userSelectionSet =
    SelectionSet.succeed (\location saved address -> { storedText = location, saved = saved, address = address })
        |> with (User.location |> SelectionSet.map (Maybe.withDefault ""))
        |> with (User.locationPlace savedSelectionSet)
        |> with (User.deliveryAddress addressSelectionSet)


savedSelectionSet : SelectionSet Saved Cambiatus.Object.LocationPlace
savedSelectionSet =
    SelectionSet.succeed Saved
        |> with (LocationPlace.municipality localitySelectionSet)
        |> with (LocationPlace.bairro localitySelectionSet)
        |> with LocationPlace.bairroText
        |> with LocationPlace.label


addressSelectionSet : SelectionSet Address Cambiatus.Object.DeliveryAddress
addressSelectionSet =
    SelectionSet.succeed Address
        |> with DeliveryAddress.street
        |> with (DeliveryAddress.number |> SelectionSet.map (Maybe.withDefault ""))
        |> with (DeliveryAddress.complement |> SelectionSet.map (Maybe.withDefault ""))
        |> with (DeliveryAddress.postalCode |> SelectionSet.map (Maybe.withDefault ""))



-- SAVING


{-| The part of `UserUpdateInput` that is about the place and the address
-}
type alias SaveFields =
    { location : OptionalArgument String
    , locationMunicipalityId : OptionalArgument Int
    , locationBairroId : OptionalArgument Int
    , locationBairroText : OptionalArgument String
    , deliveryAddress : OptionalArgument Cambiatus.InputObject.DeliveryAddressInput
    }


{-| Leaves the place and the address as they are
-}
noSaveFields : SaveFields
noSaveFields =
    { location = Absent
    , locationMunicipalityId = Absent
    , locationBairroId = Absent
    , locationBairroText = Absent
    , deliveryAddress = Absent
    }


{-| The legacy path: the text field, sent as `location`
-}
legacySaveFields : String -> SaveFields
legacySaveFields location =
    { noSaveFields | location = Present location }


{-| What to send given what the editor loaded (`original`) and what the member
has now (`current`), so a save changes only what the member changed:

  - The picker fields only when the place changed, or when the address did (the
    backend needs the picked place to file an address under).
  - `deliveryAddress` only when a private field changed; an explicit null when
    they were all cleared.
  - The legacy `location` only for "Outro país" text or a cleared place.

An address is sent while the place is a picked município or cantón, or a place
that is only text ("Outro país"): then it is a Brazilian address filed under its
own CEP, and no picker field goes with it.

-}
saveFields :
    { original : { place : Place, address : Address }
    , current : { place : Place, address : Address }
    }
    -> SaveFields
saveFields { original, current } =
    let
        currentKey =
            placeKey current.place

        placeChanged =
            placeKey original.place /= currentKey

        addressChanged =
            normalizeAddress original.address /= normalizeAddress current.address

        legacyLocation text =
            if placeChanged then
                { noSaveFields | location = Present text }

            else
                noSaveFields
    in
    case currentKey of
        NoPlace ->
            legacyLocation ""

        OtherPlace text ->
            let
                withAddress fields =
                    if addressChanged then
                        { fields | deliveryAddress = addressField current }

                    else
                        fields
            in
            withAddress (legacyLocation text)

        PickedPlace picked ->
            if placeChanged || addressChanged then
                { location = Absent
                , locationMunicipalityId = Present picked.municipalityId
                , locationBairroId = OptionalArgument.fromMaybe picked.bairroId
                , locationBairroText = optionalText picked.bairroText
                , deliveryAddress =
                    if addressChanged then
                        addressField current

                    else
                        Absent
                }

            else
                noSaveFields


addressField : { place : Place, address : Address } -> OptionalArgument Cambiatus.InputObject.DeliveryAddressInput
addressField { place, address } =
    if isAddressEmpty address then
        Null

    else
        Present (toInput place.country address)


toInput : Country -> Address -> Cambiatus.InputObject.DeliveryAddressInput
toInput country address =
    let
        toSend =
            dropUnusedFields country (normalizeAddress address)
    in
    { street = toSend.street
    , number = optionalText toSend.number
    , complement = optionalText toSend.complement
    , postalCode = optionalText toSend.postalCode
    }


optionalText : String -> OptionalArgument String
optionalText value =
    if value == "" then
        Absent

    else
        Present value


{-| What identifies a place, ignoring everything that doesn't change what is saved
-}
type PlaceKey
    = NoPlace
    | OtherPlace String
    | PickedPlace { municipalityId : Int, bairroId : Maybe Int, bairroText : String }


placeKey : Place -> PlaceKey
placeKey place =
    case ( place.country, place.municipality ) of
        ( Other, _ ) ->
            nonEmptyText place.otherText
                |> Maybe.map OtherPlace
                |> Maybe.withDefault NoPlace

        ( _, Nothing ) ->
            NoPlace

        ( _, Just municipality ) ->
            PickedPlace
                { municipalityId = municipality.id
                , bairroId = Maybe.map .id place.bairro
                , bairroText =
                    if place.bairro == Nothing then
                        cleanText place.bairroText

                    else
                        ""
                }


{-| Whether the backend refused the save because the address has no CEP to be
filed under (`details: {delivery_address: ["needs_postal_code"]}`)
-}
needsPostalCode : Graphql.Http.Error a -> Bool
needsPostalCode error =
    case error of
        Graphql.Http.GraphqlError _ errors ->
            List.any
                (\graphqlError ->
                    Dict.get "details" graphqlError.details
                        |> Maybe.andThen
                            (Json.Decode.decodeValue
                                (Json.Decode.at [ "delivery_address" ] (Json.Decode.list Json.Decode.string))
                                >> Result.toMaybe
                            )
                        |> Maybe.withDefault []
                        |> List.member "needs_postal_code"
                )
                errors

        _ ->
            False
