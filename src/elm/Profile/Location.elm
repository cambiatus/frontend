module Profile.Location exposing
    ( Locality, localitySelectionSet
    , Country(..), countryCode
    , Place, emptyPlace, placeFromSaved, previewLabel, provinceName
    , cleanText, BairroTextError(..), validateBairroText
    , Address, emptyAddress, isAddressEmpty, AddressError(..), validateAddress, looksLikeAddress
    , Saved, EditorData, editorSelectionSet
    , SaveFields, noSaveFields, legacySaveFields, saveFields
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
@docs Place, emptyPlace, placeFromSaved, previewLabel, provinceName


# Typed bairros

@docs cleanText, BairroTextError, validateBairroText


# Delivery address

@docs Address, emptyAddress, isAddressEmpty, AddressError, validateAddress, looksLikeAddress


# Loading

@docs Saved, EditorData, editorSelectionSet


# Saving

@docs SaveFields, noSaveFields, legacySaveFields, saveFields

-}

import Cambiatus.InputObject
import Cambiatus.Object
import Cambiatus.Object.Community as Community
import Cambiatus.Object.DeliveryAddress as DeliveryAddress
import Cambiatus.Object.Locality as Locality
import Cambiatus.Object.LocationPlace as LocationPlace
import Cambiatus.Object.User as User
import Cambiatus.Query
import Eos
import Eos.Account
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
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
        "CR" ->
            CostaRica

        _ ->
            Brazil


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
            { country = countryFromCode municipality.country
            , municipality = Just municipality
            , bairro = bairro
            , bairroText = Maybe.withDefault "" bairroText
            , otherText = ""
            }

        Nothing ->
            if cleanText storedText == "" || looksLikeAddress storedText then
                emptyPlace

            else
                { emptyPlace | country = Other, otherText = cleanText storedText }


{-| The line that will show on the member's profile, the same text the backend
composes from the same pick
-}
previewLabel : Place -> Maybe String
previewLabel place =
    case place.country of
        Other ->
            Just (cleanText place.otherText)
                |> keepNonEmpty

        _ ->
            place.municipality
                |> Maybe.map
                    (\municipality ->
                        let
                            base =
                                case place.country of
                                    CostaRica ->
                                        municipality.name
                                            ++ " - "
                                            ++ (municipality.stateCode
                                                    |> Maybe.map provinceName
                                                    |> Maybe.withDefault ""
                                               )
                                            ++ ", Costa Rica"

                                    _ ->
                                        municipality.name
                                            ++ " - "
                                            ++ Maybe.withDefault "" municipality.stateCode

                            bairroName =
                                case place.bairro of
                                    Just bairro ->
                                        Just bairro.name

                                    Nothing ->
                                        Just (cleanText place.bairroText)
                                            |> keepNonEmpty
                        in
                        case bairroName of
                            Just name ->
                                name ++ ", " ++ base

                            Nothing ->
                                base
                    )


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


keepNonEmpty : Maybe String -> Maybe String
keepNonEmpty maybeText =
    case maybeText of
        Just "" ->
            Nothing

        other ->
            other


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

        digitsOnly =
            String.all Char.isDigit normalized.postalCode

        expectedLength =
            case country of
                CostaRica ->
                    5

                _ ->
                    8

        errors =
            List.filterMap identity
                [ if normalized.street == "" then
                    Just StreetRequired

                  else
                    Nothing
                , if country /= CostaRica && normalized.number == "" then
                    Just NumberRequired

                  else
                    Nothing
                , if normalized.postalCode /= "" && not (digitsOnly && String.length normalized.postalCode == expectedLength) then
                    Just PostalCodeInvalid

                  else
                    Nothing
                ]
    in
    if isAddressEmpty address then
        Ok Nothing

    else if List.isEmpty errors then
        Ok
            (Just
                (if country == CostaRica then
                    { normalized | number = "", complement = "" }

                 else
                    normalized
                )
            )

    else
        Err errors



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

An address is sent only while the place is a picked município or cantón.

-}
saveFields :
    { original : { place : Place, address : Address }
    , current : { place : Place, address : Address }
    }
    -> SaveFields
saveFields { original, current } =
    let
        placeChanged =
            placeKey original.place /= placeKey current.place

        isPicked =
            (placeKey current.place).kind == PickedPlace

        addressChanged =
            isPicked && normalizeAddress original.address /= normalizeAddress current.address

        sendPickerFields =
            isPicked && (placeChanged || addressChanged)

        locationText =
            if placeChanged then
                case (placeKey current.place).kind of
                    OtherPlace ->
                        Present (placeKey current.place).text

                    EmptyPlace ->
                        Present ""

                    PickedPlace ->
                        Absent

            else
                Absent
    in
    { location = locationText
    , locationMunicipalityId =
        if sendPickerFields then
            current.place.municipality
                |> Maybe.map (.id >> Present)
                |> Maybe.withDefault Absent

        else
            Absent
    , locationBairroId =
        if sendPickerFields then
            current.place.bairro
                |> Maybe.map (.id >> Present)
                |> Maybe.withDefault Absent

        else
            Absent
    , locationBairroText =
        if sendPickerFields && current.place.bairro == Nothing && cleanText current.place.bairroText /= "" then
            Present (cleanText current.place.bairroText)

        else
            Absent
    , deliveryAddress =
        if addressChanged then
            if isAddressEmpty current.address then
                Null

            else
                Present (toInput current.place.country current.address)

        else
            Absent
    }


toInput : Country -> Address -> Cambiatus.InputObject.DeliveryAddressInput
toInput country address =
    let
        normalized =
            normalizeAddress address

        optional value =
            if value == "" then
                Absent

            else
                Present value
    in
    { street = normalized.street
    , number =
        if country == CostaRica then
            Absent

        else
            optional normalized.number
    , complement =
        if country == CostaRica then
            Absent

        else
            optional normalized.complement
    , postalCode = optional normalized.postalCode
    }


type PlaceKind
    = EmptyPlace
    | OtherPlace
    | PickedPlace


{-| What identifies a place, ignoring everything that doesn't change what is saved
-}
type alias PlaceKey =
    { kind : PlaceKind
    , text : String
    , ids : List Int
    }


placeKey : Place -> PlaceKey
placeKey place =
    case place.country of
        Other ->
            case cleanText place.otherText of
                "" ->
                    { kind = EmptyPlace, text = "", ids = [] }

                text ->
                    { kind = OtherPlace, text = text, ids = [] }

        _ ->
            case place.municipality of
                Nothing ->
                    { kind = EmptyPlace, text = "", ids = [] }

                Just municipality ->
                    { kind = PickedPlace
                    , text =
                        if place.bairro == Nothing then
                            cleanText place.bairroText

                        else
                            ""
                    , ids = municipality.id :: (place.bairro |> Maybe.map (.id >> List.singleton) |> Maybe.withDefault [])
                    }
