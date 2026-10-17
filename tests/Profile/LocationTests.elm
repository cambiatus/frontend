module Profile.LocationTests exposing (all)

import Avatar
import Cambiatus.Mutation
import Cambiatus.Object.Session
import Eos.Account
import Expect
import Graphql.Document
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Markdown
import Profile
import Profile.Location as Location exposing (Address, Country(..), Locality, Place)
import Test exposing (Test, describe, test)
import Transfer


all : Test
all =
    describe "Profile.Location"
        [ previewLabel
        , typedBairro
        , postalCodes
        , formOutput
        , saveFieldsTests
        , legacyDocuments
        , startingPlace
        ]



-- FIXTURES


rio : Locality
rio =
    { id = 3304557, name = "Rio de Janeiro", kind = "municipality", country = "BR", stateCode = Just "RJ", code = Just "3304557" }


santaTeresa : Locality
santaTeresa =
    { id = 14, name = "Santa Teresa", kind = "bairro", country = "BR", stateCode = Just "RJ", code = Nothing }


teresopolis : Locality
teresopolis =
    { id = 3305802, name = "Teresópolis", kind = "municipality", country = "BR", stateCode = Just "RJ", code = Just "3305802" }


monteverde : Locality
monteverde =
    { id = 612, name = "Monteverde", kind = "municipality", country = "CR", stateCode = Just "P", code = Just "612" }


santaElena : Locality
santaElena =
    { id = 61201, name = "Santa Elena", kind = "bairro", country = "CR", stateCode = Just "P", code = Nothing }


brazilPlace : Place
brazilPlace =
    { country = Brazil, municipality = Just rio, bairro = Just santaTeresa, bairroText = "", otherText = "" }


address : Address
address =
    { street = "Rua das Flores", number = "10", complement = "", postalCode = "" }


unchangedFields : Location.SaveFields
unchangedFields =
    Location.noSaveFields



-- AT-f37


previewLabel : Test
previewLabel =
    describe "the preview label"
        [ test "AT-f37 a bairro shows as bairro, município - UF" <|
            \_ ->
                Location.previewLabel brazilPlace
                    |> Expect.equal (Just "Santa Teresa, Rio de Janeiro - RJ")
        , test "AT-f37 a município alone shows as município - UF" <|
            \_ ->
                Location.previewLabel { brazilPlace | municipality = Just teresopolis, bairro = Nothing }
                    |> Expect.equal (Just "Teresópolis - RJ")
        , test "AT-f37 a typed bairro shows as typed, with the spaces collapsed" <|
            \_ ->
                Location.previewLabel { brazilPlace | bairro = Nothing, bairroText = "  Vila   Nova " }
                    |> Expect.equal (Just "Vila Nova, Rio de Janeiro - RJ")
        , test "AT-f37 Costa Rica shows the province and the country" <|
            \_ ->
                Location.previewLabel
                    { country = CostaRica, municipality = Just monteverde, bairro = Just santaElena, bairroText = "", otherText = "" }
                    |> Expect.equal (Just "Santa Elena, Monteverde - Puntarenas, Costa Rica")
        , test "AT-f37 a cantón alone shows the province and the country" <|
            \_ ->
                Location.previewLabel
                    { country = CostaRica, municipality = Just monteverde, bairro = Nothing, bairroText = "", otherText = "" }
                    |> Expect.equal (Just "Monteverde - Puntarenas, Costa Rica")
        , test "AT-f37 other country shows the text; nothing picked shows nothing" <|
            \_ ->
                Expect.equal
                    ( Location.previewLabel { brazilPlace | country = Other, otherText = " Lisboa,  Portugal " }
                    , Location.previewLabel Location.emptyPlace
                    )
                    ( Just "Lisboa, Portugal", Nothing )
        ]


typedBairro : Test
typedBairro =
    describe "the typed-bairro shape check"
        [ test "AT-f37 a plain name is accepted and cleaned" <|
            \_ ->
                Location.validateBairroText "  Vila   Nova "
                    |> Expect.equal (Ok "Vila Nova")
        , test "AT-f37 more than 60 characters is refused" <|
            \_ ->
                Location.validateBairroText (String.repeat 61 "a")
                    |> Expect.equal (Err Location.BairroTooLong)
        , test "AT-f37 an e-mail, a link, a phone and an address are refused" <|
            \_ ->
                [ "ana@example.com"
                , "https://example.com/x"
                , "www.example.com"
                , "(21) 99999-9999"
                , "Rua das Flores 10"
                , "20000-000"
                ]
                    |> List.map Location.validateBairroText
                    |> List.all ((==) (Err Location.BairroNotPlain))
                    |> Expect.equal True
        ]


postalCodes : Test
postalCodes =
    describe "CEP and código postal validation"
        [ test "AT-f37 a CEP of 8 digits is accepted, with or without the dash" <|
            \_ ->
                Expect.equal
                    ( Location.validateAddress Brazil { address | postalCode = "20000-000" }
                    , Location.validateAddress Brazil { address | postalCode = "20000000" }
                    )
                    ( Ok (Just { address | postalCode = "20000000" })
                    , Ok (Just { address | postalCode = "20000000" })
                    )
        , test "AT-f37 a CEP that is not 8 digits is refused" <|
            \_ ->
                Expect.equal
                    ( Location.validateAddress Brazil { address | postalCode = "2000-00" }
                    , Location.validateAddress Brazil { address | postalCode = "2000000A" }
                    )
                    ( Err [ Location.PostalCodeInvalid ], Err [ Location.PostalCodeInvalid ] )
        , test "AT-f37 a código postal of 5 digits is accepted, anything else is refused" <|
            \_ ->
                let
                    cr code =
                        Location.validateAddress CostaRica { street = "200 m norte", number = "", complement = "", postalCode = code }
                in
                Expect.equal
                    ( cr "61201", cr "6120", cr "612012" )
                    ( Ok (Just { street = "200 m norte", number = "", complement = "", postalCode = "61201" })
                    , Err [ Location.PostalCodeInvalid ]
                    , Err [ Location.PostalCodeInvalid ]
                    )
        ]


formOutput : Test
formOutput =
    describe "what each section of the form parses to"
        [ test "AT-f37 an empty address means no address" <|
            \_ ->
                Location.validateAddress Brazil Location.emptyAddress
                    |> Expect.equal (Ok Nothing)
        , test "AT-f37 Brazil needs street and number" <|
            \_ ->
                Location.validateAddress Brazil { address | street = " ", number = "", complement = "x" }
                    |> Expect.equal (Err [ Location.StreetRequired, Location.NumberRequired ])
        , test "AT-f37 Costa Rica needs the otras señas and ignores number and complement" <|
            \_ ->
                Expect.equal
                    ( Location.validateAddress CostaRica { street = "", number = "", complement = "", postalCode = "" }
                    , Location.validateAddress CostaRica { street = "Casa azul", number = "3", complement = "x", postalCode = "" }
                    )
                    ( Ok Nothing
                    , Ok (Just { street = "Casa azul", number = "", complement = "", postalCode = "" })
                    )
        , test "AT-f37 a Costa Rica address with a code but no señas is refused" <|
            \_ ->
                Location.validateAddress CostaRica { street = "", number = "", complement = "", postalCode = "61201" }
                    |> Expect.equal (Err [ Location.StreetRequired ])
        , test "AT-f37 the place section parses to the saved pick" <|
            \_ ->
                Location.saveFields
                    { original = { place = Location.emptyPlace, address = Location.emptyAddress }
                    , current = { place = brazilPlace, address = Location.emptyAddress }
                    }
                    |> (\fields -> ( fields.locationMunicipalityId, fields.locationBairroId, fields.locationBairroText ))
                    |> Expect.equal ( Present 3304557, Present 14, Absent )
        ]



-- AT-f38


saveFieldsTests : Test
saveFieldsTests =
    let
        original =
            { place = brazilPlace, address = address }
    in
    describe "the save input"
        [ test "AT-f38 nothing changed sends nothing" <|
            \_ ->
                Location.saveFields { original = original, current = original }
                    |> Expect.equal unchangedFields
        , test "AT-f38 a changed private field alone never sends the legacy location" <|
            \_ ->
                Location.saveFields
                    { original = original
                    , current = { original | address = { address | number = "11" } }
                    }
                    |> .location
                    |> Expect.equal Absent
        , test "AT-f38 a changed place sends the picker fields and no legacy location" <|
            \_ ->
                Location.saveFields
                    { original = original
                    , current = { original | place = { brazilPlace | municipality = Just teresopolis, bairro = Nothing } }
                    }
                    |> (\fields ->
                            { location = fields.location
                            , municipalityId = fields.locationMunicipalityId
                            , bairroId = fields.locationBairroId
                            , bairroText = fields.locationBairroText
                            }
                       )
                    |> Expect.equal
                        { location = Absent
                        , municipalityId = Present 3305802
                        , bairroId = Absent
                        , bairroText = Absent
                        }
        , test "AT-f38 a typed bairro is sent as text, trimmed and collapsed" <|
            \_ ->
                Location.saveFields
                    { original = original
                    , current = { original | place = { brazilPlace | bairro = Nothing, bairroText = " Vila   Nova " } }
                    }
                    |> (\fields -> ( fields.locationBairroId, fields.locationBairroText ))
                    |> Expect.equal ( Absent, Present "Vila Nova" )
        , test "AT-f38 a changed private field sends deliveryAddress, with the picker fields it needs" <|
            \_ ->
                let
                    fields =
                        Location.saveFields
                            { original = original
                            , current = { original | address = { address | complement = "apto 2" } }
                            }
                in
                Expect.equal
                    { location = fields.location
                    , municipalityId = fields.locationMunicipalityId
                    , bairroId = fields.locationBairroId
                    , deliveryAddress = fields.deliveryAddress
                    }
                    { location = Absent
                    , municipalityId = Present 3304557
                    , bairroId = Present 14
                    , deliveryAddress =
                        Present
                            { street = "Rua das Flores"
                            , number = Present "10"
                            , complement = Present "apto 2"
                            , postalCode = Absent
                            }
                    }
        , test "AT-f38 clearing every private field sends an explicit null" <|
            \_ ->
                Location.saveFields
                    { original = original
                    , current = { original | address = Location.emptyAddress }
                    }
                    |> .deliveryAddress
                    |> Expect.equal Null
        , test "AT-f38 Costa Rica sends the señas as the street and no number" <|
            \_ ->
                let
                    place =
                        { country = CostaRica, municipality = Just monteverde, bairro = Nothing, bairroText = "", otherText = "" }
                in
                Location.saveFields
                    { original = { place = place, address = Location.emptyAddress }
                    , current =
                        { place = place
                        , address = { street = "200 m norte de la iglesia", number = "9", complement = "x", postalCode = "61201" }
                        }
                    }
                    |> .deliveryAddress
                    |> Expect.equal
                        (Present
                            { street = "200 m norte de la iglesia"
                            , number = Absent
                            , complement = Absent
                            , postalCode = Present "61201"
                            }
                        )
        , test "AT-f38 Other country text is sent as the legacy location, and nothing else" <|
            \_ ->
                let
                    other =
                        { country = Other, municipality = Nothing, bairro = Nothing, bairroText = "", otherText = " Lisboa,  Portugal " }
                in
                Location.saveFields
                    { original = original, current = { original | place = other } }
                    |> (\fields ->
                            ( fields.location, fields.locationMunicipalityId, fields.deliveryAddress )
                       )
                    |> Expect.equal ( Present "Lisboa, Portugal", Absent, Absent )
        , test "AT-f38 a cleared place is sent as an empty legacy location" <|
            \_ ->
                Location.saveFields
                    { original = original, current = { original | place = Location.emptyPlace } }
                    |> (\fields -> ( fields.location, fields.locationMunicipalityId ))
                    |> Expect.equal ( Present "", Absent )
        , test "AT-f38 the legacy path sends only location" <|
            \_ ->
                Location.legacySaveFields "Rio"
                    |> Expect.equal { unchangedFields | location = Present "Rio" }
        ]



-- AT-f39


legacyDocuments : Test
legacyDocuments =
    let
        apiFields =
            [ "locationPlace", "deliveryAddress", "locationMunicipalityId", "locationBairroId", "locationBairroText", "releasedFeatures", "localities" ]

        expectNone document =
            apiFields
                |> List.filter (\field -> String.contains field document)
                |> Expect.equalLists []

        account =
            Eos.Account.stringToName "alice"

        profileForm =
            Profile.profileToForm
                { name = Just "Alice"
                , account = account
                , avatar = Avatar.empty
                , email = Just "alice@example.com"
                , bio = Just Markdown.empty
                , localization = Just "Rio"
                , contacts = []
                , interests = []
                , communities = []
                , roles = []
                , analysisCount = 0
                , kyc = Nothing
                , address = Nothing
                , claimNotification = False
                , digest = False
                , transferNotification = False
                , preferredLanguage = Nothing
                , latestAcceptedTerms = Nothing
                }
    in
    describe "the documents of everything but the editor"
        [ test "AT-f39 Profile.query has no S5f-api field" <|
            \_ -> expectNone (Graphql.Document.serializeQuery (Profile.query account))
        , test "AT-f39 Profile.mutation has no S5f-api field" <|
            \_ -> expectNone (Graphql.Document.serializeMutation (Profile.mutation profileForm))
        , test "AT-f39 the signIn document has no S5f-api field" <|
            \_ ->
                Cambiatus.Mutation.signIn identity
                    { account = "alice", password = "x" }
                    (Cambiatus.Object.Session.user Profile.selectionSet)
                    |> Graphql.Document.serializeMutation
                    |> expectNone
        , test "AT-f39 the transfer documents have no S5f-api field" <|
            \_ ->
                [ Graphql.Document.serializeQuery (Transfer.transferQuery 1)
                , Graphql.Document.serializeQuery (Transfer.transfersUserQuery account identity)
                ]
                    |> String.join "\n"
                    |> expectNone
        ]



-- THE STARTING PLACE


startingPlace : Test
startingPlace =
    describe "the place the editor starts with"
        [ test "a resolved place fills the picker" <|
            \_ ->
                Location.placeFromSaved
                    { storedText = "Santa Teresa, Rio de Janeiro - RJ"
                    , saved = Just { municipality = rio, bairro = Just santaTeresa, bairroText = Nothing, label = "Santa Teresa, Rio de Janeiro - RJ" }
                    }
                    |> Expect.equal brazilPlace
        , test "text the backend couldn't resolve starts as other country, so nothing is hidden" <|
            \_ ->
                Location.placeFromSaved { storedText = "Lisboa, Portugal", saved = Nothing }
                    |> Expect.equal { country = Other, municipality = Nothing, bairro = Nothing, bairroText = "", otherText = "Lisboa, Portugal" }
        , test "an address-shaped text is not a place" <|
            \_ ->
                Location.placeFromSaved { storedText = "Rua das Flores 10, apto 2", saved = Nothing }
                    |> Expect.equal Location.emptyPlace
        , test "address-shaped text is recognised, a place name is not" <|
            \_ ->
                [ "Rua das Flores 10", "Av. Brasil, 1500", "20000-000", "Teresópolis - RJ" ]
                    |> List.map Location.looksLikeAddress
                    |> Expect.equalLists [ True, True, True, False ]
        ]
