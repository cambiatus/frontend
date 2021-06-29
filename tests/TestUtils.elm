module TestUtils exposing
    ( actionFuzzer
    , appendGenerators
    , avatarFuzzer
    , cambiatusUrlFuzzer
    , claimFuzzer
    , createCommunityDataInputFuzzer
    , createTokenDataFuzzer
    , digitGenerator
    , expiryOptsDataFuzzer
    , nameFuzzer
    , nonZeroDigitGenerator
    , randomListWithRandomLength
    , timeFuzzer
    , updateTokenDataFuzzer
    )

import Action
import Avatar
import Cambiatus.Enum.VerificationType as VerificationType
import Cambiatus.Scalar
import Claim
import Community
import Eos
import Eos.Account as Eos
import Fuzz
import Iso8601
import Profile
import Random
import Random.Char
import Random.Extra
import Random.String
import Shrink
import Time
import Token
import Url


capFloatWithSymbol : Eos.Symbol -> Float -> Float
capFloatWithSymbol symbol value =
    let
        maxLength =
            String.fromFloat value
                |> String.split "."
                |> (\parts ->
                        case List.head parts of
                            Nothing ->
                                String.fromFloat value

                            Just firstPart ->
                                firstPart
                   )
                |> String.length
                -- Add separator
                |> (+) 1
                |> (+) (Eos.getSymbolPrecision symbol)
    in
    String.fromFloat value
        |> String.left maxLength
        |> String.toFloat
        |> Maybe.withDefault value



-- RANDOM GENERATORS


withRandom : Random.Generator a -> Random.Generator (a -> c) -> Random.Generator c
withRandom =
    Random.map2 (|>)


nonZeroDigitGenerator : Random.Generator String
nonZeroDigitGenerator =
    Random.int 1 9
        |> Random.map String.fromInt


digitGenerator : Random.Generator String
digitGenerator =
    Random.int 0 9
        |> Random.map String.fromInt


maybeGenerate : Random.Generator a -> Random.Generator (Maybe a)
maybeGenerate generator =
    Random.Extra.maybe Random.Extra.bool generator


randomListWithRandomLength : Int -> Int -> Random.Generator a -> Random.Generator (List a)
randomListWithRandomLength minLength maxLength generator =
    Random.int minLength maxLength
        |> Random.andThen (\length -> Random.list length generator)


appendGenerators : Random.Generator String -> Random.Generator String -> Random.Generator String
appendGenerators secondGenerator firstGenerator =
    firstGenerator
        |> Random.andThen (\first -> Random.map (\second -> first ++ second) secondGenerator)


stringGenerator : Random.Generator String
stringGenerator =
    Random.String.string 15 Random.Char.english


timeGenerator : Random.Generator Time.Posix
timeGenerator =
    Random.int 0 Random.maxInt
        |> Random.map Time.millisToPosix


dateTimeGenerator : Random.Generator Cambiatus.Scalar.DateTime
dateTimeGenerator =
    timeGenerator
        |> Random.map (Iso8601.fromTime >> Cambiatus.Scalar.DateTime)


stringWithRandomLengthGenerator : Int -> Int -> Random.Generator String
stringWithRandomLengthGenerator minLength maxLength =
    Random.Char.english
        |> randomListWithRandomLength minLength maxLength
        |> Random.map String.fromList


urlGenerator : List String -> Random.Generator Url.Url
urlGenerator allowedAuthorities =
    let
        protocolGenerator =
            Random.Extra.choice Url.Http Url.Https

        host =
            case allowedAuthorities of
                [] ->
                    stringGenerator
                        |> Random.map (\randomString -> randomString ++ ".")
                        |> appendGenerators stringGenerator

                firstAuthority :: rest ->
                    stringGenerator
                        |> appendGenerators
                            (Random.uniform firstAuthority rest)

        pathGenerator =
            randomListWithRandomLength 0 5 stringGenerator
                |> Random.map (String.join "/")
                |> Random.map (\path -> "/" ++ path)

        queryGenerator =
            stringGenerator
                |> Random.map (\queryKey -> queryKey ++ "=")
                |> appendGenerators stringGenerator
                |> randomListWithRandomLength 0 5
                |> Random.map (String.join "&")
                |> Random.map
                    (\queryString ->
                        if String.isEmpty queryString then
                            Nothing

                        else
                            Just queryString
                    )

        fragmentGenerator =
            stringGenerator
                |> Random.map (\fragmentString -> fragmentString |> Just)
                |> List.singleton
                |> Random.uniform (Random.constant Nothing)
                |> Random.andThen identity
    in
    Random.constant Url.Url
        |> withRandom protocolGenerator
        |> withRandom host
        |> withRandom (Random.constant Nothing)
        |> withRandom pathGenerator
        |> withRandom queryGenerator
        |> withRandom fragmentGenerator
        |> Random.map
            (\url ->
                if String.endsWith "localhost" url.host then
                    { url | port_ = Just 3000 }

                else
                    { url | port_ = Nothing }
            )


cambiatusUrlGenerator : Maybe String -> Random.Generator Url.Url
cambiatusUrlGenerator maybeAuthority =
    let
        allowedAuthorities =
            case maybeAuthority of
                Nothing ->
                    [ ".cambiatus.io"
                    , ".demo.cambiatus.io"
                    , ".staging.cambiatus.io"
                    , ".localhost"
                    , ".staging.localhost"
                    ]

                Just authority ->
                    [ authority ]
    in
    urlGenerator allowedAuthorities


symbolGenerator : Random.Generator Eos.Symbol
symbolGenerator =
    digitGenerator
        |> appendGenerators (Random.constant ",")
        |> appendGenerators (stringWithRandomLengthGenerator Eos.minSymbolLength Eos.maxSymbolLength)
        |> Random.map (Eos.symbolFromString >> Maybe.withDefault Eos.cambiatusSymbol)


nameGenerator : Random.Generator Eos.Name
nameGenerator =
    stringWithRandomLengthGenerator 12 12
        |> Random.map Eos.stringToName


avatarGenerator : Random.Generator Avatar.Avatar
avatarGenerator =
    Random.Extra.choices (Random.constant Avatar.empty)
        [ stringGenerator |> Random.map Avatar.fromString ]


emailGenerator : Random.Generator String
emailGenerator =
    stringGenerator
        |> appendGenerators (Random.constant "@")
        |> appendGenerators stringGenerator
        |> appendGenerators (Random.constant ".com")


minimalProfileGenerator : Random.Generator Profile.Minimal
minimalProfileGenerator =
    Random.constant Profile.Minimal
        |> withRandom (maybeGenerate stringGenerator)
        |> withRandom nameGenerator
        |> withRandom avatarGenerator
        |> withRandom (maybeGenerate emailGenerator)
        |> withRandom (maybeGenerate stringGenerator)
        -- TODO
        |> withRandom (Random.constant [])


actionObjectiveGenerator : Random.Generator Action.Objective
actionObjectiveGenerator =
    Random.constant Action.Objective
        |> withRandom (Random.int 0 Random.maxInt)
        |> withRandom stringGenerator
        |> withRandom (symbolGenerator |> Random.map (\symbol -> { symbol = symbol }))
        |> withRandom Random.Extra.bool


actionGenerator : Random.Generator Action.Action
actionGenerator =
    Random.constant Action.Action
        |> withRandom (Random.int 0 Random.maxInt)
        |> withRandom stringGenerator
        |> withRandom actionObjectiveGenerator
        |> withRandom (Random.float 0 1000)
        |> withRandom (Random.float 0 1000)
        |> withRandom nameGenerator
        |> withRandom (randomListWithRandomLength 3 20 minimalProfileGenerator)
        |> withRandom (Random.int 0 Random.maxInt)
        |> withRandom (Random.int 0 Random.maxInt)
        |> withRandom (maybeGenerate dateTimeGenerator)
        |> withRandom (Random.Extra.choice VerificationType.Automatic VerificationType.Claimable)
        |> withRandom (Random.uniform 3 [ 5, 7, 9 ])
        |> withRandom Random.Extra.bool
        |> withRandom Random.Extra.bool
        |> withRandom Random.Extra.bool
        |> withRandom (maybeGenerate stringGenerator)
        |> withRandom (maybeGenerate (Random.int 0 Random.maxInt))


claimGenerator : Random.Generator Claim.Model
claimGenerator =
    let
        checkGenerator : Random.Generator Claim.Check
        checkGenerator =
            Random.constant Claim.Check
                |> withRandom Random.Extra.bool
                |> withRandom minimalProfileGenerator
    in
    Random.constant Claim.Model
        |> withRandom (Random.int 0 Random.maxInt)
        |> withRandom (Random.uniform Claim.Approved [ Claim.Rejected, Claim.Pending ])
        |> withRandom minimalProfileGenerator
        |> withRandom actionGenerator
        |> withRandom (randomListWithRandomLength 3 20 checkGenerator)
        |> withRandom dateTimeGenerator
        |> withRandom (maybeGenerate stringGenerator)
        |> withRandom (maybeGenerate stringGenerator)
        |> Random.map
            (\claim ->
                { claim
                    | checks =
                        List.map2 (\check validator -> { check | validator = validator })
                            claim.checks
                            claim.action.validators
                }
            )


createCommunityDataInputGenerator : Random.Generator Community.CreateCommunityDataInput
createCommunityDataInputGenerator =
    Random.constant Community.CreateCommunityDataInput
        |> withRandom nameGenerator
        |> withRandom symbolGenerator
        |> withRandom (urlGenerator [] |> Random.map Url.toString)
        |> withRandom stringGenerator
        |> withRandom stringGenerator
        |> withRandom (cambiatusUrlGenerator Nothing |> Random.map Url.toString)
        |> withRandom (Random.float 0 1000)
        |> withRandom (Random.float 0 1000)
        |> withRandom Random.Extra.bool
        |> withRandom Random.Extra.bool
        |> withRandom Random.Extra.bool
        |> withRandom Random.Extra.bool
        |> withRandom (urlGenerator [] |> Random.map Url.toString)
        |> Random.map
            (\input ->
                { input
                    | invitedReward = capFloatWithSymbol input.symbol input.invitedReward
                    , inviterReward = capFloatWithSymbol input.symbol input.inviterReward
                }
            )


updateTokenDataGenerator : Random.Generator Token.UpdateTokenData
updateTokenDataGenerator =
    Random.constant
        (\maxSupply minBalance symbol ->
            { maxSupply = { amount = capFloatWithSymbol symbol maxSupply, symbol = symbol }
            , minBalance = { amount = capFloatWithSymbol symbol minBalance, symbol = symbol }
            }
        )
        |> withRandom (Random.float 0 1000)
        |> withRandom (Random.float -1000 1000)
        |> withRandom symbolGenerator


createTokenDataGenerator : Random.Generator Token.CreateTokenData
createTokenDataGenerator =
    Random.constant
        (\creator maxSupply minBalance symbol tokenType ->
            { creator = creator
            , maxSupply = { amount = capFloatWithSymbol symbol maxSupply, symbol = symbol }
            , minBalance = { amount = capFloatWithSymbol symbol minBalance, symbol = symbol }
            , tokenType = tokenType
            }
        )
        |> withRandom nameGenerator
        |> withRandom (Random.float 0 1000)
        |> withRandom (Random.float -1000 1000)
        |> withRandom symbolGenerator
        |> withRandom (Random.Extra.choice Token.Mcc Token.Expiry)


expiryOptsDataGenerator : Random.Generator Token.ExpiryOptsData
expiryOptsDataGenerator =
    Random.constant
        (\symbol naturalExpiration juridicalExpiration renovationAmount ->
            { currency = symbol
            , naturalExpirationPeriod = naturalExpiration
            , juridicalExpirationPeriod = juridicalExpiration
            , renovationAmount = { amount = capFloatWithSymbol symbol renovationAmount, symbol = symbol }
            }
        )
        |> withRandom symbolGenerator
        |> withRandom (Random.int 0 Random.maxInt)
        |> withRandom (Random.int 0 Random.maxInt)
        |> withRandom (Random.float 0 1000)



-- SHRINKERS


urlShrinker : Shrink.Shrinker Url.Url
urlShrinker url =
    Shrink.map Url.Url (Shrink.noShrink url.protocol)
        |> Shrink.andMap (Shrink.string url.host)
        |> Shrink.andMap (Shrink.noShrink url.port_)
        |> Shrink.andMap (Shrink.string url.path)
        |> Shrink.andMap (Shrink.maybe Shrink.string url.query)
        |> Shrink.andMap (Shrink.maybe Shrink.string url.fragment)


timeShrinker : Shrink.Shrinker Time.Posix
timeShrinker =
    Shrink.convert Time.millisToPosix Time.posixToMillis Shrink.int


avatarShrinker : Shrink.Shrinker Avatar.Avatar
avatarShrinker =
    Shrink.convert
        (\maybeString ->
            case maybeString of
                Nothing ->
                    Avatar.empty

                Just string ->
                    Avatar.fromString string
        )
        Avatar.toMaybeString
        (Shrink.maybe Shrink.string)


nameShrinker : Shrink.Shrinker Eos.Name
nameShrinker =
    Shrink.convert Eos.stringToName Eos.nameToString Shrink.string



-- FUZZERS


cambiatusUrlFuzzer : Maybe String -> Fuzz.Fuzzer Url.Url
cambiatusUrlFuzzer maybeAuthority =
    Fuzz.custom (cambiatusUrlGenerator maybeAuthority) urlShrinker


actionFuzzer : Fuzz.Fuzzer Action.Action
actionFuzzer =
    Fuzz.custom actionGenerator Shrink.noShrink


timeFuzzer : Fuzz.Fuzzer Time.Posix
timeFuzzer =
    Fuzz.custom timeGenerator timeShrinker


avatarFuzzer : Fuzz.Fuzzer Avatar.Avatar
avatarFuzzer =
    Fuzz.custom avatarGenerator avatarShrinker


claimFuzzer : Fuzz.Fuzzer Claim.Model
claimFuzzer =
    Fuzz.custom claimGenerator Shrink.noShrink


nameFuzzer : Fuzz.Fuzzer Eos.Name
nameFuzzer =
    Fuzz.custom nameGenerator nameShrinker


createCommunityDataInputFuzzer : Fuzz.Fuzzer Community.CreateCommunityDataInput
createCommunityDataInputFuzzer =
    Fuzz.custom createCommunityDataInputGenerator Shrink.noShrink


updateTokenDataFuzzer : Fuzz.Fuzzer Token.UpdateTokenData
updateTokenDataFuzzer =
    Fuzz.custom updateTokenDataGenerator Shrink.noShrink


createTokenDataFuzzer : Fuzz.Fuzzer Token.CreateTokenData
createTokenDataFuzzer =
    Fuzz.custom createTokenDataGenerator Shrink.noShrink


expiryOptsDataFuzzer : Fuzz.Fuzzer Token.ExpiryOptsData
expiryOptsDataFuzzer =
    Fuzz.custom expiryOptsDataGenerator Shrink.noShrink
