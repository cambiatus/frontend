module TestHelpers.Random exposing
    ( action
    , avatar
    , cambiatusUrl
    , cedulaDeIdentidad
    , claim
    , createCommunityDataInput
    , createTokenData
    , dateTime
    , dimex
    , expiryOptsData
    , maybe
    , name
    , nite
    , phone
    , time
    , updateTokenData
    )

import Action
import Avatar
import Cambiatus.Enum.VerificationType as VerificationType
import Cambiatus.Scalar
import Claim
import Community
import Eos
import Eos.Account as Eos
import Iso8601
import Markdown
import Profile
import Random
import Random.Char
import Random.Extra
import Random.String
import Time
import Token
import Url



-- HELPERS


capFloatWithSymbol : Eos.Symbol -> Float -> Float
capFloatWithSymbol symbol_ value =
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
                |> (+) (Eos.getSymbolPrecision symbol_)
    in
    String.fromFloat value
        |> String.left maxLength
        |> String.toFloat
        |> Maybe.withDefault value


with : Random.Generator a -> Random.Generator (a -> c) -> Random.Generator c
with =
    Random.map2 (|>)


append : Random.Generator appendable -> Random.Generator appendable -> Random.Generator appendable
append secondGenerator firstGenerator =
    firstGenerator
        |> Random.andThen (\first -> Random.map (\second -> first ++ second) secondGenerator)



-- GENERATORS


nonZeroDigit : Random.Generator String
nonZeroDigit =
    Random.int 1 9
        |> Random.map String.fromInt


digit : Random.Generator String
digit =
    Random.int 0 9
        |> Random.map String.fromInt


digits : Int -> Random.Generator String
digits length =
    Random.list length digit |> Random.map String.concat


maybe : Random.Generator a -> Random.Generator (Maybe a)
maybe generator =
    Random.Extra.maybe Random.Extra.bool generator


listWithRandomLength : Int -> Int -> Random.Generator a -> Random.Generator (List a)
listWithRandomLength minLength maxLength generator =
    Random.int minLength maxLength
        |> Random.andThen (\length -> Random.list length generator)


string : Random.Generator String
string =
    Random.String.string 15 Random.Char.english


stringWithRandomLength : Int -> Int -> Random.Generator String
stringWithRandomLength minLength maxLength =
    Random.Char.english
        |> listWithRandomLength minLength maxLength
        |> Random.map String.fromList


time : Random.Generator Time.Posix
time =
    Random.int 0 Random.maxInt
        |> Random.map Time.millisToPosix


dateTime : Random.Generator Cambiatus.Scalar.DateTime
dateTime =
    time
        |> Random.map (Iso8601.fromTime >> Cambiatus.Scalar.DateTime)


url : List String -> Random.Generator Url.Url
url allowedAuthorities =
    let
        protocolGenerator =
            Random.Extra.choice Url.Http Url.Https

        host =
            case allowedAuthorities of
                [] ->
                    string
                        |> Random.map (\randomString -> randomString ++ ".")
                        |> append string

                firstAuthority :: rest ->
                    string
                        |> append
                            (Random.uniform firstAuthority rest)

        pathGenerator =
            listWithRandomLength 0 5 string
                |> Random.map (String.join "/")
                |> Random.map (\path -> "/" ++ path)

        queryGenerator =
            string
                |> Random.map (\queryKey -> queryKey ++ "=")
                |> append string
                |> listWithRandomLength 0 5
                |> Random.map (String.join "&")
                |> Random.map
                    (\queryString ->
                        if String.isEmpty queryString then
                            Nothing

                        else
                            Just queryString
                    )

        fragmentGenerator =
            string
                |> Random.map (\fragmentString -> fragmentString |> Just)
                |> List.singleton
                |> Random.uniform (Random.constant Nothing)
                |> Random.andThen identity
    in
    Random.constant Url.Url
        |> with protocolGenerator
        |> with host
        |> with (Random.constant Nothing)
        |> with pathGenerator
        |> with queryGenerator
        |> with fragmentGenerator
        |> Random.map
            (\url_ ->
                if String.endsWith "localhost" url_.host then
                    { url_ | port_ = Just 3000 }

                else
                    { url_ | port_ = Nothing }
            )


cambiatusUrl : Maybe String -> Random.Generator Url.Url
cambiatusUrl maybeAuthority =
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
    url allowedAuthorities


symbol : Random.Generator Eos.Symbol
symbol =
    digit
        |> append (Random.constant ",")
        |> append (stringWithRandomLength Eos.minSymbolLength Eos.maxSymbolLength)
        |> Random.map (Eos.symbolFromString >> Maybe.withDefault Eos.cambiatusSymbol)


name : Random.Generator Eos.Name
name =
    Random.String.string 12 Random.Char.english
        |> Random.map Eos.stringToName


avatar : Random.Generator Avatar.Avatar
avatar =
    maybe string
        |> Random.map
            (\maybeString ->
                case maybeString of
                    Nothing ->
                        Avatar.empty

                    Just avatarString ->
                        Avatar.fromString avatarString
            )


email : Random.Generator String
email =
    string
        |> append (Random.constant "@")
        |> append string
        |> append (Random.constant ".com")



--PROFILE


minimalProfile : Random.Generator Profile.Minimal
minimalProfile =
    Random.constant Profile.Minimal
        |> with (maybe string)
        |> with name
        |> with avatar
        |> with (maybe email)
        |> with (maybe (Markdown.generator string))
        |> with (Random.constant [])



-- ACTION


actionObjective : Random.Generator Action.Objective
actionObjective =
    Random.constant Action.Objective
        |> with (Random.int 0 Random.maxInt |> Random.map Action.objectiveIdFromInt)
        |> with (Markdown.generator string)
        |> with
            (Random.map2 (\symbol_ name_ -> { symbol = symbol_, name = name_ })
                symbol
                string
            )
        |> with Random.Extra.bool


actionId : Random.Generator Action.Id
actionId =
    Random.int 0 Random.maxInt
        |> Random.map Action.idFromInt


action : Random.Generator Action.Action
action =
    Random.constant Action.Action
        |> with actionId
        |> with (Markdown.generator string)
        |> with (Random.constant Nothing)
        |> with actionObjective
        |> with (Random.float 0 1000)
        |> with (Random.float 0 1000)
        |> with name
        |> with (listWithRandomLength 3 20 minimalProfile)
        |> with (Random.int 0 Random.maxInt)
        |> with (Random.int 0 Random.maxInt)
        |> with (maybe dateTime)
        |> with (Random.Extra.choice VerificationType.Automatic VerificationType.Claimable)
        |> with (Random.uniform 3 [ 5, 7, 9 ])
        |> with Random.Extra.bool
        |> with Random.Extra.bool
        |> with Random.Extra.bool
        |> with (maybe (Markdown.generator string))
        |> with (maybe (Random.int 0 Random.maxInt))
        |> with (Random.int 0 Random.maxInt)



-- CLAIM


claim : Random.Generator Claim.Model
claim =
    let
        check : Random.Generator Claim.Check
        check =
            Random.constant Claim.Check
                |> with Random.Extra.bool
                |> with minimalProfile
    in
    Random.constant Claim.Model
        |> with (Random.int 0 Random.maxInt)
        |> with (Random.uniform Claim.Approved [ Claim.Rejected, Claim.Pending ])
        |> with minimalProfile
        |> with action
        |> with (listWithRandomLength 3 20 check)
        |> with dateTime
        |> with (maybe string)
        |> with (maybe string)
        |> Random.map
            (\claim_ ->
                { claim_
                    | checks =
                        List.map2 (\check_ validator -> { check_ | validator = validator })
                            claim_.checks
                            claim_.action.validators
                }
            )



-- COMMUNITY


createCommunityDataInput : Random.Generator Community.CreateCommunityDataInput
createCommunityDataInput =
    Random.constant Community.CreateCommunityDataInput
        |> with name
        |> with symbol
        |> with (url [] |> Random.map Url.toString)
        |> with string
        |> with (Markdown.generator string)
        |> with (cambiatusUrl Nothing |> Random.map Url.toString)
        |> with (Random.float 0 1000)
        |> with (Random.float 0 1000)
        |> with Random.Extra.bool
        |> with Random.Extra.bool
        |> with Random.Extra.bool
        |> with Random.Extra.bool
        |> with (url [] |> Random.map Url.toString)
        |> Random.map
            (\input ->
                { input
                    | invitedReward = capFloatWithSymbol input.symbol input.invitedReward
                    , inviterReward = capFloatWithSymbol input.symbol input.inviterReward
                }
            )



-- TOKEN


updateTokenData : Random.Generator Token.UpdateTokenData
updateTokenData =
    Random.constant
        (\maxSupply minBalance symbol_ ->
            { maxSupply = { amount = capFloatWithSymbol symbol_ maxSupply, symbol = symbol_ }
            , minBalance = { amount = capFloatWithSymbol symbol_ minBalance, symbol = symbol_ }
            }
        )
        |> with (Random.float 0 1000)
        |> with (Random.float -1000 1000)
        |> with symbol


createTokenData : Random.Generator Token.CreateTokenData
createTokenData =
    Random.constant
        (\creator maxSupply minBalance symbol_ tokenType ->
            { creator = creator
            , maxSupply = { amount = capFloatWithSymbol symbol_ maxSupply, symbol = symbol_ }
            , minBalance = { amount = capFloatWithSymbol symbol_ minBalance, symbol = symbol_ }
            , tokenType = tokenType
            }
        )
        |> with name
        |> with (Random.float 0 1000)
        |> with (Random.float -1000 1000)
        |> with symbol
        |> with (Random.Extra.choice Token.Mcc Token.Expiry)


expiryOptsData : Random.Generator Token.ExpiryOptsData
expiryOptsData =
    Random.constant
        (\symbol_ naturalExpiration juridicalExpiration renovationAmount ->
            { currency = symbol_
            , naturalExpirationPeriod = naturalExpiration
            , juridicalExpirationPeriod = juridicalExpiration
            , renovationAmount = { amount = capFloatWithSymbol symbol_ renovationAmount, symbol = symbol_ }
            }
        )
        |> with symbol
        |> with (Random.int 0 Random.maxInt)
        |> with (Random.int 0 Random.maxInt)
        |> with (Random.float 0 1000)



-- KYC


cedulaDeIdentidad : Random.Generator String
cedulaDeIdentidad =
    let
        maybeDash =
            Random.Extra.choice "" "-"
    in
    nonZeroDigit
        |> append maybeDash
        |> append (digits 4)
        |> append maybeDash
        |> append (digits 4)


dimex : Random.Generator String
dimex =
    nonZeroDigit
        |> append
            (listWithRandomLength 10 11 digit
                |> Random.map String.concat
            )


nite : Random.Generator String
nite =
    nonZeroDigit
        |> append (digits 9)


phone : Random.Generator String
phone =
    nonZeroDigit
        |> append (digits 3)
        |> append (Random.Extra.choice "" "-")
        |> append (digits 4)
