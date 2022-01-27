module TestHelpers.Fuzz exposing
    ( action
    , avatar
    , cambiatusUrl
    , cedulaDeIdentidad
    , claim
    , createCommunityDataInput
    , createTokenData
    , dimex
    , expiryOptsData
    , maybeDateTime
    , name
    , nite
    , phone
    , time
    , updateTokenData
    )

import Action
import Avatar
import Cambiatus.Scalar
import Claim
import Community
import Eos.Account as Eos
import Fuzz exposing (Fuzzer)
import Shrink
import TestHelpers.Random as Random
import TestHelpers.Shrink as Shrink
import Time
import Token
import Url



-- FUZZERS


time : Fuzzer Time.Posix
time =
    Fuzz.custom Random.time Shrink.time


maybeDateTime : Fuzzer (Maybe Cambiatus.Scalar.DateTime)
maybeDateTime =
    Fuzz.custom (Random.maybe Random.dateTime) (Shrink.maybe Shrink.dateTime)


cambiatusUrl : Maybe String -> Fuzzer Url.Url
cambiatusUrl maybeAuthority =
    Fuzz.custom (Random.cambiatusUrl maybeAuthority) Shrink.url


avatar : Fuzzer Avatar.Avatar
avatar =
    Fuzz.custom Random.avatar Shrink.avatar


name : Fuzzer Eos.Name
name =
    Fuzz.custom Random.name Shrink.noShrink



-- ACTION


action : Fuzzer Action.Action
action =
    Fuzz.custom Random.action Shrink.action



-- CLAIM


claim : Fuzzer Claim.Model
claim =
    Fuzz.custom Random.claim Shrink.claim



-- COMMUNITY


createCommunityDataInput : Fuzzer Community.CreateCommunityDataInput
createCommunityDataInput =
    Fuzz.custom Random.createCommunityDataInput Shrink.createCommunityDataInput



-- TOKEN


updateTokenData : Fuzzer Token.UpdateTokenData
updateTokenData =
    Fuzz.custom Random.updateTokenData Shrink.updateTokenData


createTokenData : Fuzzer Token.CreateTokenData
createTokenData =
    Fuzz.custom Random.createTokenData Shrink.createTokenData


expiryOptsData : Fuzzer Token.ExpiryOptsData
expiryOptsData =
    Fuzz.custom Random.expiryOptsData Shrink.expiryOptsData



-- KYC


nite : Fuzzer String
nite =
    Fuzz.custom Random.nite Shrink.noShrink


phone : Fuzzer String
phone =
    Fuzz.custom Random.phone Shrink.noShrink


dimex : Fuzzer String
dimex =
    Fuzz.custom Random.dimex Shrink.noShrink


cedulaDeIdentidad : Fuzzer String
cedulaDeIdentidad =
    Fuzz.custom Random.cedulaDeIdentidad Shrink.noShrink
