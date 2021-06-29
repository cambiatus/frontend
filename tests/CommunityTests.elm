module CommunityTests exposing (all)

import Community
import Eos
import Expect
import Json.Decode as Decode
import Test exposing (..)
import TestUtils


all : Test
all =
    describe "Community"
        [ createCommunityData ]



-- CREATE COMMUNITY DATA


createCommunityData : Test
createCommunityData =
    describe "CreateCommunityData"
        [ fuzz TestUtils.createCommunityDataInputFuzzer "CreateCommunityDataInput creates correct CreateCommunityData" <|
            \fuzzCreateCommunityDataInput ->
                { cmmAsset = { amount = 0, symbol = fuzzCreateCommunityDataInput.symbol }
                , creator = fuzzCreateCommunityDataInput.accountName
                , logoUrl = fuzzCreateCommunityDataInput.logoUrl
                , name = fuzzCreateCommunityDataInput.name
                , description = fuzzCreateCommunityDataInput.description
                , subdomain = fuzzCreateCommunityDataInput.subdomain
                , inviterReward = { amount = fuzzCreateCommunityDataInput.inviterReward, symbol = fuzzCreateCommunityDataInput.symbol }
                , invitedReward = { amount = fuzzCreateCommunityDataInput.invitedReward, symbol = fuzzCreateCommunityDataInput.symbol }
                , hasShop = Eos.boolToEosBool fuzzCreateCommunityDataInput.hasShop
                , hasObjectives = Eos.boolToEosBool fuzzCreateCommunityDataInput.hasObjectives
                , hasKyc = Eos.boolToEosBool fuzzCreateCommunityDataInput.hasKyc
                , hasAutoInvite = Eos.boolToEosBool fuzzCreateCommunityDataInput.hasAutoInvite
                , website = fuzzCreateCommunityDataInput.website
                }
                    |> Expect.equal (Community.createCommunityData fuzzCreateCommunityDataInput)
        , fuzzWith { runs = 1 } TestUtils.createCommunityDataInputFuzzer "Encoding and decoding is a no-op" <|
            \fuzzCreateCommunityDataInput ->
                fuzzCreateCommunityDataInput
                    |> Community.createCommunityData
                    |> Community.encodeCreateCommunityData
                    |> Decode.decodeValue Community.createCommunityDataDecoder
                    |> Expect.equal (Ok (Community.createCommunityData fuzzCreateCommunityDataInput |> Debug.log "Created again"))
        ]
