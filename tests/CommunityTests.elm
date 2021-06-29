module CommunityTests exposing (all)

import Community
import Dict
import Eos
import Expect
import Graphql.Http
import Graphql.Http.GraphqlError
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import TestHelpers.Fuzz as Fuzz


all : Test
all =
    describe "Community"
        [ createCommunityData, isNonExistingCommunityError ]



-- CREATE COMMUNITY DATA


createCommunityData : Test
createCommunityData =
    describe "CreateCommunityData"
        [ fuzz Fuzz.createCommunityDataInput "CreateCommunityDataInput creates correct CreateCommunityData" <|
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
        , fuzz Fuzz.createCommunityDataInput "Encoding and decoding is a no-op" <|
            \fuzzCreateCommunityDataInput ->
                fuzzCreateCommunityDataInput
                    |> Community.createCommunityData
                    |> Community.encodeCreateCommunityData
                    |> Decode.decodeValue Community.createCommunityDataDecoder
                    |> Expect.equal (Ok (Community.createCommunityData fuzzCreateCommunityDataInput))
        ]



-- IS NON EXISTING COMMUNITY ERROR


isNonExistingCommunityError : Test
isNonExistingCommunityError =
    let
        stringToGraphqlError : String -> Graphql.Http.Error a
        stringToGraphqlError error =
            Graphql.Http.GraphqlError (Graphql.Http.GraphqlError.UnparsedData Encode.null)
                [ { message = error, locations = Nothing, details = Dict.empty } ]
    in
    describe "isNonExistingCommunityError"
        [ test "reports error for community that doesn't exist" <|
            \() ->
                stringToGraphqlError "No community found using the domain thisisntarealcommunityanditneverwillbe.staging.cambiatus.io"
                    |> Community.isNonExistingCommunityError
                    |> Expect.true "expected to detect as an error about a community that doesn't exist"
        , test "doesn't report error for community that exists" <|
            \() ->
                stringToGraphqlError "Some other error"
                    |> Community.isNonExistingCommunityError
                    |> Expect.false "expected to know it's not an error about a community that doesn't exist"
        ]
