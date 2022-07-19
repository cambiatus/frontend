module TestHelpers.Shrink exposing
    ( action
    , avatar
    , claim
    , createCommunityDataInput
    , createTokenData
    , dateTime
    , expiryOptsData
    , time
    , updateTokenData
    , url
    )

import Action
import Avatar
import Cambiatus.Scalar
import Claim
import Community
import Eos
import Iso8601
import Markdown
import Profile
import Shrink exposing (Shrinker, andMap, bool, float, int, list, maybe, noShrink, string)
import Time
import Token
import Url



-- SHRINKERS


asset : Shrinker Eos.Asset
asset asset_ =
    noShrink Eos.Asset
        |> andMap (float asset_.amount)
        |> andMap (noShrink asset_.symbol)


time : Shrinker Time.Posix
time =
    Shrink.convert Time.millisToPosix Time.posixToMillis int


dateTime : Shrinker Cambiatus.Scalar.DateTime
dateTime =
    let
        toTime : Cambiatus.Scalar.DateTime -> Time.Posix
        toTime (Cambiatus.Scalar.DateTime dateTime_) =
            dateTime_
                |> Iso8601.toTime
                |> Result.withDefault (Time.millisToPosix 0)

        fromTime : Time.Posix -> Cambiatus.Scalar.DateTime
        fromTime time_ =
            Iso8601.fromTime time_
                |> Cambiatus.Scalar.DateTime
    in
    Shrink.convert fromTime toTime time


url : Shrinker Url.Url
url url_ =
    noShrink Url.Url
        |> andMap (noShrink url_.protocol)
        |> andMap (string url_.host)
        |> andMap (noShrink url_.port_)
        |> andMap (string url_.path)
        |> andMap (maybe string url_.query)
        |> andMap (maybe string url_.fragment)


avatar : Shrinker Avatar.Avatar
avatar avatar_ =
    case Avatar.toMaybeString avatar_ of
        Nothing ->
            noShrink Avatar.empty

        Just avatarString ->
            string avatarString
                |> Shrink.map Avatar.fromString



-- PROFILE


minimalProfile : Shrinker Profile.Minimal
minimalProfile profile =
    noShrink Profile.Minimal
        |> andMap (maybe string profile.name)
        |> andMap (noShrink profile.account)
        |> andMap (avatar profile.avatar)
        |> andMap (noShrink profile.email)
        |> andMap (maybe Markdown.shrink profile.bio)
        |> andMap (noShrink profile.contacts)



-- ACTION


objectiveId : Shrinker Action.ObjectiveId
objectiveId =
    Shrink.convert Action.objectiveIdFromInt Action.objectiveIdToInt int


actionObjective : Shrinker Action.Objective
actionObjective objective =
    noShrink Action.Objective
        |> andMap (objectiveId objective.id)
        |> andMap (Markdown.shrink objective.description)
        |> andMap (noShrink objective.community)
        |> andMap (bool objective.isCompleted)


actionId : Shrinker Action.Id
actionId =
    Shrink.convert Action.idFromInt Action.idToInt int


action : Shrinker Action.Action
action action_ =
    noShrink Action.Action
        |> andMap (actionId action_.id)
        |> andMap (Markdown.shrink action_.description)
        |> andMap (noShrink action_.image)
        |> andMap (actionObjective action_.objective)
        |> andMap (float action_.reward)
        |> andMap (float action_.verifierReward)
        |> andMap (noShrink action_.creator)
        |> andMap (list minimalProfile action_.validators)
        |> andMap (int action_.usages)
        |> andMap (int action_.usagesLeft)
        |> andMap (maybe dateTime action_.deadline)
        |> andMap (noShrink action_.verificationType)
        |> andMap (int action_.verifications)
        |> andMap (bool action_.isCompleted)
        |> andMap (bool action_.hasProofPhoto)
        |> andMap (bool action_.hasProofCode)
        |> andMap (maybe Markdown.shrink action_.photoProofInstructions)
        |> andMap (maybe int action_.position)
        |> andMap (int action_.claimCount)



-- CLAIM


claimCheck : Shrinker Claim.Check
claimCheck check =
    noShrink Claim.Check
        |> andMap (bool check.isApproved)
        |> andMap (minimalProfile check.validator)


claim : Shrinker Claim.Model
claim claim_ =
    noShrink Claim.Model
        |> andMap (int claim_.id)
        |> andMap (noShrink claim_.status)
        |> andMap (minimalProfile claim_.claimer)
        |> andMap (action claim_.action)
        |> andMap (list claimCheck claim_.checks)
        |> andMap (dateTime claim_.createdAt)
        |> andMap (maybe string claim_.proofPhoto)
        |> andMap (maybe string claim_.proofCode)



-- COMMUNITY


createCommunityDataInput : Shrinker Community.CreateCommunityDataInput
createCommunityDataInput input =
    noShrink Community.CreateCommunityDataInput
        |> andMap (noShrink input.accountName)
        |> andMap (noShrink input.symbol)
        |> andMap (noShrink input.logoUrl)
        |> andMap (string input.name)
        |> andMap (Markdown.shrink input.description)
        |> andMap (noShrink input.subdomain)
        |> andMap (float input.inviterReward)
        |> andMap (float input.invitedReward)
        |> andMap (bool input.hasShop)
        |> andMap (bool input.hasObjectives)
        |> andMap (bool input.hasKyc)
        |> andMap (bool input.hasAutoInvite)
        |> andMap (noShrink input.website)



-- TOKEN


updateTokenData : Shrinker Token.UpdateTokenData
updateTokenData tokenData =
    noShrink Token.UpdateTokenData
        |> andMap (asset tokenData.maxSupply)
        |> andMap (asset tokenData.minBalance)


createTokenData : Shrinker Token.CreateTokenData
createTokenData tokenData =
    noShrink Token.CreateTokenData
        |> andMap (noShrink tokenData.creator)
        |> andMap (asset tokenData.maxSupply)
        |> andMap (asset tokenData.minBalance)
        |> andMap (noShrink tokenData.tokenType)


expiryOptsData : Shrinker Token.ExpiryOptsData
expiryOptsData expiryOpts =
    noShrink Token.ExpiryOptsData
        |> andMap (noShrink expiryOpts.currency)
        |> andMap (int expiryOpts.naturalExpirationPeriod)
        |> andMap (int expiryOpts.juridicalExpirationPeriod)
        |> andMap (asset expiryOpts.renovationAmount)
