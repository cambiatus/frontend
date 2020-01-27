module Community exposing (Action, ActionVerification, ActionVerificationsResponse, Balance, ClaimResponse, Community, CreateCommunityData, CreateTokenData, DashboardInfo, Metadata, Objective, Transaction, Validator, Verification(..), Verifiers, WithObjectives, claimSelectionSet, communitiesQuery, communityQuery, createCommunityData, decodeBalance, decodeTransaction, encodeClaimAction, encodeCreateActionAction, encodeCreateCommunityData, encodeCreateObjectiveAction, encodeCreateTokenData, encodeUpdateLogoData, encodeUpdateObjectiveAction, logoBackground, logoTitleQuery, logoUrl, newCommunitySubscription, toVerifications)

import Account exposing (Profile, accountSelectionSet)
import Api.Relay exposing (MetadataConnection, PaginationArgs)
import Bespiral.Enum.VerificationType exposing (VerificationType(..))
import Bespiral.Object
import Bespiral.Object.Action as Action
import Bespiral.Object.Check as Check
import Bespiral.Object.Claim as Claim exposing (ChecksOptionalArguments)
import Bespiral.Object.Community as Community
import Bespiral.Object.Objective as Objective
import Bespiral.Object.Validator
import Bespiral.Query as Query
import Bespiral.Scalar exposing (DateTime(..))
import Bespiral.Subscription as Subscription
import Eos exposing (EosBool(..), Symbol, symbolToString)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Decode exposing (required)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)
import Transfer exposing (ConnectionTransfer, Transfer, metadataConnectionSelectionSet, transferConnectionSelectionSet)
import User exposing (User, selectionSet)
import Utils
import View.Tag as Tag



-- DashboardInfo for Dashboard


type alias DashboardInfo =
    { title : String
    , logo : String
    , members : List Profile
    }



-- METADATA
-- Used on community listing


type alias Metadata =
    { title : String
    , description : String
    , symbol : Symbol
    , logo : String
    , creator : Eos.Name
    , transfers : Maybe MetadataConnection
    , memberCount : Int
    }



-- Community Data


type alias Community =
    { title : String
    , description : String
    , symbol : Symbol
    , logo : String
    , creator : Eos.Name
    , inviterReward : Float
    , invitedReward : Float
    , memberCount : Int
    , members : List Profile
    , transfers : Maybe ConnectionTransfer
    , objectives : List Objective
    }



-- GraphQL


communitiesSelectionSet : (PaginationArgs -> PaginationArgs) -> SelectionSet Metadata Bespiral.Object.Community
communitiesSelectionSet paginateArgs =
    SelectionSet.succeed Metadata
        |> with Community.name
        |> with Community.description
        |> with (Eos.symbolSelectionSet Community.symbol)
        |> with Community.logo
        |> with (Eos.nameSelectionSet Community.creator)
        |> with
            (Community.transfers
                paginateArgs
                metadataConnectionSelectionSet
            )
        |> with Community.memberCount


dashboardSelectionSet : SelectionSet DashboardInfo Bespiral.Object.Community
dashboardSelectionSet =
    SelectionSet.succeed DashboardInfo
        |> with Community.name
        |> with Community.logo
        |> with (Community.members accountSelectionSet)


communitySelectionSet : (PaginationArgs -> PaginationArgs) -> SelectionSet Community Bespiral.Object.Community
communitySelectionSet paginateArgs =
    SelectionSet.succeed Community
        |> with Community.name
        |> with Community.description
        |> with (Eos.symbolSelectionSet Community.symbol)
        |> with Community.logo
        |> with (Eos.nameSelectionSet Community.creator)
        |> with Community.inviterReward
        |> with Community.invitedReward
        |> with Community.memberCount
        |> with (Community.members accountSelectionSet)
        |> with
            (Community.transfers
                paginateArgs
                transferConnectionSelectionSet
            )
        |> with (Community.objectives objectiveSelectionSet)



-- Communities Query


communitiesQuery : SelectionSet (List Metadata) RootQuery
communitiesQuery =
    communitiesSelectionSet
        (\args ->
            { args | first = Present 0 }
        )
        |> Query.communities



-- NEW COMMUNITY NAME


type alias NewCommunity =
    { title : String }


newCommunitySubscription : Symbol -> SelectionSet NewCommunity RootSubscription
newCommunitySubscription symbol =
    let
        stringSymbol =
            symbolToString symbol
                |> String.toUpper

        selectionSet =
            SelectionSet.succeed NewCommunity
                |> with Community.name

        args =
            { input = { symbol = stringSymbol } }
    in
    Subscription.newcommunity args selectionSet


logoTitleQuery : Symbol -> SelectionSet (Maybe DashboardInfo) RootQuery
logoTitleQuery symbol =
    Query.community { symbol = symbolToString symbol } <| dashboardSelectionSet


type alias WithObjectives =
    { metadata : Metadata
    , objectives : List Objective
    }


communityQuery : Symbol -> SelectionSet (Maybe Community) RootQuery
communityQuery symbol =
    Query.community { symbol = symbolToString symbol } <|
        communitySelectionSet
            (\args ->
                { args | first = Present 10 }
            )


logoUrl : String -> Maybe String -> String
logoUrl ipfsUrl maybeHash =
    case maybeHash of
        Nothing ->
            logoPlaceholder ipfsUrl

        Just hash ->
            if String.isEmpty (String.trim hash) then
                logoPlaceholder ipfsUrl

            else
                ipfsUrl ++ "/" ++ hash


logoBackground : String -> Maybe String -> Html.Attribute msg
logoBackground ipfsUrl maybeHash =
    Html.Attributes.style "background-image"
        ("url(" ++ logoUrl ipfsUrl maybeHash ++ ")")


logoPlaceholder : String -> String
logoPlaceholder ipfsUrl =
    ipfsUrl ++ "/QmXuf6y8TMGRN96HZEy86c8N9aDseaeyuCQ5qVLqPyd8Ld"



-- OBJECTIVE


type alias Objective =
    { id : Int
    , description : String
    , creator : Eos.Name
    , actions : List Action
    }


objectiveSelectionSet : SelectionSet Objective Bespiral.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> with Objective.id
        |> with Objective.description
        |> with (Eos.nameSelectionSet Objective.creatorId)
        |> with (Objective.actions identity actionSelectionSet)


type alias CreateObjectiveAction =
    { symbol : Symbol
    , description : String
    , creator : Eos.Name
    }


encodeCreateObjectiveAction : CreateObjectiveAction -> Value
encodeCreateObjectiveAction c =
    Encode.object
        [ ( "cmm_asset", Encode.string ("0 " ++ Eos.symbolToString c.symbol) )
        , ( "description", Encode.string c.description )
        , ( "creator", Eos.encodeName c.creator )
        ]


type alias UpdateObjectiveAction =
    { objectiveId : Int
    , description : String
    , editor : Eos.Name
    }


encodeUpdateObjectiveAction : UpdateObjectiveAction -> Value
encodeUpdateObjectiveAction c =
    Encode.object
        [ ( "objective_id", Encode.int c.objectiveId )
        , ( "description", Encode.string c.description )
        , ( "editor", Eos.encodeName c.editor )
        ]



-- ACTION


type alias Action =
    { description : String
    , reward : Float
    , verificationReward : Float
    , creator : Eos.Name
    , validators : List Validator
    , usages : Int
    , usagesLeft : Int
    , deadline : Maybe DateTime
    , verificationType : VerificationType
    , id : Int
    , isCompleted : Bool
    }


type alias Validator =
    { validator : User }


actionSelectionSet : SelectionSet Action Bespiral.Object.Action
actionSelectionSet =
    SelectionSet.succeed Action
        |> with Action.description
        |> with Action.reward
        |> with Action.verifierReward
        |> with (Eos.nameSelectionSet Action.creatorId)
        |> with (Action.validators validatorSelectionSet)
        |> with Action.usages
        |> with Action.usagesLeft
        |> with Action.deadline
        |> with Action.verificationType
        |> with Action.id
        |> with Action.isCompleted


validatorSelectionSet : SelectionSet Validator Bespiral.Object.Validator
validatorSelectionSet =
    SelectionSet.succeed Validator
        |> with (Bespiral.Object.Validator.validator User.selectionSet)


type Verification
    = Manually Verifiers
    | Automatically String


type alias Verifiers =
    { verifiers : List String
    , reward : Float
    }



---- ACTION CREATE


type alias CreateActionAction =
    { actionId : Int
    , objectiveId : Int
    , description : String
    , reward : String
    , verifier_reward : String
    , deadline : Int
    , usages : Int
    , usagesLeft : Int
    , verifications : Int
    , verificationType : String
    , validatorsStr : String
    , isCompleted : Int
    , creator : Eos.Name
    }


encodeCreateActionAction : CreateActionAction -> Value
encodeCreateActionAction c =
    Encode.object
        [ ( "action_id", Encode.int c.actionId )
        , ( "objective_id", Encode.int c.objectiveId )
        , ( "description", Encode.string c.description )
        , ( "reward", Encode.string c.reward )
        , ( "verifier_reward", Encode.string c.verifier_reward )
        , ( "deadline", Encode.int c.deadline )
        , ( "usages", Encode.int c.usages )
        , ( "usages_left", Encode.int c.usagesLeft )
        , ( "verifications", Encode.int c.verifications )
        , ( "verification_type", Encode.string c.verificationType )
        , ( "validators_str", Encode.string c.validatorsStr )
        , ( "is_completed", Encode.int c.isCompleted )
        , ( "creator", Eos.encodeName c.creator )
        ]



-- Claim Action


type alias ClaimAction =
    { actionId : Int
    , maker : Eos.Name
    }


encodeClaimAction : ClaimAction -> Value
encodeClaimAction c =
    Encode.object
        [ ( "action_id", Encode.int c.actionId )
        , ( "maker", Eos.encodeName c.maker )
        ]



-- Balance


type alias Balance =
    { asset : Eos.Asset
    , lastActivity : Posix
    }


decodeBalance : Decoder Balance
decodeBalance =
    Decode.succeed Balance
        |> required "balance" Eos.decodeAsset
        |> required "last_activity" Utils.decodeTimestamp



-- Transaction


type alias Transaction =
    { id : String
    , accountFrom : Eos.Name
    , symbol : Eos.Symbol
    }


decodeTransaction : Decoder Transaction
decodeTransaction =
    Decode.succeed Transaction
        |> required "txId" string
        |> required "accountFrom" Eos.nameDecoder
        |> required "symbol" Eos.symbolDecoder



-- CREATE COMMUNITY


type alias CreateCommunityData =
    { cmmAsset : Eos.Asset
    , creator : Eos.Name
    , logoHash : String
    , name : String
    , description : String
    , inviterReward : Eos.Asset
    , invitedReward : Eos.Asset
    }


createCommunityData :
    { accountName : Eos.Name
    , symbol : Eos.Symbol
    , logoHash : String
    , name : String
    , description : String
    , inviterReward : Float
    , invitedReward : Float
    }
    -> CreateCommunityData
createCommunityData params =
    { cmmAsset =
        { amount = 0
        , symbol = params.symbol
        }
    , creator = params.accountName
    , logoHash = params.logoHash
    , name = params.name
    , description = params.description
    , inviterReward =
        { amount = params.inviterReward
        , symbol = params.symbol
        }
    , invitedReward =
        { amount = params.invitedReward
        , symbol = params.symbol
        }
    }


encodeCreateCommunityData : CreateCommunityData -> Value
encodeCreateCommunityData c =
    Encode.object
        [ ( "cmm_asset", Eos.encodeAsset c.cmmAsset )
        , ( "creator", Eos.encodeName c.creator )
        , ( "logo", Encode.string c.logoHash )
        , ( "name", Encode.string c.name )
        , ( "description", Encode.string c.description )
        , ( "inviter_reward", Eos.encodeAsset c.inviterReward )
        , ( "invited_reward", Eos.encodeAsset c.invitedReward )
        ]


type alias CreateTokenData =
    { creator : Eos.Name
    , maxSupply : Eos.Asset
    , minBalance : Eos.Asset
    , tokenType : String
    }


encodeCreateTokenData : CreateTokenData -> Value
encodeCreateTokenData c =
    Encode.object
        [ ( "issuer", Eos.encodeName c.creator )
        , ( "max_supply", Eos.encodeAsset c.maxSupply )
        , ( "min_balance", Eos.encodeAsset c.minBalance )
        , ( "type", Encode.string c.tokenType )
        ]


type alias UpdateCommunityData =
    { asset : Eos.Asset
    , logo : String
    , name : String
    , description : String
    , inviterReward : Eos.Asset
    , invitedReward : Eos.Asset
    }


encodeUpdateLogoData : UpdateCommunityData -> Value
encodeUpdateLogoData c =
    Encode.object
        [ ( "logo", Encode.string c.logo )
        , ( "cmm_asset", Eos.encodeAsset c.asset )
        , ( "name", Encode.string c.name )
        , ( "description", Encode.string c.description )
        , ( "inviter_reward", Eos.encodeAsset c.inviterReward )
        , ( "invited_reward", Eos.encodeAsset c.invitedReward )
        ]



-- Action Verification


type alias ActionVerification =
    { symbol : Maybe Symbol
    , logo : String
    , objectiveId : Int
    , actionId : Int
    , claimId : Int
    , description : String
    , createdAt : DateTime
    , status : Tag.TagStatus
    }


type alias ActionVerificationsResponse =
    { claims : List ClaimResponse }


type alias ClaimResponse =
    { id : Int
    , createdAt : DateTime
    , checks : List CheckResponse
    , action : ActionResponse
    }


type alias CheckResponse =
    { isVerified : Bool
    }


type alias ActionResponse =
    { id : Int
    , description : String
    , objective : ObjectiveResponse
    }


type alias ObjectiveResponse =
    { id : Int
    , community : CommunityResponse
    }


type alias CommunityResponse =
    { symbol : String
    , logo : String
    }



-- Verifications SelectionSets


claimSelectionSet : String -> SelectionSet ClaimResponse Bespiral.Object.Claim
claimSelectionSet validator =
    let
        checksArg : ChecksOptionalArguments -> ChecksOptionalArguments
        checksArg _ =
            { input = Present { validator = Present validator }
            }
    in
    SelectionSet.succeed ClaimResponse
        |> with Claim.id
        |> with Claim.createdAt
        |> with (Claim.checks checksArg checkSelectionSet)
        |> with (Claim.action verificationActionSelectionSet)


checkSelectionSet : SelectionSet CheckResponse Bespiral.Object.Check
checkSelectionSet =
    SelectionSet.succeed CheckResponse
        |> with Check.isVerified


verificationActionSelectionSet : SelectionSet ActionResponse Bespiral.Object.Action
verificationActionSelectionSet =
    SelectionSet.succeed ActionResponse
        |> with Action.id
        |> with Action.description
        |> with (Action.objective verificationObjectiveSelectionSet)


verificationObjectiveSelectionSet : SelectionSet ObjectiveResponse Bespiral.Object.Objective
verificationObjectiveSelectionSet =
    SelectionSet.succeed ObjectiveResponse
        |> with Objective.id
        |> with (Objective.community verificationCommunitySelectionSet)


verificationCommunitySelectionSet : SelectionSet CommunityResponse Bespiral.Object.Community
verificationCommunitySelectionSet =
    SelectionSet.succeed CommunityResponse
        |> with Community.symbol
        |> with Community.logo



-- convert claims response to verification


toVerifications : ActionVerificationsResponse -> List ActionVerification
toVerifications actionVerificationResponse =
    let
        claimsResponse : List ClaimResponse
        claimsResponse =
            actionVerificationResponse.claims

        toStatus : List CheckResponse -> Tag.TagStatus
        toStatus checks =
            case List.head checks of
                Just check ->
                    if check.isVerified == True then
                        Tag.APPROVED

                    else
                        Tag.DISAPPROVED

                Nothing ->
                    Tag.PENDING

        toVerification : ClaimResponse -> ActionVerification
        toVerification claimResponse =
            { symbol = Eos.symbolFromString claimResponse.action.objective.community.symbol
            , logo = claimResponse.action.objective.community.logo
            , objectiveId = claimResponse.action.objective.id
            , actionId = claimResponse.action.id
            , claimId = claimResponse.id
            , description = claimResponse.action.description
            , createdAt = claimResponse.createdAt
            , status = toStatus claimResponse.checks
            }
    in
    List.map
        toVerification
        claimsResponse
