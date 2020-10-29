module Community exposing
    ( Action
    , ActionVerification
    , ActionVerificationsResponse
    , Balance
    , ClaimResponse
    , CreateCommunityData
    , CreateTokenData
    , DashboardInfo
    , Invite
    , Metadata
    , Model
    , Objective
    , Settings
    , Transaction
    , Verification(..)
    , Verifiers
    , WithObjectives
    , claimSelectionSet
    , communitiesQuery
    , communityQuery
    , communitySelectionSet
    , createCommunityData
    , dashboardSelectionSet
    , decodeBalance
    , decodeTransaction
    , encodeClaimAction
    , encodeCreateActionAction
    , encodeCreateCommunityData
    , encodeCreateObjectiveAction
    , encodeCreateTokenData
    , encodeUpdateLogoData
    , encodeUpdateObjectiveAction
    , inviteQuery
    , logoBackground
    , logoTitleQuery
    , logoUrl
    , newCommunitySubscription
    , objectiveSelectionSet
    , settingsQuery
    , settingsSelectionSet
    , toVerifications
    )

import Cambiatus.Enum.VerificationType exposing (VerificationType(..))
import Cambiatus.Object
import Cambiatus.Object.Action as Action
import Cambiatus.Object.Check as Check
import Cambiatus.Object.Claim as Claim exposing (ChecksOptionalArguments)
import Cambiatus.Object.Community as Community
import Cambiatus.Object.Invite as Invite
import Cambiatus.Object.Objective as Objective
import Cambiatus.Query as Query
import Cambiatus.Scalar exposing (DateTime(..))
import Cambiatus.Subscription as Subscription
import Eos exposing (EosBool(..), Symbol, symbolToString)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html
import Html.Attributes
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Decode exposing (required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Time exposing (Posix)
import Utils
import View.Tag as Tag



-- DashboardInfo for Dashboard


type alias DashboardInfo =
    { name : String
    , logo : String
    , memberCount : Int
    , transferCount : Int
    , actionCount : Int
    , saleCount : Int
    , hasObjectives : Bool
    , creator : Eos.Name
    }



-- METADATA
-- Used on community listing


type alias Metadata =
    { title : String
    , description : String
    , symbol : Symbol
    , logo : String
    , creator : Eos.Name
    , memberCount : Int
    }



-- Community Data


type alias Model =
    { title : String
    , description : String
    , symbol : Symbol
    , logo : String
    , creator : Eos.Name
    , inviterReward : Float
    , invitedReward : Float
    , minBalance : Maybe Float
    , memberCount : Int
    , members : List Profile
    , objectives : List Objective
    , precision : Int
    , hasObjectives : Bool
    , hasShop : Bool
    , hasKyc : Bool
    }



-- GraphQL


communitiesSelectionSet : SelectionSet Metadata Cambiatus.Object.Community
communitiesSelectionSet =
    SelectionSet.succeed Metadata
        |> with Community.name
        |> with Community.description
        |> with Eos.symbolSelectionSet
        |> with Community.logo
        |> with (Eos.nameSelectionSet Community.creator)
        |> with Community.memberCount


dashboardSelectionSet : SelectionSet DashboardInfo Cambiatus.Object.Community
dashboardSelectionSet =
    SelectionSet.succeed DashboardInfo
        |> with Community.name
        |> with Community.logo
        |> with Community.memberCount
        |> with Community.transferCount
        |> with Community.actionCount
        |> with Community.saleCount
        |> with Community.hasObjectives
        |> with (Eos.nameSelectionSet Community.creator)


communitySelectionSet : SelectionSet Model Cambiatus.Object.Community
communitySelectionSet =
    SelectionSet.succeed Model
        |> with Community.name
        |> with Community.description
        |> with Eos.symbolSelectionSet
        |> with Community.logo
        |> with (Eos.nameSelectionSet Community.creator)
        |> with Community.inviterReward
        |> with Community.invitedReward
        |> with Community.minBalance
        |> with Community.memberCount
        |> with (Community.members Profile.selectionSet)
        |> with (Community.objectives objectiveSelectionSet)
        |> with Community.precision
        |> with Community.hasObjectives
        |> with Community.hasShop
        |> with Community.hasKyc


type alias Settings =
    { hasObjectives : Bool
    , hasShop : Bool
    , hasKyc : Bool
    }


settingsSelectionSet : SelectionSet Settings Cambiatus.Object.Community
settingsSelectionSet =
    SelectionSet.succeed Settings
        |> with Community.hasObjectives
        |> with Community.hasShop
        |> with Community.hasKyc



-- Communities Query


communitiesQuery : SelectionSet (List Metadata) RootQuery
communitiesQuery =
    Query.communities communitiesSelectionSet



-- NEW COMMUNITY NAME


type alias NewCommunity =
    String


newCommunitySubscription : Symbol -> SelectionSet NewCommunity RootSubscription
newCommunitySubscription symbol =
    let
        stringSymbol =
            symbolToString symbol
                |> String.toUpper

        selectionSet =
            Community.name

        args =
            { input = { symbol = stringSymbol } }
    in
    Subscription.newcommunity args selectionSet


logoTitleQuery : Symbol -> SelectionSet (Maybe DashboardInfo) RootQuery
logoTitleQuery symbol =
    Query.community { symbol = symbolToString symbol } dashboardSelectionSet


type alias WithObjectives =
    { metadata : Metadata
    , objectives : List Objective
    }


communityQuery : Symbol -> SelectionSet (Maybe Model) RootQuery
communityQuery symbol =
    Query.community { symbol = symbolToString symbol } communitySelectionSet


settingsQuery : Symbol -> SelectionSet (Maybe Settings) RootQuery
settingsQuery symbol =
    Query.community { symbol = symbolToString symbol } settingsSelectionSet


logoUrl : Maybe String -> String
logoUrl maybeUrl =
    let
        logoPlaceholder =
            "/icons/community_placeholder.png"
    in
    case maybeUrl of
        Nothing ->
            logoPlaceholder

        Just url ->
            if String.isEmpty (String.trim url) then
                logoPlaceholder

            else
                url


logoBackground : Maybe String -> Html.Attribute msg
logoBackground maybeUrl =
    Html.Attributes.style "background-image"
        ("url(" ++ logoUrl maybeUrl ++ ")")



-- OBJECTIVE


type alias Objective =
    { id : Int
    , description : String
    , creator : Eos.Name
    , actions : List Action
    , community : Metadata
    }


objectiveSelectionSet : SelectionSet Objective Cambiatus.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> with Objective.id
        |> with Objective.description
        |> with (Eos.nameSelectionSet Objective.creatorId)
        |> with (Objective.actions identity actionSelectionSet)
        |> with (Objective.community communitiesSelectionSet)


type alias CreateObjectiveAction =
    { asset : Eos.Asset
    , description : String
    , creator : Eos.Name
    }


encodeCreateObjectiveAction : CreateObjectiveAction -> Value
encodeCreateObjectiveAction c =
    Encode.object
        [ ( "cmm_asset", Eos.encodeAsset c.asset )
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
    { id : Int
    , description : String
    , reward : Float
    , verificationReward : Float
    , creator : Eos.Name
    , validators : List Profile
    , usages : Int
    , usagesLeft : Int
    , deadline : Maybe DateTime
    , verificationType : VerificationType
    , verifications : Int
    , isCompleted : Bool
    , hasProofPhoto : Maybe Bool
    , hasProofCode : Maybe Bool
    , photoProofInstructions : Maybe String
    }


actionSelectionSet : SelectionSet Action Cambiatus.Object.Action
actionSelectionSet =
    SelectionSet.succeed Action
        |> with Action.id
        |> with Action.description
        |> with Action.reward
        |> with Action.verifierReward
        |> with (Eos.nameSelectionSet Action.creatorId)
        |> with (Action.validators Profile.selectionSet)
        |> with Action.usages
        |> with Action.usagesLeft
        |> with Action.deadline
        |> with Action.verificationType
        |> with Action.verifications
        |> with Action.isCompleted
        |> with Action.hasProofPhoto
        |> with Action.hasProofCode
        |> with Action.photoProofInstructions


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
    , verifierReward : String
    , deadline : Int
    , usages : String
    , usagesLeft : String
    , verifications : String
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
        , ( "verifier_reward", Encode.string c.verifierReward )
        , ( "deadline", Encode.int c.deadline )
        , ( "usages", Encode.string c.usages )
        , ( "usages_left", Encode.string c.usagesLeft )
        , ( "verifications", Encode.string c.verifications )
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
    , logoUrl : String
    , name : String
    , description : String
    , inviterReward : Eos.Asset
    , invitedReward : Eos.Asset
    , minBalance : Eos.Asset
    , hasShop : Eos.EosBool
    , hasObjectives : Eos.EosBool
    , hasKyc : Eos.EosBool
    }


createCommunityData :
    { accountName : Eos.Name
    , symbol : Eos.Symbol
    , logoUrl : String
    , name : String
    , description : String
    , inviterReward : Float
    , invitedReward : Float
    , minBalance : Float
    , hasShop : Bool
    , hasObjectives : Bool
    , hasKyc : Bool
    }
    -> CreateCommunityData
createCommunityData params =
    { cmmAsset =
        { amount = 0
        , symbol = params.symbol
        }
    , creator = params.accountName
    , logoUrl = params.logoUrl
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
    , minBalance =
        { amount = params.minBalance
        , symbol = params.symbol
        }
    , hasShop = params.hasShop |> Eos.boolToEosBool
    , hasObjectives = params.hasObjectives |> Eos.boolToEosBool
    , hasKyc = params.hasKyc |> Eos.boolToEosBool
    }


encodeCreateCommunityData : CreateCommunityData -> Value
encodeCreateCommunityData c =
    Encode.object
        [ ( "cmm_asset", Eos.encodeAsset c.cmmAsset )
        , ( "creator", Eos.encodeName c.creator )
        , ( "logo", Encode.string c.logoUrl )
        , ( "name", Encode.string c.name )
        , ( "description", Encode.string c.description )
        , ( "inviter_reward", Eos.encodeAsset c.inviterReward )
        , ( "invited_reward", Eos.encodeAsset c.invitedReward )
        , ( "has_objectives", Eos.encodeEosBool c.hasObjectives )
        , ( "has_shop", Eos.encodeEosBool c.hasShop )
        , ( "has_kyc", Eos.encodeEosBool c.hasKyc )
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
    , hasObjectives : Int
    , hasShop : Int

    -- , hasKyc : Int
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
        , ( "has_objectives", Encode.int c.hasObjectives )
        , ( "has_shop", Encode.int c.hasShop )

        -- , ( "has_kyc", Encode.int c.hasKyc )
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
    List ClaimResponse


type alias ClaimResponse =
    { id : Int
    , createdAt : DateTime
    , checks : List CheckResponse
    , action : ActionResponse
    }


type alias CheckResponse =
    Bool


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


claimSelectionSet : String -> SelectionSet ClaimResponse Cambiatus.Object.Claim
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


checkSelectionSet : SelectionSet CheckResponse Cambiatus.Object.Check
checkSelectionSet =
    Check.isVerified


verificationActionSelectionSet : SelectionSet ActionResponse Cambiatus.Object.Action
verificationActionSelectionSet =
    SelectionSet.succeed ActionResponse
        |> with Action.id
        |> with Action.description
        |> with (Action.objective verificationObjectiveSelectionSet)


verificationObjectiveSelectionSet : SelectionSet ObjectiveResponse Cambiatus.Object.Objective
verificationObjectiveSelectionSet =
    SelectionSet.succeed ObjectiveResponse
        |> with Objective.id
        |> with (Objective.community verificationCommunitySelectionSet)


verificationCommunitySelectionSet : SelectionSet CommunityResponse Cambiatus.Object.Community
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
            actionVerificationResponse

        toStatus : List CheckResponse -> Tag.TagStatus
        toStatus checks =
            case List.head checks of
                Just check ->
                    if check then
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



-- INVITE


type alias Invite =
    { community : Model
    , creator : Profile
    }


inviteSelectionSet : SelectionSet Invite Cambiatus.Object.Invite
inviteSelectionSet =
    SelectionSet.succeed Invite
        |> with (Invite.community communitySelectionSet)
        |> with (Invite.creator Profile.selectionSet)


inviteQuery : String -> SelectionSet (Maybe Invite) RootQuery
inviteQuery invitationId =
    Query.invite { input = { id = Present invitationId } } inviteSelectionSet
