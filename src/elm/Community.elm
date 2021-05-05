module Community exposing
    ( ActionVerification
    , ActionVerificationsResponse
    , Balance
    , ClaimResponse
    , CommunityPreview
    , CreateCommunityData
    , Invite
    , Metadata
    , Model
    , Objective
    , Transaction
    , Verification(..)
    , Verifiers
    , WithObjectives
    , claimSelectionSet
    , communitiesQuery
    , communityPreviewImage
    , communityPreviewQuery
    , communityPreviewSelectionSet
    , communitySelectionSet
    , createCommunityData
    , createCommunityDataDecoder
    , decodeBalance
    , decodeTransaction
    , domainAvailableQuery
    , encodeCreateCommunityData
    , encodeCreateObjectiveAction
    , encodeUpdateData
    , encodeUpdateObjectiveAction
    , inviteQuery
    , logoBackground
    , logoUrl
    , newCommunitySubscription
    , objectiveSelectionSet
    , subdomainQuery
    , symbolQuery
    , toVerifications
    )

import Action exposing (Action)
import Cambiatus.Enum.VerificationType exposing (VerificationType(..))
import Cambiatus.Object
import Cambiatus.Object.Action as ActionObject
import Cambiatus.Object.Check as Check
import Cambiatus.Object.Claim as Claim exposing (ChecksOptionalArguments)
import Cambiatus.Object.Community as Community
import Cambiatus.Object.CommunityPreview as CommunityPreview
import Cambiatus.Object.Exists
import Cambiatus.Object.Invite as Invite
import Cambiatus.Object.Objective as Objective
import Cambiatus.Object.Subdomain as Subdomain
import Cambiatus.Object.User as Profile
import Cambiatus.Query as Query
import Cambiatus.Scalar exposing (DateTime(..))
import Cambiatus.Subscription as Subscription
import Eos exposing (EosBool(..), Symbol, symbolToString)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (class, classList, src)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Decode exposing (required)
import Json.Encode as Encode exposing (Value)
import Profile
import Session.Shared exposing (Shared)
import Time exposing (Posix)
import Token
import Utils
import View.Tag as Tag



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
    { name : String
    , description : String
    , symbol : Symbol
    , logo : String
    , subdomain : String
    , creator : Eos.Name
    , inviterReward : Float
    , invitedReward : Float
    , minBalance : Maybe Float
    , maxSupply : Maybe Float
    , tokenType : Maybe Token.TokenType
    , memberCount : Int
    , actionCount : Int
    , claimCount : Int
    , transferCount : Int
    , productCount : Int
    , orderCount : Int
    , members : List Profile.Minimal
    , objectives : List Objective
    , hasObjectives : Bool
    , hasShop : Bool
    , hasKyc : Bool
    , hasAutoInvite : Bool
    , validators : List Eos.Name
    , coverPhoto : Maybe String
    , website : Maybe String
    }



-- GraphQL


communitiesSelectionSet : SelectionSet Metadata Cambiatus.Object.Community
communitiesSelectionSet =
    SelectionSet.succeed Metadata
        |> with Community.name
        |> with Community.description
        |> with (Eos.symbolSelectionSet Community.symbol)
        |> with Community.logo
        |> with (Eos.nameSelectionSet Community.creator)
        |> with Community.memberCount


communitySelectionSet : SelectionSet Model Cambiatus.Object.Community
communitySelectionSet =
    SelectionSet.succeed Model
        |> with Community.name
        |> with Community.description
        |> with (Eos.symbolSelectionSet Community.symbol)
        |> with Community.logo
        |> with (Community.subdomain Subdomain.name |> SelectionSet.map (Maybe.withDefault ""))
        |> with (Eos.nameSelectionSet Community.creator)
        |> with Community.inviterReward
        |> with Community.invitedReward
        |> with Community.minBalance
        |> with Community.maxSupply
        |> with Token.tokenTypeSelectionSet
        |> with Community.memberCount
        |> with Community.actionCount
        |> with Community.claimCount
        |> with Community.transferCount
        |> with Community.productCount
        |> with Community.orderCount
        |> with (Community.members Profile.minimalSelectionSet)
        |> with (Community.objectives objectiveSelectionSet)
        |> with Community.hasObjectives
        |> with Community.hasShop
        |> with Community.hasKyc
        |> with Community.autoInvite
        |> with (Community.validators (Eos.nameSelectionSet Profile.account))
        -- TODO
        |> SelectionSet.hardcoded Nothing
        |> with Community.website



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


type alias WithObjectives =
    { metadata : Metadata
    , objectives : List Objective
    }


symbolQuery : Symbol -> SelectionSet (Maybe Model) RootQuery
symbolQuery symbol =
    Query.community (\optionals -> { optionals | symbol = Present <| symbolToString symbol }) communitySelectionSet


subdomainQuery : String -> SelectionSet (Maybe Model) RootQuery
subdomainQuery subdomain =
    Query.community (\optionals -> { optionals | subdomain = Present subdomain }) communitySelectionSet


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
    , isCompleted : Bool
    }


objectiveSelectionSet : SelectionSet Objective Cambiatus.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> with Objective.id
        |> with Objective.description
        |> with (Eos.nameSelectionSet Objective.creatorId)
        |> with (Objective.actions identity Action.selectionSet)
        |> with (Objective.community communitiesSelectionSet)
        |> with Objective.isCompleted


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


type Verification
    = Manually Verifiers
    | Automatically String


type alias Verifiers =
    { verifiers : List String
    , reward : Float
    }



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
    , subdomain : String
    , inviterReward : Eos.Asset
    , invitedReward : Eos.Asset
    , hasShop : Eos.EosBool
    , hasObjectives : Eos.EosBool
    , hasKyc : Eos.EosBool
    , hasAutoInvite : Eos.EosBool
    , website : String
    }


createCommunityData :
    { accountName : Eos.Name
    , symbol : Eos.Symbol
    , logoUrl : String
    , name : String
    , description : String
    , subdomain : String
    , inviterReward : Float
    , invitedReward : Float
    , hasShop : Bool
    , hasObjectives : Bool
    , hasKyc : Bool
    , hasAutoInvite : Bool
    , website : String
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
    , subdomain = params.subdomain
    , inviterReward =
        { amount = params.inviterReward
        , symbol = params.symbol
        }
    , invitedReward =
        { amount = params.invitedReward
        , symbol = params.symbol
        }
    , hasShop = params.hasShop |> Eos.boolToEosBool
    , hasObjectives = params.hasObjectives |> Eos.boolToEosBool
    , hasKyc = params.hasKyc |> Eos.boolToEosBool
    , hasAutoInvite = params.hasAutoInvite |> Eos.boolToEosBool
    , website = params.website
    }


encodeCreateCommunityData : CreateCommunityData -> Value
encodeCreateCommunityData c =
    Encode.object
        [ ( "cmm_asset", Eos.encodeAsset c.cmmAsset )
        , ( "creator", Eos.encodeName c.creator )
        , ( "logo", Encode.string c.logoUrl )
        , ( "name", Encode.string c.name )
        , ( "description", Encode.string c.description )
        , ( "subdomain", Encode.string c.subdomain )
        , ( "inviter_reward", Eos.encodeAsset c.inviterReward )
        , ( "invited_reward", Eos.encodeAsset c.invitedReward )
        , ( "has_objectives", Eos.encodeEosBool c.hasObjectives )
        , ( "has_shop", Eos.encodeEosBool c.hasShop )
        , ( "has_kyc", Eos.encodeEosBool c.hasKyc )
        , ( "auto_invite", Eos.encodeEosBool c.hasAutoInvite )
        , ( "website", Encode.string c.website )
        ]


createCommunityDataDecoder : Decoder CreateCommunityData
createCommunityDataDecoder =
    Decode.succeed CreateCommunityData
        |> required "cmm_asset" Eos.decodeAsset
        |> required "creator" Eos.nameDecoder
        |> required "logo" Decode.string
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "subdomain" Decode.string
        |> required "inviter_reward" Eos.decodeAsset
        |> required "invited_reward" Eos.decodeAsset
        |> required "has_objectives" Eos.eosBoolDecoder
        |> required "has_shop" Eos.eosBoolDecoder
        |> required "has_kyc" Eos.eosBoolDecoder
        |> required "auto_invite" Eos.eosBoolDecoder
        |> required "website" Decode.string


type alias UpdateCommunityData =
    { asset : Eos.Asset
    , logo : String
    , name : String
    , description : String
    , subdomain : String
    , inviterReward : Eos.Asset
    , invitedReward : Eos.Asset
    , hasObjectives : Eos.EosBool
    , hasShop : Eos.EosBool
    , hasKyc : Eos.EosBool
    , hasAutoInvite : Eos.EosBool
    , website : String
    }


encodeUpdateData : UpdateCommunityData -> Value
encodeUpdateData c =
    Encode.object
        [ ( "logo", Encode.string c.logo )
        , ( "cmm_asset", Eos.encodeAsset c.asset )
        , ( "name", Encode.string c.name )
        , ( "description", Encode.string c.description )
        , ( "subdomain", Encode.string c.subdomain )
        , ( "inviter_reward", Eos.encodeAsset c.inviterReward )
        , ( "invited_reward", Eos.encodeAsset c.invitedReward )
        , ( "has_objectives", Eos.encodeEosBool c.hasObjectives )
        , ( "has_shop", Eos.encodeEosBool c.hasShop )
        , ( "has_kyc", Eos.encodeEosBool c.hasKyc )
        , ( "auto_invite", Eos.encodeEosBool c.hasAutoInvite )
        , ( "website", Encode.string c.website )
        ]


domainAvailableSelectionSet : SelectionSet Bool Cambiatus.Object.Exists
domainAvailableSelectionSet =
    Cambiatus.Object.Exists.exists
        |> SelectionSet.map (Maybe.withDefault False)


domainAvailableQuery : String -> SelectionSet Bool RootQuery
domainAvailableQuery domain =
    Query.domainAvailable { domain = domain } domainAvailableSelectionSet
        |> SelectionSet.map (Maybe.map not >> Maybe.withDefault False)



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
        |> with ActionObject.id
        |> with ActionObject.description
        |> with (ActionObject.objective verificationObjectiveSelectionSet)


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
    { community : CommunityPreview
    , creator : Profile.Minimal
    }


inviteSelectionSet : SelectionSet Invite Cambiatus.Object.Invite
inviteSelectionSet =
    SelectionSet.succeed Invite
        |> with (Invite.communityPreview communityPreviewSelectionSet)
        |> with (Invite.creator Profile.minimalSelectionSet)


inviteQuery : String -> SelectionSet (Maybe Invite) RootQuery
inviteQuery invitationId =
    Query.invite { id = invitationId } inviteSelectionSet



-- PREVIEW


type alias CommunityPreview =
    { name : String
    , description : String
    , logo : String
    , symbol : Eos.Symbol
    , subdomain : String
    , hasShop : Bool
    , hasObjectives : Bool
    , hasKyc : Bool
    , hasAutoInvite : Bool
    , coverPhoto : Maybe String
    , memberCount : Int
    , website : Maybe String
    }


communityPreviewSelectionSet : SelectionSet CommunityPreview Cambiatus.Object.CommunityPreview
communityPreviewSelectionSet =
    SelectionSet.succeed CommunityPreview
        |> with CommunityPreview.name
        |> with CommunityPreview.description
        |> with CommunityPreview.logo
        |> with (Eos.symbolSelectionSet CommunityPreview.symbol)
        |> with (CommunityPreview.subdomain Subdomain.name |> SelectionSet.map (Maybe.withDefault ""))
        |> with CommunityPreview.hasShop
        |> with CommunityPreview.hasObjectives
        |> with CommunityPreview.hasKyc
        |> with CommunityPreview.autoInvite
        -- TODO
        |> SelectionSet.hardcoded Nothing
        |> with CommunityPreview.memberCount
        |> with CommunityPreview.website


communityPreviewQuery : String -> SelectionSet (Maybe CommunityPreview) RootQuery
communityPreviewQuery subdomain =
    Query.communityPreview (\optionals -> { optionals | subdomain = Present subdomain })
        communityPreviewSelectionSet


defaultCommunityCoverPhoto : String
defaultCommunityCoverPhoto =
    "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/832a40918a9042c994b92ecde4e49705"


communityPreviewImage :
    Bool
    -> Shared
    -> { community | name : String, coverPhoto : Maybe String, memberCount : Int }
    -> Html msg
communityPreviewImage isLeftSide { translators } community =
    div
        [ class "relative"
        , classList
            [ ( "md:hidden w-full", not isLeftSide )
            , ( "w-1/2 flex-grow hidden md:block", isLeftSide )
            ]
        ]
        [ div [ class "bg-black" ]
            [ img
                [ class "w-full opacity-60"
                , classList
                    [ ( "h-screen object-cover", isLeftSide )
                    , ( "max-h-108", not isLeftSide )
                    ]
                , src (Maybe.withDefault defaultCommunityCoverPhoto community.coverPhoto)
                ]
                []
            ]
        , div [ class "absolute inset-0 flex flex-col items-center justify-center text-white uppercase px-5" ]
            [ span [ class "font-medium text-xl" ] [ text community.name ]
            , span [ class "text-xs mt-2" ]
                [ text
                    (translators.tr "community.join.member_count"
                        [ ( "member_count", String.fromInt community.memberCount ) ]
                    )
                ]
            ]
        ]
