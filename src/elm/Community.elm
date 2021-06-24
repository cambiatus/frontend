module Community exposing
    ( Balance
    , CommunityPreview
    , CreateCommunityData
    , Invite
    , Metadata
    , Model
    , Objective
    , addPhotosMutation
    , communityPreviewImage
    , communityPreviewQuery
    , communityPreviewSymbolQuery
    , createCommunityData
    , createCommunityDataDecoder
    , decodeBalance
    , domainAvailableQuery
    , encodeCreateCommunityData
    , encodeCreateObjectiveAction
    , encodeUpdateData
    , encodeUpdateObjectiveAction
    , inviteQuery
    , isNonExistingCommunityError
    , logoBackground
    , newCommunitySubscription
    , subdomainQuery
    , symbolQuery
    )

import Action exposing (Action)
import Cambiatus.Mutation as Mutation
import Cambiatus.Object
import Cambiatus.Object.Community as Community
import Cambiatus.Object.CommunityPreview as CommunityPreview
import Cambiatus.Object.Exists
import Cambiatus.Object.Invite as Invite
import Cambiatus.Object.Objective as Objective
import Cambiatus.Object.Subdomain as Subdomain
import Cambiatus.Object.Upload as Upload
import Cambiatus.Object.User as Profile
import Cambiatus.Query as Query
import Cambiatus.Scalar exposing (DateTime(..))
import Cambiatus.Subscription as Subscription
import Eos
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (class, classList, src)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Profile
import Session.Shared exposing (Shared)
import Time exposing (Posix)
import Utils



-- METADATA
-- Used on community listing


type alias Metadata =
    { title : String
    , description : String
    , symbol : Eos.Symbol
    , logo : String
    , creator : Eos.Name
    , memberCount : Int
    }



-- Community Data


type alias Model =
    { name : String
    , description : String
    , symbol : Eos.Symbol
    , logo : String
    , subdomain : String
    , creator : Eos.Name
    , inviterReward : Float
    , invitedReward : Float
    , minBalance : Maybe Float
    , maxSupply : Maybe Float
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
    , uploads : List String
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
        |> with (Community.uploads Upload.url)
        |> with Community.website



-- NEW COMMUNITY NAME


type alias NewCommunity =
    String


newCommunitySubscription : Eos.Symbol -> SelectionSet NewCommunity RootSubscription
newCommunitySubscription symbol =
    let
        stringSymbol =
            Eos.symbolToString symbol
                |> String.toUpper

        selectionSet =
            Community.name

        args =
            { input = { symbol = stringSymbol } }
    in
    Subscription.newcommunity args selectionSet


symbolQuery : Eos.Symbol -> SelectionSet (Maybe Model) RootQuery
symbolQuery symbol =
    Query.community (\optionals -> { optionals | symbol = Present <| Eos.symbolToString symbol }) communitySelectionSet


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


addPhotosMutation : Eos.Symbol -> List String -> SelectionSet (Maybe Model) RootMutation
addPhotosMutation symbol photos =
    Mutation.addCommunityPhotos { symbol = Eos.symbolToString symbol, urls = photos }
        communitySelectionSet



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
        , ( "inviter_reward", Eos.encodeAsset c.inviterReward )
        , ( "invited_reward", Eos.encodeAsset c.invitedReward )
        , ( "has_objectives", Eos.encodeEosBool c.hasObjectives )
        , ( "has_shop", Eos.encodeEosBool c.hasShop )
        , ( "has_kyc", Eos.encodeEosBool c.hasKyc )
        , ( "auto_invite", Eos.encodeEosBool c.hasAutoInvite )
        , ( "subdomain", Encode.string c.subdomain )
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
    , uploads : List String
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
        |> with (CommunityPreview.uploads Upload.url)
        |> with CommunityPreview.memberCount
        |> with CommunityPreview.website


communityPreviewQuery : String -> SelectionSet (Maybe CommunityPreview) RootQuery
communityPreviewQuery subdomain =
    Query.communityPreview (\optionals -> { optionals | subdomain = Present subdomain })
        communityPreviewSelectionSet


communityPreviewSymbolQuery : Eos.Symbol -> SelectionSet (Maybe CommunityPreview) RootQuery
communityPreviewSymbolQuery symbol =
    Query.communityPreview (\optionals -> { optionals | symbol = Present (Eos.symbolToString symbol) })
        communityPreviewSelectionSet


communityPreviewImage :
    Bool
    -> Shared
    -> { community | name : String, uploads : List String, memberCount : Int }
    -> Html msg
communityPreviewImage isLeftSide { translators } community =
    let
        defaultImage =
            if isLeftSide then
                "/images/community-bg-desktop.svg"

            else
                "/images/community-bg-mobile.svg"
    in
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
                , src
                    (List.head community.uploads
                        |> Maybe.withDefault defaultImage
                    )
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


isNonExistingCommunityError : Graphql.Http.Error community -> Bool
isNonExistingCommunityError error =
    Utils.errorToString error
        |> String.toLower
        |> String.contains "no community found using the domain"
