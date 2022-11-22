module Community exposing
    ( Balance
    , CommunityPreview
    , Contribution
    , ContributionConfiguration
    , CreateCommunityData
    , CreateCommunityDataInput
    , Field(..)
    , FieldError(..)
    , FieldValue(..)
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
    , currencyTranslationKey
    , domainAvailableQuery
    , encodeCreateCommunityData
    , encodeCreateObjectiveAction
    , encodeUpdateData
    , encodeUpdateObjectiveAction
    , fieldSelectionSet
    , fieldsSelectionSet
    , getField
    , inviteQuery
    , isFieldLoading
    , maybeFieldValue
    , mergeFields
    , newCommunitySubscription
    , setFieldAsLoading
    , setFieldValue
    , subdomainQuery
    , symbolQuery
    )

import Action exposing (Action)
import Cambiatus.Enum.ContributionStatusType
import Cambiatus.Enum.CurrencyType
import Cambiatus.Mutation as Mutation
import Cambiatus.Object
import Cambiatus.Object.Community as Community
import Cambiatus.Object.CommunityPreview as CommunityPreview
import Cambiatus.Object.Contribution
import Cambiatus.Object.ContributionConfig
import Cambiatus.Object.Exists
import Cambiatus.Object.Invite as Invite
import Cambiatus.Object.Objective as Objective
import Cambiatus.Object.Subdomain as Subdomain
import Cambiatus.Object.Upload as Upload
import Cambiatus.Object.User as Profile
import Cambiatus.Query as Query
import Cambiatus.Scalar
import Cambiatus.Subscription as Subscription
import Community.News
import Constants
import Contact
import Eos
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (class, classList, src)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Markdown exposing (Markdown)
import Profile
import RemoteData exposing (RemoteData)
import Session.Shared exposing (Shared)
import Shop.Category
import Time exposing (Posix)



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
    , description : Markdown
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
    , contributions : RemoteData (Graphql.Http.Error (List Contribution)) (List Contribution)
    , contributionConfiguration : Maybe ContributionConfiguration
    , news : RemoteData (Graphql.Http.Error (List Community.News.Model)) (List Community.News.Model)
    , highlightedNews : Maybe Community.News.Model
    , objectives : RemoteData (Graphql.Http.Error (List Objective)) (List Objective)
    , hasObjectives : Bool
    , hasShop : Bool
    , hasKyc : Bool
    , hasAutoInvite : Bool
    , hasNews : Bool
    , validators : List Eos.Name
    , uploads : RemoteData (Graphql.Http.Error (List String)) (List String)
    , website : Maybe String
    , contacts : List Contact.Valid
    , shopCategories : RemoteData (Graphql.Http.Error (List Shop.Category.Tree)) (List Shop.Category.Tree)
    }


{-| In order to be able to query for fields separately from the community (such
as objectives and uploads), we need to add a `Field` constructor to represent
that field. The constructor's name should be the name of the field followed by
`Field` (e.g. `ObjectivesField`).

In order to have a nice API, we also need types to wrap the success and error
cases. That's why we have `FieldValue` and `FieldError`.

If you want to add a new field that isn't loaded by default (usually Lists are
good candidates):

1.  Add the field to the `Model`. It's type should be in the format
    `RemoteData (Graphql.Http.Error field) field`
2.  Add a new constructor to this `Field` type following the practices described above
3.  Add a new constructor to `FieldValue` (see `FieldValue`'s documentation for
    more info)
4.  Fill in the functions that use `Field` and `FieldValue` (the compiler will
    let you know which ones)

Pages can use this type to request separate fields from `LoggedIn`, using
`LoggedIn.External` messages, specifying which field they want. There are two
variants they can use to do so:

1.  `RequestedCommunityField`: Checks if the field is loaded. If so, send a
    `BroadcastMsg` with the field. If not, queries the backend for that field,
    and once the result comes in, sends a `BroadcastMsg`
2.  `RequestedReloadCommunityField`: Always queries for the field, and sends the
    result as a `BroadcastMsg`

-}
type Field
    = ContributionsField
    | ObjectivesField
    | UploadsField
    | MembersField
    | NewsField
    | ShopCategoriesField


{-| `FieldValue` is useful to wrap results of queries for fields that aren't
loaded in by default with the community (such as objectives and uploads). The
constructor's name should be the name of the field followed by `Value`, and
should hold the actual value of that field (e.g. `ObjectivesValue (List Objetive)`).
-}
type FieldValue
    = ContributionsValue (List Contribution)
    | ObjectivesValue (List Objective)
    | UploadsValue (List String)
    | MembersValue (List Profile.Minimal)
    | NewsValue (List Community.News.Model)
    | ShopCategories (List Shop.Category.Tree)


{-| When we want to extract a field that is not loaded by default with the
community, and there is an error, we need to know if it was an error when
fetching the community or when fetching the actual field.
-}
type FieldError a
    = CommunityError (Graphql.Http.Error (Maybe Model))
    | FieldError a


getField :
    RemoteData (Graphql.Http.Error (Maybe Model)) Model
    -> (Model -> RemoteData err field)
    -> RemoteData (FieldError err) ( Model, field )
getField remoteDataModel accessor =
    remoteDataModel
        |> RemoteData.mapError CommunityError
        |> RemoteData.andThen
            (\model ->
                accessor model
                    |> RemoteData.map (\field -> ( model, field ))
                    |> RemoteData.mapError FieldError
            )


setFieldValue : FieldValue -> Model -> Model
setFieldValue fieldValue model =
    case fieldValue of
        ContributionsValue contributions ->
            { model | contributions = RemoteData.Success contributions }

        ObjectivesValue objectives ->
            { model | objectives = RemoteData.Success objectives }

        UploadsValue uploads ->
            { model | uploads = RemoteData.Success uploads }

        MembersValue members ->
            { model | members = members }

        NewsValue news ->
            { model | news = RemoteData.Success news }

        ShopCategories categories ->
            { model | shopCategories = RemoteData.Success categories }


setFieldAsLoading : Field -> Model -> Model
setFieldAsLoading field model =
    case field of
        ContributionsField ->
            { model | contributions = RemoteData.Loading }

        ObjectivesField ->
            { model | objectives = RemoteData.Loading }

        UploadsField ->
            { model | uploads = RemoteData.Loading }

        MembersField ->
            model

        NewsField ->
            { model | news = RemoteData.Loading }

        ShopCategoriesField ->
            { model | shopCategories = RemoteData.Loading }


isFieldLoading : Field -> Model -> Bool
isFieldLoading field model =
    case field of
        ContributionsField ->
            RemoteData.isLoading model.contributions

        ObjectivesField ->
            RemoteData.isLoading model.objectives

        UploadsField ->
            RemoteData.isLoading model.uploads

        MembersField ->
            False

        NewsField ->
            RemoteData.isLoading model.news

        ShopCategoriesField ->
            RemoteData.isLoading model.shopCategories


maybeFieldValue : Field -> Model -> Maybe FieldValue
maybeFieldValue field model =
    case field of
        ContributionsField ->
            model.contributions
                |> RemoteData.toMaybe
                |> Maybe.map ContributionsValue

        ObjectivesField ->
            model.objectives
                |> RemoteData.toMaybe
                |> Maybe.map ObjectivesValue

        UploadsField ->
            model.uploads
                |> RemoteData.toMaybe
                |> Maybe.map UploadsValue

        MembersField ->
            Just (MembersValue model.members)

        NewsField ->
            model.news
                |> RemoteData.toMaybe
                |> Maybe.map NewsValue

        ShopCategoriesField ->
            model.shopCategories
                |> RemoteData.toMaybe
                |> Maybe.map ShopCategories


mergeFields : RemoteData x Model -> Model -> Model
mergeFields loadedCommunity newCommunity =
    case loadedCommunity of
        RemoteData.Success oldCommunity ->
            { newCommunity
                | objectives = oldCommunity.objectives
                , uploads = oldCommunity.uploads
            }

        _ ->
            newCommunity



-- GraphQL


communitiesSelectionSet : SelectionSet Metadata Cambiatus.Object.Community
communitiesSelectionSet =
    SelectionSet.succeed Metadata
        |> with Community.name
        |> with Community.description
        |> with (Eos.symbolSelectionSet Community.symbol)
        |> with (SelectionSet.withDefault Constants.defaultCommunityLogo Community.logo)
        |> with (Eos.nameSelectionSet Community.creator)
        |> with Community.memberCount


communitySelectionSet : SelectionSet Model Cambiatus.Object.Community
communitySelectionSet =
    SelectionSet.succeed Model
        |> with Community.name
        |> with (Markdown.selectionSet Community.description)
        |> with (Eos.symbolSelectionSet Community.symbol)
        |> with (SelectionSet.withDefault Constants.defaultCommunityLogo Community.logo)
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
        |> SelectionSet.hardcoded RemoteData.NotAsked
        |> with (Community.contributionConfiguration contributionConfigurationSelectionSet)
        |> SelectionSet.hardcoded RemoteData.NotAsked
        |> with (Community.highlightedNews Community.News.selectionSet)
        |> SelectionSet.hardcoded RemoteData.NotAsked
        |> with Community.hasObjectives
        |> with Community.hasShop
        |> with Community.hasKyc
        |> with Community.autoInvite
        |> with Community.hasNews
        |> with (Community.validators (Eos.nameSelectionSet Profile.account))
        |> SelectionSet.hardcoded RemoteData.NotAsked
        |> with Community.website
        |> with
            (Community.contacts Contact.selectionSet
                |> SelectionSet.map (List.filterMap identity)
            )
        |> SelectionSet.hardcoded RemoteData.NotAsked



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


selectionSetForField : Field -> SelectionSet FieldValue Cambiatus.Object.Community
selectionSetForField field =
    case field of
        ContributionsField ->
            Community.contributions
                (\optionals -> { optionals | status = Present Cambiatus.Enum.ContributionStatusType.Created })
                contributionSelectionSet
                |> SelectionSet.map ContributionsValue

        ObjectivesField ->
            Community.objectives objectiveSelectionSet
                |> SelectionSet.map ObjectivesValue

        UploadsField ->
            Community.uploads Upload.url
                |> SelectionSet.map UploadsValue

        MembersField ->
            Community.members Profile.minimalSelectionSet
                |> SelectionSet.map MembersValue

        NewsField ->
            Community.news Community.News.selectionSet
                |> SelectionSet.map NewsValue

        ShopCategoriesField ->
            Shop.Category.treesSelectionSet Community.categories
                |> SelectionSet.map ShopCategories


fieldSelectionSet : Eos.Symbol -> Field -> SelectionSet (Maybe FieldValue) RootQuery
fieldSelectionSet symbol field =
    field
        |> selectionSetForField
        |> Query.community (\optionals -> { optionals | symbol = Present <| Eos.symbolToString symbol })


fieldsSelectionSet : Eos.Symbol -> List Field -> SelectionSet (List FieldValue) RootQuery
fieldsSelectionSet symbol fields =
    fields
        |> List.Extra.unique
        |> List.map selectionSetForField
        |> SelectionSet.list
        |> Query.community (\optionals -> { optionals | symbol = Present <| Eos.symbolToString symbol })
        |> SelectionSet.withDefault []


symbolQuery : Eos.Symbol -> SelectionSet (Maybe Model) RootQuery
symbolQuery symbol =
    Query.community (\optionals -> { optionals | symbol = Present <| Eos.symbolToString symbol }) communitySelectionSet


subdomainQuery : String -> SelectionSet (Maybe Model) RootQuery
subdomainQuery subdomain =
    Query.community (\optionals -> { optionals | subdomain = Present subdomain }) communitySelectionSet


addPhotosMutation : Eos.Symbol -> List String -> SelectionSet (Maybe Model) RootMutation
addPhotosMutation symbol photos =
    Mutation.addCommunityPhotos { symbol = Eos.symbolToString symbol, urls = photos }
        communitySelectionSet



-- OBJECTIVE


type alias Objective =
    { id : Action.ObjectiveId
    , description : Markdown
    , creator : Eos.Name
    , actions : List Action
    , community : Metadata
    , isCompleted : Bool
    }


objectiveSelectionSet : SelectionSet Objective Cambiatus.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> with Action.objectiveIdSelectionSet
        |> with (Markdown.selectionSet Objective.description)
        |> with (Eos.nameSelectionSet Objective.creatorId)
        |> with (Objective.actions identity Action.selectionSet)
        |> with (Objective.community communitiesSelectionSet)
        |> with Objective.isCompleted


type alias CreateObjectiveAction =
    { communityId : Eos.Symbol
    , description : Markdown
    , creator : Eos.Name
    }


encodeCreateObjectiveAction : CreateObjectiveAction -> Value
encodeCreateObjectiveAction c =
    Encode.object
        [ ( "community_id", Eos.encodeSymbol c.communityId )
        , ( "objective_id", Encode.int 0 )
        , ( "description", Markdown.encode c.description )
        , ( "editor", Eos.encodeName c.creator )
        ]


type alias UpdateObjectiveAction =
    { communityId : Eos.Symbol
    , objectiveId : Action.ObjectiveId
    , description : Markdown
    , editor : Eos.Name
    }


encodeUpdateObjectiveAction : UpdateObjectiveAction -> Value
encodeUpdateObjectiveAction c =
    Encode.object
        [ ( "community_id", Eos.encodeSymbol c.communityId )
        , ( "objective_id", Action.encodeObjectiveId c.objectiveId )
        , ( "description", Markdown.encode c.description )
        , ( "editor", Eos.encodeName c.editor )
        ]



-- CONTRIBUTION


type alias Contribution =
    { user : Profile.Minimal
    , amount : Float
    , currency : Cambiatus.Enum.CurrencyType.CurrencyType
    , id : String
    , insertedAt : Posix
    }


type alias ContributionConfiguration =
    { acceptedCurrencies : List Cambiatus.Enum.CurrencyType.CurrencyType
    , paypalAccount : Maybe String
    , thankYouDescription : Maybe Markdown
    , thankYouTitle : Maybe String
    }


contributionSelectionSet : SelectionSet Contribution Cambiatus.Object.Contribution
contributionSelectionSet =
    SelectionSet.succeed Contribution
        |> with (Cambiatus.Object.Contribution.user Profile.minimalSelectionSet)
        |> with Cambiatus.Object.Contribution.amount
        |> with Cambiatus.Object.Contribution.currency
        |> with Cambiatus.Object.Contribution.id
        |> with
            (Cambiatus.Object.Contribution.insertedAt
                |> SelectionSet.map
                    (\(Cambiatus.Scalar.NaiveDateTime naiveDateTime) ->
                        Iso8601.toTime naiveDateTime
                            |> Result.withDefault (Time.millisToPosix 0)
                    )
            )


contributionConfigurationSelectionSet : SelectionSet ContributionConfiguration Cambiatus.Object.ContributionConfig
contributionConfigurationSelectionSet =
    SelectionSet.succeed ContributionConfiguration
        |> with Cambiatus.Object.ContributionConfig.acceptedCurrencies
        |> with Cambiatus.Object.ContributionConfig.paypalAccount
        |> with (Markdown.maybeSelectionSet Cambiatus.Object.ContributionConfig.thankYouDescription)
        |> with Cambiatus.Object.ContributionConfig.thankYouTitle


currencyTranslationKey : { contribution | amount : Float, currency : Cambiatus.Enum.CurrencyType.CurrencyType } -> String
currencyTranslationKey { amount, currency } =
    let
        baseTranslation =
            case currency of
                Cambiatus.Enum.CurrencyType.Brl ->
                    "currency.brl"

                Cambiatus.Enum.CurrencyType.Btc ->
                    "currency.btc"

                Cambiatus.Enum.CurrencyType.Crc ->
                    "currency.crc"

                Cambiatus.Enum.CurrencyType.Eos ->
                    "currency.eos"

                Cambiatus.Enum.CurrencyType.Eth ->
                    "currency.eth"

                Cambiatus.Enum.CurrencyType.Usd ->
                    "currency.usd"
    in
    if amount == 1 then
        baseTranslation ++ "_singular"

    else
        baseTranslation ++ "_plural"



-- Balance


type alias Balance =
    { asset : Eos.Asset
    , lastActivity : Posix
    }



-- CREATE COMMUNITY


type alias CreateCommunityData =
    { cmmAsset : Eos.Asset
    , creator : Eos.Name
    , logoUrl : String
    , name : String
    , description : Markdown
    , subdomain : String
    , inviterReward : Eos.Asset
    , invitedReward : Eos.Asset
    , hasShop : Eos.EosBool
    , hasObjectives : Eos.EosBool
    , hasKyc : Eos.EosBool
    , hasAutoInvite : Eos.EosBool
    , website : String
    }


type alias CreateCommunityDataInput =
    { accountName : Eos.Name
    , symbol : Eos.Symbol
    , logoUrl : String
    , name : String
    , description : Markdown
    , subdomain : String
    , inviterReward : Float
    , invitedReward : Float
    , hasShop : Bool
    , hasObjectives : Bool
    , hasKyc : Bool
    , hasAutoInvite : Bool
    , website : String
    }


createCommunityData : CreateCommunityDataInput -> CreateCommunityData
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
        , ( "description", Markdown.encode c.description )
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
        |> required "description" Markdown.decoder
        |> required "subdomain" Decode.string
        |> required "inviter_reward" Eos.decodeAsset
        |> required "invited_reward" Eos.decodeAsset
        |> required "has_shop" Eos.eosBoolDecoder
        |> required "has_objectives" Eos.eosBoolDecoder
        |> required "has_kyc" Eos.eosBoolDecoder
        |> required "auto_invite" Eos.eosBoolDecoder
        |> required "website" Decode.string


type alias UpdateCommunityData =
    { asset : Eos.Asset
    , logo : String
    , name : String
    , description : Markdown
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
        , ( "description", Markdown.encode c.description )
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
    , description : Markdown
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
        |> with (Markdown.selectionSet CommunityPreview.description)
        |> with (SelectionSet.withDefault Constants.defaultCommunityLogo CommunityPreview.logo)
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
                    , ( "min-h-25 max-h-108", not isLeftSide )
                    ]
                , src
                    (List.head community.uploads
                        |> Maybe.withDefault defaultImage
                    )
                ]
                []
            ]
        , div [ class "absolute inset-0 flex flex-col items-center justify-center text-white uppercase px-5" ]
            [ span [ class "font-bold text-lg" ] [ text community.name ]
            , span [ class "font-bold text-sm mt-4" ]
                [ text
                    (translators.tr "community.join.member_count"
                        [ ( "member_count", String.fromInt community.memberCount ) ]
                    )
                ]
            ]
        ]
