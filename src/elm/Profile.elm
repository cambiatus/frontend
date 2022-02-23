module Profile exposing
    ( Basic
    , CommunityInfo
    , Contribution
    , DeleteKycAndAddressResult
    , Minimal
    , Model
    , ProfileForm
    , contributionCountQuery
    , contributionsQuery
    , deleteKycAndAddressMutation
    , minimalSelectionSet
    , mutation
    , profileToForm
    , query
    , selectionSet
    , upsertKycMutation
    , userContactSelectionSet
    , viewEmpty
    , viewProfileName
    , viewProfileNameTag
    )

import Avatar exposing (Avatar)
import Cambiatus.Enum.ContributionStatusType
import Cambiatus.Enum.CurrencyType
import Cambiatus.Enum.Permission exposing (Permission)
import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.Community as Community
import Cambiatus.Object.Contribution
import Cambiatus.Object.DeleteStatus
import Cambiatus.Object.Role
import Cambiatus.Object.Subdomain as Subdomain
import Cambiatus.Object.User as User
import Cambiatus.Query
import Cambiatus.Scalar exposing (DateTime(..), Id(..))
import Dict exposing (Dict)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Iso8601
import Kyc exposing (ProfileKyc)
import Markdown exposing (Markdown)
import Profile.Address as Address exposing (Address)
import Profile.Contact as Contact
import Time
import Translation


type alias Basic a =
    { a
        | name : Maybe String
        , account : Eos.Name
        , avatar : Avatar
        , email : Maybe String
        , bio : Maybe Markdown
        , contacts : List Contact.Normalized
    }


type alias Minimal =
    { name : Maybe String
    , account : Eos.Name
    , avatar : Avatar
    , email : Maybe String
    , bio : Maybe Markdown
    , contacts : List Contact.Normalized
    }


type alias Role =
    { color : Maybe String
    , name : String
    , permissions : List Permission
    }


type alias Model =
    { name : Maybe String
    , account : Eos.Name
    , avatar : Avatar
    , email : Maybe String
    , bio : Maybe Markdown
    , localization : Maybe String
    , contacts : List Contact.Normalized
    , interests : List String
    , communities : List CommunityInfo
    , roles : List Role
    , analysisCount : Int
    , kyc : Maybe ProfileKyc
    , address : Maybe Address
    }


type alias CommunityInfo =
    { symbol : Symbol
    , name : String
    , logo : String
    , subdomain : String
    , hasShop : Bool
    , hasActions : Bool
    , hasKyc : Bool
    }


userContactSelectionSet : SelectionSet (List Contact.Normalized) Cambiatus.Object.User
userContactSelectionSet =
    User.contacts Contact.selectionSet
        |> SelectionSet.map (List.filterMap identity)


roleSelectionSet : SelectionSet Role Cambiatus.Object.Role
roleSelectionSet =
    SelectionSet.succeed Role
        |> with Cambiatus.Object.Role.color
        |> with Cambiatus.Object.Role.name
        |> with Cambiatus.Object.Role.permissions


selectionSet : SelectionSet Model Cambiatus.Object.User
selectionSet =
    SelectionSet.succeed Model
        |> with User.name
        |> with (Eos.nameSelectionSet User.account)
        |> with (Avatar.selectionSet User.avatar)
        |> with User.email
        |> with (Markdown.maybeSelectionSet User.bio)
        |> with User.location
        |> with userContactSelectionSet
        |> with
            (User.interests
                |> SelectionSet.map
                    (\maybeInterests ->
                        Maybe.map (String.split ",") maybeInterests |> Maybe.withDefault []
                    )
            )
        |> with (User.communities communityInfoSelectionSet)
        |> with (User.roles roleSelectionSet)
        |> with User.analysisCount
        |> with (User.kyc Kyc.selectionSet)
        |> with (User.address Address.selectionSet)


minimalSelectionSet : SelectionSet Minimal Cambiatus.Object.User
minimalSelectionSet =
    SelectionSet.succeed Minimal
        |> with User.name
        |> with (Eos.nameSelectionSet User.account)
        |> with (Avatar.selectionSet User.avatar)
        |> with User.email
        |> with (Markdown.maybeSelectionSet User.bio)
        |> with userContactSelectionSet


communityInfoSelectionSet : SelectionSet CommunityInfo Cambiatus.Object.Community
communityInfoSelectionSet =
    SelectionSet.succeed CommunityInfo
        |> with (Eos.symbolSelectionSet Community.symbol)
        |> with Community.name
        |> with Community.logo
        |> with (Community.subdomain Subdomain.name |> SelectionSet.map (Maybe.withDefault ""))
        |> with Community.hasShop
        |> with Community.hasObjectives
        |> with Community.hasKyc


query : Eos.Name -> SelectionSet (Maybe Model) RootQuery
query account =
    let
        nameString =
            Eos.nameToString account
    in
    Cambiatus.Query.user
        { account = nameString }
        selectionSet


mutation : ProfileForm -> SelectionSet (Maybe Model) RootMutation
mutation form =
    let
        interestString =
            form.interest
                :: form.interests
                |> List.filter ((/=) "")
                |> String.join ","

        avatarInput =
            Maybe.map Present form.avatar
                |> Maybe.withDefault Absent

        contactInput { contactType, contact } =
            { type_ = Present contactType, externalId = Present contact }
    in
    Cambiatus.Mutation.updateUser
        { input =
            { name = Present form.name
            , email = Present form.email
            , claimNotification = Absent
            , bio = Present (Markdown.toRawString form.bio)
            , contacts = Present (List.map (Contact.unwrap >> contactInput) form.contacts)
            , digest = Absent
            , interests = Present interestString
            , location = Present form.localization
            , avatar = avatarInput
            , transferNotification = Absent
            }
        }
        selectionSet



-- CONTRIBUTION


type alias Contribution =
    { amount : Float
    , insertedAt : Time.Posix
    , currency : Cambiatus.Enum.CurrencyType.CurrencyType
    , status : Cambiatus.Enum.ContributionStatusType.ContributionStatusType
    }


contributionSelectionSet : Symbol -> SelectionSet (List Contribution) Cambiatus.Object.User
contributionSelectionSet symbol =
    let
        selectionSet_ =
            SelectionSet.succeed Contribution
                |> with Cambiatus.Object.Contribution.amount
                |> with
                    (Cambiatus.Object.Contribution.insertedAt
                        |> SelectionSet.map
                            (\(Cambiatus.Scalar.NaiveDateTime naiveDateTime) ->
                                Iso8601.toTime naiveDateTime
                                    |> Result.withDefault (Time.millisToPosix 0)
                            )
                    )
                |> with Cambiatus.Object.Contribution.currency
                |> with Cambiatus.Object.Contribution.status
    in
    User.contributions
        (\optionals -> { optionals | communityId = Present (Eos.symbolToString symbol) })
        selectionSet_


contributionsQuery : Symbol -> Eos.Name -> SelectionSet (Maybe (List Contribution)) RootQuery
contributionsQuery symbol account =
    Cambiatus.Query.user { account = Eos.nameToString account }
        (contributionSelectionSet symbol)


contributionCountSelectionSet : Symbol -> SelectionSet Int Cambiatus.Object.User
contributionCountSelectionSet symbol =
    User.contributionCount
        (\optionals -> { optionals | communityId = Present (Eos.symbolToString symbol) })


contributionCountQuery : Symbol -> Eos.Name -> SelectionSet (Maybe Int) RootQuery
contributionCountQuery symbol account =
    Cambiatus.Query.user
        { account = Eos.nameToString account }
        (contributionCountSelectionSet symbol)



-- UPDATE/INSERT KYC


upsertKycMutation : ProfileKyc -> SelectionSet (Maybe ProfileKyc) RootMutation
upsertKycMutation data =
    Cambiatus.Mutation.upsertKyc
        { input =
            { countryId = Id "1"
            , documentType = data.documentType
            , document = data.document
            , phone = data.phone
            , userType = "natural"
            }
        }
        Kyc.selectionSet



-- DELETE KYC/ADDRESS


type alias DeleteKycResult =
    { result : String
    , status : String
    }


deleteKycMutation : Eos.Name -> SelectionSet (Maybe DeleteKycResult) RootMutation
deleteKycMutation _ =
    Cambiatus.Mutation.deleteKyc
        (SelectionSet.succeed DeleteKycResult
            |> with Cambiatus.Object.DeleteStatus.status
            |> with Cambiatus.Object.DeleteStatus.reason
        )


type alias DeleteAddressResult =
    { result : String
    , status : String
    }


deleteAddressMutation : Eos.Name -> SelectionSet (Maybe DeleteAddressResult) RootMutation
deleteAddressMutation _ =
    Cambiatus.Mutation.deleteAddress
        (SelectionSet.succeed DeleteAddressResult
            |> with Cambiatus.Object.DeleteStatus.status
            |> with Cambiatus.Object.DeleteStatus.reason
        )


type alias DeleteKycAndAddressResult =
    { deleteKyc : Maybe DeleteKycResult
    , deleteAddress : Maybe DeleteAddressResult
    }


deleteKycAndAddressMutation : Eos.Name -> SelectionSet DeleteKycAndAddressResult RootMutation
deleteKycAndAddressMutation accountName =
    SelectionSet.map2 DeleteKycAndAddressResult
        (deleteKycMutation accountName)
        (deleteAddressMutation accountName)



-- Profile Login


type alias ProfileForm =
    { name : String
    , email : String
    , bio : Markdown
    , localization : String
    , avatar : Maybe String
    , contacts : List Contact.Normalized
    , interest : String
    , interests : List String
    , errors : Dict String String
    }


profileToForm : Model -> ProfileForm
profileToForm { name, email, bio, localization, avatar, interests, contacts } =
    { name = Maybe.withDefault "" name
    , email = Maybe.withDefault "" email
    , bio = Maybe.withDefault Markdown.empty bio
    , localization = Maybe.withDefault "" localization
    , avatar = Avatar.toMaybeString avatar
    , contacts = contacts
    , interest = ""
    , interests = interests
    , errors = Dict.empty
    }



-- View profile


viewProfileNameTag : Translation.Translators -> Eos.Name -> { profile | account : Eos.Name, name : Maybe String } -> Html msg
viewProfileNameTag translators loggedInAccount profile =
    p [ class "py-1 px-3 rounded-label uppercase font-bold text-white bg-black text-xs text-center" ]
        [ viewProfileName translators loggedInAccount profile ]


viewProfileName : Translation.Translators -> Eos.Name -> { profile | account : Eos.Name, name : Maybe String } -> Html msg
viewProfileName translators loggedInAccount profile =
    if profile.account == loggedInAccount then
        text (translators.t "transfer_result.you")

    else
        case profile.name of
            Just u ->
                text u

            Nothing ->
                Eos.viewName profile.account


viewEmpty : Translation.Translators -> Html msg
viewEmpty translators =
    div
        []
        [ p
            [ class "uppercase text-gray-900 text-sm" ]
            [ text (translators.t "profile.no_one") ]
        ]
