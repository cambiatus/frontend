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
    , selectConfig
    , selectFilter
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
import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.Community as Community
import Cambiatus.Object.Contribution
import Cambiatus.Object.DeleteKycAddress
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
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class)
import Iso8601
import Kyc exposing (ProfileKyc)
import Profile.Address as Address exposing (Address)
import Profile.Contact as Contact
import Select
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import Time


type alias Basic a =
    { a
        | name : Maybe String
        , account : Eos.Name
        , avatar : Avatar
        , email : Maybe String
        , bio : Maybe String
        , contacts : List Contact.Normalized
    }


type alias Minimal =
    { name : Maybe String
    , account : Eos.Name
    , avatar : Avatar
    , email : Maybe String
    , bio : Maybe String
    , contacts : List Contact.Normalized
    }


type alias Model =
    { name : Maybe String
    , account : Eos.Name
    , avatar : Avatar
    , email : Maybe String
    , bio : Maybe String
    , localization : Maybe String
    , contacts : List Contact.Normalized
    , interests : List String
    , communities : List CommunityInfo
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


selectionSet : SelectionSet Model Cambiatus.Object.User
selectionSet =
    SelectionSet.succeed Model
        |> with User.name
        |> with (Eos.nameSelectionSet User.account)
        |> with (Avatar.selectionSet User.avatar)
        |> with User.email
        |> with User.bio
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
        |> with User.bio
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
            , bio = Present form.bio
            , contacts = Present (List.map (Contact.unwrap >> contactInput) form.contacts)
            , interests = Present interestString
            , location = Present form.localization
            , avatar = avatarInput
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
            |> with Cambiatus.Object.DeleteKycAddress.status
            |> with Cambiatus.Object.DeleteKycAddress.reason
        )


type alias DeleteAddressResult =
    { result : String
    , status : String
    }


deleteAddressMutation : Eos.Name -> SelectionSet (Maybe DeleteAddressResult) RootMutation
deleteAddressMutation _ =
    Cambiatus.Mutation.deleteAddress
        (SelectionSet.succeed DeleteAddressResult
            |> with Cambiatus.Object.DeleteKycAddress.status
            |> with Cambiatus.Object.DeleteKycAddress.reason
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
    , bio : String
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
    , bio = Maybe.withDefault "" bio
    , localization = Maybe.withDefault "" localization
    , avatar = Avatar.toMaybeString avatar
    , contacts = contacts
    , interest = ""
    , interests = interests
    , errors = Dict.empty
    }



-- View profile


viewProfileNameTag : Shared -> Eos.Name -> { profile | account : Eos.Name, name : Maybe String } -> Html msg
viewProfileNameTag shared loggedInAccount profile =
    p [ class "py-1 px-3 rounded-label uppercase font-bold text-white bg-black text-xs text-center" ]
        [ viewProfileName shared loggedInAccount profile ]


viewProfileName : Shared -> Eos.Name -> { profile | account : Eos.Name, name : Maybe String } -> Html msg
viewProfileName shared loggedInAccount profile =
    if profile.account == loggedInAccount then
        text (shared.translators.t "transfer_result.you")

    else
        case profile.name of
            Just u ->
                text u

            Nothing ->
                Eos.viewName profile.account


viewEmpty : Shared -> Html msg
viewEmpty shared =
    div
        []
        [ p
            [ class "uppercase text-gray-900 text-sm" ]
            [ text (shared.translators.t "profile.no_one") ]
        ]



-- Autocomplete select


selectConfig : Select.Config msg (Basic p) -> Shared -> Bool -> Select.Config msg (Basic p)
selectConfig select shared isDisabled =
    select
        |> Select.withInputClass "form-input h-12 w-full placeholder-gray-900"
        |> Select.withMultiInputItemContainerClass "hidden h-0"
        |> Select.withNotFound (shared.translators.t "community.actions.form.verifier_not_found")
        |> Select.withNotFoundClass "text-red border-solid border-gray-100 border rounded z-30 bg-white w-select"
        |> Select.withNotFoundStyles [ ( "padding", "0 2rem" ) ]
        |> Select.withDisabled isDisabled
        |> Select.withHighlightedItemClass "autocomplete-item-highlight"
        |> Select.withPrompt (shared.translators.t "community.actions.form.verifier_placeholder")
        |> Select.withItemHtml (viewAutoCompleteItem shared)
        |> Select.withMenuClass "w-full border-t-none border-solid border-gray-100 border rounded-sm z-30 bg-indigo-500 px-4 py-1"


selectFilter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
selectFilter minChars toLabel q items =
    if String.length q > minChars then
        Just <| Simple.Fuzzy.filter toLabel q items

    else
        Nothing


viewAutoCompleteItem : Shared -> Basic p -> Html Never
viewAutoCompleteItem _ { avatar, name, account } =
    div [ class "flex flex-row items-center z-30" ]
        [ div [ class "pt-4 pr-4 pb-4 pl-4" ] [ Avatar.view avatar "h-10 w-10" ]
        , div [ class "flex flex-col border-dotted border-b border-gray-500 pb-1 w-full" ]
            [ span [ class "text-white font-bold" ]
                [ text <| Maybe.withDefault "" name ]
            , span [ class "font-light text-white" ]
                [ text (Eos.nameToString account) ]
            ]
        ]
