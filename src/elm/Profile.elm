module Profile exposing
    ( Basic
    , CommunityInfo
    , DeleteKycAndAddressResult
    , Minimal
    , Model
    , ProfileCreate
    , ProfileForm
    , decode
    , deleteKycAndAddressMutation
    , emptyProfileForm
    , encodeProfileCreate
    , encodeProfileForm
    , encodeProfileLogin
    , encodeProfileLoginWithInvitation
    , maxPinChars
    , minPinChars
    , minimalSelectionSet
    , mutation
    , pinValidationAttrs
    , profileToForm
    , query
    , selectConfig
    , selectFilter
    , selectionSet
    , upsertKycMutation
    , username
    , view
    , viewEmpty
    , viewLarge
    , viewProfileName
    , viewProfileNameTag
    )

import Avatar exposing (Avatar)
import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.Community as Community
import Cambiatus.Object.DeleteKycAddress
import Cambiatus.Object.User as User
import Cambiatus.Query
import Cambiatus.Scalar exposing (Id(..))
import Dict exposing (Dict)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, div, p, span, text)
import Html.Attributes exposing (class, href, maxlength, minlength, pattern, title, type_)
import Json.Decode as Decode exposing (Decoder, int, nullable, string)
import Json.Decode.Pipeline as Decode exposing (optional, required)
import Json.Encode as Encode
import Kyc exposing (ProfileKyc)
import Profile.Address as Address exposing (Address)
import Select
import Session.Shared exposing (Shared)
import Simple.Fuzzy


type alias Basic a =
    { a
        | name : Maybe String
        , account : Eos.Name
        , avatar : Avatar
    }


type alias Minimal =
    Basic {}


type alias Model =
    Basic
        { email : Maybe String
        , bio : Maybe String
        , localization : Maybe String
        , account : Eos.Name
        , interests : List String
        , communities : List CommunityInfo
        , analysisCount : Int
        , kyc : Maybe ProfileKyc
        , address : Maybe Address
        }



-- buildModel : Maybe String -> Eos.Name -> Avatar -> Maybe String -> Maybe String -> Maybe String -> Eos.Name -> List String -> List CommunityInfo -> Int -> Maybe ProfileKyc -> Maybe Address


buildModel name account avatar email bio location interests communities analysis kyc address =
    { name = name
    , account = account
    , avatar = avatar
    , email = email
    , bio = bio
    , localization = location
    , interests = interests
    , communities = communities
    , analysisCount = analysis
    , kyc = kyc
    , address = address
    }


type alias CommunityInfo =
    { id : Symbol
    , name : String
    , logo : String
    , hasShop : Bool
    , hasActions : Bool
    , hasKyc : Bool
    }


selectionSet : SelectionSet Model Cambiatus.Object.User
selectionSet =
    SelectionSet.succeed buildModel
        |> with User.name
        |> with (Eos.nameSelectionSet User.account)
        |> with (Avatar.selectionSet User.avatar)
        |> with User.email
        |> with User.bio
        |> with User.location
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
    SelectionSet.succeed (\name account avatar -> { name = name, account = account, avatar = avatar })
        |> with User.name
        |> with (Eos.nameSelectionSet User.account)
        |> with (Avatar.selectionSet User.avatar)


communityInfoSelectionSet : SelectionSet CommunityInfo Cambiatus.Object.Community
communityInfoSelectionSet =
    SelectionSet.succeed CommunityInfo
        |> with (Eos.symbolSelectionSet Community.symbol)
        |> with Community.name
        |> with Community.logo
        |> with Community.hasShop
        |> with Community.hasObjectives
        |> with Community.hasKyc


decode : Decoder Model
decode =
    Decode.succeed buildModel
        |> optional "name" (nullable string) Nothing
        |> required "account" Eos.nameDecoder
        |> optional "avatar" Avatar.decode Avatar.empty
        |> optional "email" (nullable string) Nothing
        |> optional "bio" (nullable string) Nothing
        |> optional "localization" (nullable string) Nothing
        |> optional "interests" decodeInterests []
        |> Decode.hardcoded []
        |> Decode.at [ "data", "user" ]
        |> optional "analysisCount" int 0
        |> optional "kyc" (nullable Kyc.decode) Nothing
        |> optional "address" (nullable Address.decode) Nothing


decodeInterests : Decoder (List String)
decodeInterests =
    Decode.string
        |> Decode.andThen
            (\s ->
                String.split "," s
                    |> Decode.succeed
            )


query : Eos.Name -> SelectionSet (Maybe Model) RootQuery
query account =
    let
        nameString =
            Eos.nameToString account
    in
    Cambiatus.Query.user
        { account = nameString }
        selectionSet


mutation : Eos.Name -> ProfileForm -> SelectionSet (Maybe Model) RootMutation
mutation account form =
    let
        nameString =
            Eos.nameToString account

        interestString =
            form.interest
                :: form.interests
                |> List.filter ((/=) "")
                |> String.join ","

        avatarInput =
            Maybe.map Present form.avatar
                |> Maybe.withDefault Absent
    in
    Cambiatus.Mutation.updateUser
        { input =
            { account = nameString
            , name = Present form.name
            , email = Present form.email
            , bio = Present form.bio

            -- TODO: Implement add/edit contacts
            , contacts = Absent
            , interests = Present interestString
            , location = Present form.localization
            , avatar = avatarInput
            }
        }
        selectionSet



-- UPDATE/INSERT KYC


upsertKycMutation : Eos.Name -> ProfileKyc -> SelectionSet (Maybe ProfileKyc) RootMutation
upsertKycMutation account data =
    let
        nameString =
            Eos.nameToString account
    in
    Cambiatus.Mutation.upsertKyc
        { input =
            { accountId = nameString
            , countryId = Id "1"
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
deleteKycMutation account =
    let
        nameString =
            Eos.nameToString account
    in
    Cambiatus.Mutation.deleteKyc
        { input =
            { account = nameString
            }
        }
        (SelectionSet.succeed DeleteKycResult
            |> with Cambiatus.Object.DeleteKycAddress.status
            |> with Cambiatus.Object.DeleteKycAddress.reason
        )


type alias DeleteAddressResult =
    { result : String
    , status : String
    }


deleteAddressMutation : Eos.Name -> SelectionSet (Maybe DeleteAddressResult) RootMutation
deleteAddressMutation account =
    let
        nameString =
            Eos.nameToString account
    in
    Cambiatus.Mutation.deleteAddress
        { input =
            { account = nameString
            }
        }
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


encodeProfileLogin : Eos.Name -> Encode.Value
encodeProfileLogin account =
    let
        accountEncoded =
            Encode.object [ ( "account", Eos.encodeName account ) ]
    in
    Encode.object [ ( "user", accountEncoded ) ]


encodeProfileLoginWithInvitation : Eos.Name -> String -> Encode.Value
encodeProfileLoginWithInvitation account invitationId =
    let
        accountEncoded =
            Encode.object [ ( "account", Eos.encodeName account ) ]
    in
    Encode.object
        [ ( "user", accountEncoded )
        , ( "invitation_id", Encode.string invitationId )
        ]


type alias ProfileCreate =
    { name : String
    , email : String
    , account : Eos.Name
    , invitationId : Maybe String
    }


encodeProfileCreate : ProfileCreate -> Encode.Value
encodeProfileCreate form =
    let
        user =
            [ Just ( "name", Encode.string form.name )
            , Just ( "email", Encode.string form.email )
            , Just ( "account", Eos.encodeName form.account )
            , Maybe.map (\invId -> ( "invitation_id", Encode.string invId )) form.invitationId
            ]
                |> List.filterMap identity
                |> Encode.object
    in
    Encode.object [ ( "user", user ) ]


type alias ProfileForm =
    { name : String
    , email : String
    , bio : String
    , localization : String
    , avatar : Maybe String
    , interest : String
    , interests : List String
    , errors : Dict String String
    }


emptyProfileForm : ProfileForm
emptyProfileForm =
    { name = ""
    , email = ""
    , bio = ""
    , localization = ""
    , avatar = Nothing
    , interest = ""
    , interests = []
    , errors = Dict.empty
    }


profileToForm : Model -> ProfileForm
profileToForm { name, email, bio, localization, avatar, interests } =
    { name = Maybe.withDefault "" name
    , email = Maybe.withDefault "" email
    , bio = Maybe.withDefault "" bio
    , localization = Maybe.withDefault "" localization
    , avatar = Avatar.toMaybeString avatar
    , interest = ""
    , interests = interests
    , errors = Dict.empty
    }


encodeProfileForm : Eos.Name -> ProfileForm -> Encode.Value
encodeProfileForm account form =
    Encode.object
        [ ( "name", Encode.string form.name )
        , ( "email", Encode.string form.email )
        , ( "bio", Encode.string form.bio )
        , ( "localization", Encode.string form.localization )
        , ( "account", Eos.encodeName account )
        , ( "interests"
          , Encode.list Encode.string form.interests
          )
        ]



{- Show account.name if no profile name is set. -}


username : Basic a -> String
username { name, account } =
    case Maybe.map String.trim name of
        Nothing ->
            Eos.nameToString account

        Just "" ->
            Eos.nameToString account

        Just userName ->
            userName


minPinChars : Int
minPinChars =
    6


maxPinChars : Int
maxPinChars =
    6


pinValidationAttrs : List (Html.Attribute msg)
pinValidationAttrs =
    [ type_ "password"
    , minlength minPinChars
    , maxlength maxPinChars
    , pattern "[0-9]*"
    , Html.Attributes.attribute "inputmode" "numeric"
    , title "Use only numbers."
    ]



-- View profile


view : Shared -> Eos.Name -> { profile | account : Eos.Name, avatar : Avatar, name : Maybe String } -> Html msg
view shared loggedInAccount profile =
    a
        [ class "flex flex-col items-center"
        , href ("/profile/" ++ Eos.nameToString profile.account)
        ]
        [ div [ class "w-10 h-10 rounded-full" ]
            [ Avatar.view profile.avatar "w-10 h-10"
            ]
        , div [ class "mt-2" ]
            [ viewProfileNameTag shared loggedInAccount profile ]
        ]


viewLarge :
    Shared
    -> Eos.Name
    -> { profile | account : Eos.Name, name : Maybe String, avatar : Avatar }
    -> Html msg
viewLarge shared loggedInAccount profile =
    a
        [ class "flex flex-col items-center"
        , href ("/profile/" ++ Eos.nameToString profile.account)
        ]
        [ div [ class "w-20 h-20 rounded-full" ]
            [ Avatar.view profile.avatar "w-20 h-20"
            ]
        , div [ class "mt-2" ]
            [ viewProfileNameTag shared loggedInAccount profile ]
        ]


viewProfileNameTag : Shared -> Eos.Name -> { profile | account : Eos.Name, name : Maybe String } -> Html msg
viewProfileNameTag shared loggedInAccount profile =
    div [ class "flex items-center bg-black rounded-label p-1" ]
        [ p [ class "mx-2 pt-caption uppercase font-bold text-white text-caption" ]
            [ viewProfileName shared loggedInAccount profile ]
        ]


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
    div [ class "flex flex-col items-center" ]
        [ div [ class "w-10 h-10 rounded-full" ]
            [ div
                [ class "profile-avatar w-10 h-10"
                ]
                []
            ]
        , div [ class "mt-2" ]
            [ div [ class "flex items-center bg-black rounded-sm p-1" ]
                [ p [ class "mx-2 pt-caption uppercase font-medium text-white text-caption" ]
                    [ text (shared.translators.t "profile.no_one") ]
                ]
            ]
        ]



-- Autocomplete select


selectConfig : Select.Config msg (Basic p) -> Shared -> Bool -> Select.Config msg (Basic p)
selectConfig select shared isDisabled =
    select
        |> Select.withInputClass "form-input h-12 w-full placeholder-gray-900"
        |> Select.withClear False
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
            [ span [ class "text-white text-body font-bold leading-loose" ]
                [ text <| Maybe.withDefault "" name ]
            , span [ class "font-light text-white" ]
                [ text (Eos.nameToString account) ]
            ]
        ]
