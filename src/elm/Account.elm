module Account exposing (Profile, ProfileCreate, ProfileForm, accountSelectionSet, decodeProfile, emptyProfileForm, encodeProfileChat, encodeProfileCreate, encodeProfileForm, encodeProfileLogin, maxPinChars, minPinChars, pinValidationAttrs, profileMutation, profileQuery, profileToForm, username)

{-| The logged-in user currently viewing this page. It stores enough data to
be able to render the menu bar (username and avatar), along with Cred so it's
impossible to have an Account if you aren't logged in.
-}

import Avatar exposing (Avatar)
import Bespiral.Mutation
import Bespiral.Object
import Bespiral.Object.Profile as User
import Bespiral.Query
import Dict exposing (Dict)
import Eos.Account as Eos
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (..)
import Html.Attributes exposing (maxlength, minlength, pattern, title, type_)
import Json.Decode as Decode exposing (Decoder, list, nullable, string)
import Json.Decode.Pipeline as Decode exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)



-- Profile


type alias Profile =
    { userName : Maybe String
    , email : Maybe String
    , bio : Maybe String
    , localization : Maybe String
    , accountName : Eos.Name
    , avatar : Avatar
    , interests : List String
    , chatUserId : Maybe String
    , chatToken : Maybe String
    , createdAt : Posix
    }


accountSelectionSet : SelectionSet Profile Bespiral.Object.Profile
accountSelectionSet =
    SelectionSet.succeed Profile
        |> with User.name
        |> with User.email
        |> with User.bio
        |> with User.location
        |> with (Eos.nameSelectionSet User.account)
        |> with (Avatar.selectionSet User.avatar)
        |> with
            (User.interests
                |> SelectionSet.map
                    (\maybeInterests ->
                        Maybe.map
                            (String.split ",")
                            maybeInterests
                            |> Maybe.withDefault []
                    )
            )
        |> with User.chatUserId
        |> with User.chatToken
        |> SelectionSet.hardcoded (Time.millisToPosix 0)


decodeProfile : Decoder Profile
decodeProfile =
    Decode.succeed Profile
        |> optional "name" (nullable string) Nothing
        |> optional "email" (nullable string) Nothing
        |> optional "bio" (nullable string) Nothing
        |> optional "localization" (nullable string) Nothing
        |> required "account" Eos.nameDecoder
        |> optional "avatar" Avatar.decode Avatar.empty
        |> optional "interests" decodeInterests []
        |> optional "chat_user_id" (nullable string) Nothing
        |> optional "chat_token" (nullable string) Nothing
        |> Decode.hardcoded (Time.millisToPosix 0)
        |> Decode.at [ "data", "user" ]


decodeInterests : Decoder (List String)
decodeInterests =
    Decode.string
        |> Decode.andThen
            (\s ->
                String.split "," s
                    |> Decode.succeed
            )


profileQuery : Eos.Name -> SelectionSet (Maybe Profile) RootQuery
profileQuery account =
    let
        nameString =
            Eos.nameToString account
    in
    Bespiral.Query.profile
        { input = { account = Present nameString } }
        accountSelectionSet


profileMutation : Eos.Name -> ProfileForm -> SelectionSet (Maybe Profile) RootMutation
profileMutation accountName form =
    let
        nameString =
            Eos.nameToString accountName

        interestString =
            form.interest
                :: form.interests
                |> List.filter ((/=) "")
                |> String.join ","

        avatarInput =
            Maybe.map Present form.avatar
                |> Maybe.withDefault Absent
    in
    Bespiral.Mutation.updateProfile
        { input =
            { account = nameString
            , name = Present form.name
            , email = Present form.email
            , bio = Present form.bio
            , interests = Present interestString
            , location = Present form.localization
            , avatar = avatarInput
            }
        }
        accountSelectionSet



-- Profile Chat


encodeProfileChat : Profile -> Encode.Value
encodeProfileChat profile =
    let
        chatUserId =
            Maybe.withDefault "" profile.chatUserId

        chatToken =
            Maybe.withDefault "" profile.chatToken
    in
    [ Just ( "chatUserId", Encode.string chatUserId )
    , Just ( "chatToken", Encode.string chatToken )
    ]
        |> List.filterMap identity
        |> Encode.object



-- Profile Login


encodeProfileLogin : Eos.Name -> Encode.Value
encodeProfileLogin accountName =
    let
        account =
            Encode.object [ ( "account", Eos.encodeName accountName ) ]
    in
    Encode.object [ ( "user", account ) ]



-- Profile Create


type alias ProfileCreate =
    { name : String
    , email : String
    , accountName : Eos.Name
    , invitationId : Maybe String
    }


encodeProfileCreate : ProfileCreate -> Encode.Value
encodeProfileCreate form =
    let
        user =
            [ Just ( "name", Encode.string form.name )
            , Just ( "email", Encode.string form.email )
            , Just ( "account", Eos.encodeName form.accountName )
            , Maybe.map (\invId -> ( "invitation_id", Encode.string invId )) form.invitationId
            ]
                |> List.filterMap identity
                |> Encode.object
    in
    Encode.object [ ( "user", user ) ]



-- Profile Form


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


profileToForm : Profile -> ProfileForm
profileToForm profile =
    { name = Maybe.withDefault "" profile.userName
    , email = Maybe.withDefault "" profile.email
    , bio = Maybe.withDefault "" profile.bio
    , localization = Maybe.withDefault "" profile.localization
    , avatar = Avatar.toMaybeString profile.avatar
    , interest = ""
    , interests = profile.interests
    , errors = Dict.empty
    }


encodeProfileForm : Eos.Name -> ProfileForm -> Encode.Value
encodeProfileForm accountName form =
    Encode.object
        [ ( "name", Encode.string form.name )
        , ( "email", Encode.string form.email )
        , ( "bio", Encode.string form.bio )
        , ( "localization", Encode.string form.localization )
        , ( "account", Eos.encodeName accountName )
        , ( "interests"
          , Encode.list Encode.string form.interests
          )
        ]



-- Show account.name if no profile name is set.


username : Profile -> String
username profile =
    case Maybe.map String.trim profile.userName of
        Nothing ->
            Eos.nameToString profile.accountName

        Just "" ->
            Eos.nameToString profile.accountName

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
