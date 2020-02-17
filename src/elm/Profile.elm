module Profile exposing
    ( Profile
    , ProfileCreate
    , ProfileForm
    , decode
    , emptyProfileForm
    , encodeProfileChat
    , encodeProfileCreate
    , encodeProfileForm
    , encodeProfileLogin
    , maxPinChars
    , minPinChars
    , mutation
    , pinValidationAttrs
    , profileToForm
    , query
    , selectionSet
    , username
    , view
    , viewProfileName
    , viewProfileNameTag
    )

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
import Html.Attributes exposing (class, maxlength, minlength, pattern, title, type_)
import I18Next exposing (Translations, t)
import Json.Decode as Decode exposing (Decoder, list, nullable, string)
import Json.Decode.Pipeline as Decode exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)



-- Profile


type alias Profile =
    { userName : Maybe String
    , email : Maybe String
    , bio : Maybe String
    , localization : Maybe String
    , account : Eos.Name
    , avatar : Avatar
    , interests : List String
    , chatUserId : Maybe String
    , chatToken : Maybe String
    , createdAt : Posix
    }


selectionSet : SelectionSet Profile Bespiral.Object.Profile
selectionSet =
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


decode : Decoder Profile
decode =
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


query : Eos.Name -> SelectionSet (Maybe Profile) RootQuery
query account =
    let
        nameString =
            Eos.nameToString account
    in
    Bespiral.Query.profile
        { input = { account = Present nameString } }
        selectionSet


mutation : Eos.Name -> ProfileForm -> SelectionSet (Maybe Profile) RootMutation
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
        selectionSet



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
encodeProfileLogin account =
    let
        accountEncoded =
            Encode.object [ ( "account", Eos.encodeName account ) ]
    in
    Encode.object [ ( "user", accountEncoded ) ]



-- Profile Create


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



-- Show account.name if no profile name is set.


username : Profile -> String
username profile =
    case Maybe.map String.trim profile.userName of
        Nothing ->
            Eos.nameToString profile.account

        Just "" ->
            Eos.nameToString profile.account

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


view : String -> Eos.Name -> Translations -> Profile -> Html msg
view ipfsUrl loggedInAccount translations profile =
    div [ class "flex flex-col items-center" ]
        [ div [ class "w-10 h-10 rounded-full" ]
            [ Avatar.view ipfsUrl profile.avatar "w-10 h-10"
            ]
        , div [ class "mt-2" ]
            [ viewProfileNameTag loggedInAccount profile translations ]
        ]


viewProfileNameTag : Eos.Name -> Profile -> Translations -> Html msg
viewProfileNameTag loggedInAccount profile translations =
    div [ class "flex items-center bg-black rounded p-1" ]
        [ p [ class "mx-2 pt-caption uppercase font-medium text-white text-caption" ]
            [ viewProfileName loggedInAccount profile translations ]
        ]


viewProfileName : Eos.Name -> Profile -> Translations -> Html msg
viewProfileName loggedInAccount profile translations =
    if profile.account == loggedInAccount then
        text (I18Next.t translations "transfer_result.you")

    else
        case profile.userName of
            Just u ->
                text u

            Nothing ->
                Eos.viewName profile.account
