module Api.Graphql exposing
    ( Phrase, askForPhrase
    , signPhrasePort, decodeSignedPhrasePort, Password
    , signIn, Token, SignInResponse
    , storeToken, createAbsintheSocket
    , mutation, query
    , tokenDecoder
    )

{-| Some operations on our GraphQL API require authentication of a user with an
Auth token. This is used to authenticate users so they're able to use our
GraphQL API. The steps to generating an auth token are:


# Authentication


## 1. Ask for a phrase from the backendAPI

@docs Phrase, askForPhrase


## 2. Sign the phrase using the user's private key

If we sign a phrase with a private key, the backend can verify if it really was
the user who signed the phrase, by using the user's public key

@docs signPhrasePort, decodeSignedPhrasePort, Password


## 3. Perform the `signIn` mutation, providing the signed phrase as the password

The backend will verify if it was the user who really signed the phrase, and
give back an auth token so we can perform further requests

@docs signIn, Token, SignInResponse


### Token Helpers

@docs storeToken, createAbsintheSocket, withAuthTokenHeader


# Operations

@docs mutation, query

---

If you want to know how to get the user's private key, or how to authenticate them
to use our EOS API, check out the `Auth` module.

-}

import Cambiatus.Mutation
import Cambiatus.Object.Request
import Cambiatus.Object.Session
import Environment
import Eos.Account
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode
import Json.Encode
import List.Extra as List
import Ports
import Profile
import RemoteData exposing (RemoteData)
import Url exposing (Url)



-- AUTHENTICATION


{-| In order to sign in, the user needs to sign a phrase (gotten from the
backend API). In order to get a `Phrase`, use `askForPhrase`
-}
type Phrase
    = Phrase String


{-| The `Password` is the signed version of a `Phrase`. It can be sent to the backend
to actually log in and get a `Token`. In order to get a `Password`, use `signPhrasePort`
and `decodeSignedPhrasePort`
-}
type Password
    = Password String


{-| A `Token` is what authenticates a user on our GraphQL API. Check this module's
docs to learn how to get one, and how our auth system works
-}
type Token
    = Token String


type alias SignInResponse =
    { profile : Profile.Model
    , token : Token
    }


askForPhrase : Shared shared endpoints -> Eos.Account.Name -> (RemoteData (Graphql.Http.Error Phrase) Phrase -> msg) -> Cmd msg
askForPhrase shared account toMsg =
    mutation shared
        Nothing
        (Cambiatus.Mutation.genAuth { account = Eos.Account.nameToString account }
            Cambiatus.Object.Request.phrase
            |> SelectionSet.map Phrase
        )
        toMsg


signPhrasePort : msg -> Eos.Account.PrivateKey -> Phrase -> Ports.JavascriptOutModel msg
signPhrasePort responseAddress privateKey (Phrase phrase) =
    { responseAddress = responseAddress
    , responseData = Json.Encode.null
    , data =
        Json.Encode.object
            [ ( "name", Json.Encode.string "signString" )
            , ( "input", Json.Encode.string phrase )
            , ( "privateKey", Eos.Account.encodePrivateKey privateKey )
            ]
    }


decodeSignedPhrasePort : (Password -> msg) -> Json.Encode.Value -> Maybe msg
decodeSignedPhrasePort toMsg val =
    Json.Decode.decodeValue
        (Json.Decode.field "signed" Json.Decode.string
            |> Json.Decode.map (Password >> toMsg)
        )
        val
        |> Result.toMaybe


signIn :
    Shared shared endpoints
    ->
        { account : Eos.Account.Name
        , password : Password
        , invitationId : Maybe String
        }
    -> (RemoteData (Graphql.Http.Error SignInResponse) SignInResponse -> msg)
    -> Cmd msg
signIn shared { account, password, invitationId } toMsg =
    let
        (Password unwrappedPassword) =
            password
    in
    mutation shared
        Nothing
        (Cambiatus.Mutation.signIn (\opts -> { opts | invitationId = OptionalArgument.fromMaybe invitationId })
            { account = Eos.Account.nameToString account
            , password = unwrappedPassword
            }
            (SelectionSet.succeed SignInResponse
                |> with (Cambiatus.Object.Session.user Profile.selectionSet)
                |> with (SelectionSet.map Token Cambiatus.Object.Session.token)
            )
        )
        toMsg



-- TOKEN HELPERS


storeToken : Token -> Cmd msg
storeToken (Token token) =
    Ports.storeAuthToken token


createAbsintheSocket : Token -> Cmd msg
createAbsintheSocket (Token token) =
    Ports.createAbsintheSocket token


tokenDecoder : Json.Decode.Decoder Token
tokenDecoder =
    Json.Decode.map Token Json.Decode.string



-- OPERATIONS


query : Shared shared endpoints -> Maybe Token -> SelectionSet a RootQuery -> (RemoteData (Graphql.Http.Error a) a -> msg) -> Cmd msg
query ({ endpoints } as shared) maybeAuthToken query_ toMsg =
    query_
        |> Graphql.Http.queryRequest endpoints.graphql
        |> withCommunityDomain shared
        |> withAuthToken maybeAuthToken
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


mutation : Shared shared endpoints -> Maybe Token -> SelectionSet a RootMutation -> (RemoteData (Graphql.Http.Error a) a -> msg) -> Cmd msg
mutation ({ endpoints } as shared) maybeAuthToken mutation_ toMsg =
    mutation_
        |> Graphql.Http.mutationRequest endpoints.graphql
        |> withCommunityDomain shared
        |> withAuthToken maybeAuthToken
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)



-- INTERNAL HELPERS


type alias Shared shared endpoints =
    { shared
        | url : Url
        , useSubdomain : Bool
        , endpoints : { endpoints | graphql : String }
    }


withAuthToken : Maybe Token -> Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo
withAuthToken maybeToken =
    case maybeToken of
        Just (Token token) ->
            Graphql.Http.withHeader "authorization"
                ("Bearer " ++ token)

        Nothing ->
            identity


withCommunityDomain : Shared shared endpoints -> Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo
withCommunityDomain shared =
    let
        communityDomain =
            if shared.useSubdomain then
                Environment.communityDomain shared.url

            else if String.endsWith ".cambiatus.io" shared.url.path then
                let
                    sharedUrl =
                        shared.url
                in
                { sharedUrl
                    | path =
                        String.split "." sharedUrl.path
                            |> List.updateAt 0 (\_ -> "cambiatus")
                            |> String.join "."
                }
                    |> .path

            else
                "cambiatus.staging.cambiatus.io"
    in
    Graphql.Http.withHeader "Community-Domain" ("https://" ++ communityDomain)
