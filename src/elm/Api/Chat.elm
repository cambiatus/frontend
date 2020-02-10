module Api.Chat exposing (ChatPreferences, chatPreferencesSelectionSet, toChatFormat, updateChatLanguage, updateChatLanguageMutation)

import Api.Graphql
import Bespiral.Mutation
import Bespiral.Object
import Bespiral.Object.ChatPreferences as ChatPreferences
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Profile exposing (Profile)
import Session.Shared exposing (Shared)


type alias ChatPreferences =
    { userId : String
    , language : String
    }


toChatFormat : String -> String
toChatFormat language =
    if String.startsWith "es" language then
        "es"

    else if String.startsWith "pt" language then
        "pt-BR"

    else if String.startsWith "cat" language then
        "ca"

    else
        "en"


chatPreferencesSelectionSet : SelectionSet ChatPreferences Bespiral.Object.ChatPreferences
chatPreferencesSelectionSet =
    SelectionSet.succeed ChatPreferences
        |> with ChatPreferences.userId
        |> with ChatPreferences.language


updateChatLanguageMutation : String -> String -> SelectionSet (Maybe ChatPreferences) RootMutation
updateChatLanguageMutation userId language =
    Bespiral.Mutation.updateChatLanguage
        { input =
            { userId = userId
            , language = language
            }
        }
        chatPreferencesSelectionSet


updateChatLanguage : Shared -> Profile -> String -> (Result (Graphql.Http.Error (Maybe ChatPreferences)) (Maybe ChatPreferences) -> msg) -> Cmd msg
updateChatLanguage shared profile language toMsg =
    case profile.chatUserId of
        Just userId ->
            Api.Graphql.mutation shared
                (toChatFormat language
                    |> updateChatLanguageMutation userId
                )
                toMsg

        Nothing ->
            Cmd.none
