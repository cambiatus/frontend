module PlacePrompt exposing
    ( Prompt(..)
    , confirm
    , dismiss
    , query
    )

{-| The card that asks a member where they are, once. The backend decides who
gets it (`User.placePrompt`, only the owner, only the recently active, never
after an answer); this module reads it and sends the answers.
-}

import Cambiatus.Enum.PlacePromptKind as Kind
import Cambiatus.Mutation as Mutation
import Cambiatus.Object
import Cambiatus.Object.PlacePrompt as PlacePrompt
import Cambiatus.Object.User as User
import Cambiatus.Query as Query
import Eos.Account
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)


{-| `Ask`: there is no place yet. `Confirm`: the place guessed from the profile,
as a label.
-}
type Prompt
    = Ask
    | Confirm String


query : Eos.Account.Name -> SelectionSet (Maybe Prompt) RootQuery
query account =
    Query.user { account = Eos.Account.nameToString account }
        (User.placePrompt selectionSet)
        |> SelectionSet.map (Maybe.andThen identity)


selectionSet : SelectionSet Prompt Cambiatus.Object.PlacePrompt
selectionSet =
    SelectionSet.succeed fromKind
        |> with PlacePrompt.kind
        |> with PlacePrompt.label


fromKind : Kind.PlacePromptKind -> Maybe String -> Prompt
fromKind kind label =
    case ( kind, label ) of
        ( Kind.Confirm, Just place ) ->
            Confirm place

        _ ->
            Ask


confirm : SelectionSet Bool RootMutation
confirm =
    Mutation.confirmPlace


dismiss : SelectionSet Bool RootMutation
dismiss =
    Mutation.dismissPlacePrompt
