module User exposing (User, selectionSet, view, viewName)

import Avatar exposing (Avatar)
import Bespiral.Object
import Bespiral.Object.Profile
import Eos.Account as Eos
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (..)
import Html.Attributes exposing (..)
import I18Next exposing (Translations, t)


type alias User =
    { avatar : Avatar
    , userName : Maybe String
    , account : Eos.Name
    }


selectionSet : SelectionSet User Bespiral.Object.Profile
selectionSet =
    SelectionSet.succeed User
        |> with (Avatar.selectionSet Bespiral.Object.Profile.avatar)
        |> with (SelectionSet.map identity Bespiral.Object.Profile.name)
        |> with (Eos.nameSelectionSet Bespiral.Object.Profile.account)


view : String -> Eos.Name -> Translations -> User -> Html msg
view ipfsUrl loggedInAccount translations user =
    div []
        [ div [ class "w-8 h-8 rounded-full mx-auto" ]
            [ Avatar.view ipfsUrl user.avatar "w-10 h-10"
            ]
        , div [ class "px-6 py-4" ]
            [ viewName loggedInAccount translations user
            ]
        ]


viewName : Eos.Name -> Translations -> User -> Html msg
viewName loggedInAccount translations user =
    div [ class "inline-block bg-black rounded px-3 py-1 text-xs uppercase font-medium text-white" ]
        [ if user.account == loggedInAccount then
            text (I18Next.t translations "transfer_result.you")

          else
            case user.userName of
                Just username ->
                    text username

                Nothing ->
                    Eos.viewName user.account
        ]
