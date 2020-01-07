module User exposing (User, selectionSet, view, viewName, viewNameTag)

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
    div [ class "flex flex-col items-center" ]
        [ div [ class "w-10 h-10 rounded-full" ]
            [ Avatar.view ipfsUrl user.avatar "w-10 h-10"
            ]
        , div [ class "mt-2" ]
            [ viewNameTag loggedInAccount user translations ]
        ]


viewNameTag : Eos.Name -> User -> Translations -> Html msg
viewNameTag loggedInAccount user translations =
    div [ class "flex items-center bg-black rounded p-1" ]
        [ p [ class "mx-2 pt-caption uppercase font-medium text-white text-caption" ]
            [ viewName loggedInAccount user translations ]
        ]


viewName : Eos.Name -> User -> Translations -> Html msg
viewName loggedInAccount user translations =
    if user.account == loggedInAccount then
        text (I18Next.t translations "transfer_result.you")

    else
        case user.userName of
            Just username ->
                text username

            Nothing ->
                Eos.viewName user.account
