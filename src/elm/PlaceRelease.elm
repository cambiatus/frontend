module PlaceRelease exposing (isReleased, query)

{-| The `elm_places` release: Elm's "Onde" chips in the shop and the place card
on the dashboard. Without the key Elm behaves as it did before, and the new
fields are never asked for.
-}

import Cambiatus.Object.Community as Community
import Cambiatus.Query
import Eos
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import RemoteData
import Session.LoggedIn as LoggedIn


releaseKey : String
releaseKey =
    "elm_places"


isReleased : List String -> Bool
isReleased releasedFeatures =
    List.member releaseKey releasedFeatures


{-| Whether the member holds the release in the selected community. Same symbol
rule as the profile editor's query.
-}
query : LoggedIn.Model -> SelectionSet Bool RootQuery
query loggedIn =
    let
        symbol =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    community.symbol

                _ ->
                    loggedIn.shared.selectedCommunity
                        |> Maybe.withDefault Eos.cambiatusSymbol
    in
    Cambiatus.Query.community (\optionals -> { optionals | symbol = Present (Eos.symbolToString symbol) })
        Community.releasedFeatures
        |> SelectionSet.map (Maybe.withDefault [] >> isReleased)
