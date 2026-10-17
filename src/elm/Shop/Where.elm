module Shop.Where exposing
    ( Where(..), isChosen
    , Place, ChipPlace, Options, query
    , Chips, chips, chipsIf
    )

{-| The shop's "Onde" filter: where an offer is had. A chip is a município the
member lives in ("Perto de você"), a município the community's offers are had in,
or "Online". Choosing one is passed to `products` as `filters.placeId` or
`filters.online`.

The counts come from the backend (`placeCounts`), the same numbers LiveView's
chips show.


# Choice

@docs Where, isChosen


# Reading the options

@docs Place, ChipPlace, Options, query


# Chips

@docs Chips, chips, chipsIf

-}

import Cambiatus.Object
import Cambiatus.Object.Locality as Locality
import Cambiatus.Object.LocationPlace as LocationPlace
import Cambiatus.Object.PlaceCount as PlaceCount
import Cambiatus.Object.PlaceCounts as PlaceCounts
import Cambiatus.Object.User as User
import Cambiatus.Query as Query
import Eos.Account
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)



-- CHOICE


type Where
    = Anywhere
    | InPlace Int
    | Online


isChosen : Where -> Bool
isChosen chosen =
    chosen /= Anywhere



-- OPTIONS


{-| A município (or the município of a bairro)
-}
type alias Place =
    { id : Int
    , name : String
    , stateCode : Maybe String
    }


type alias ChipPlace =
    { place : Place
    , count : Int
    }


{-| `home` is the member's own município; `places` are the municípios the
community's offers are had in, with how many; `online` is how many Online offers
there are
-}
type alias Options =
    { home : Maybe Place
    , places : List ChipPlace
    , online : Int
    }


query : Eos.Account.Name -> SelectionSet Options RootQuery
query account =
    SelectionSet.map2
        (\home counts ->
            { home = home
            , places = counts |> Maybe.map .places |> Maybe.withDefault []
            , online = counts |> Maybe.map .online |> Maybe.withDefault 0
            }
        )
        (Query.user { account = Eos.Account.nameToString account } homeSelectionSet
            |> SelectionSet.map (Maybe.andThen identity)
        )
        (Query.placeCounts
            (SelectionSet.map2 (\online places -> { online = online, places = places })
                PlaceCounts.online
                (PlaceCounts.places chipPlaceSelectionSet)
            )
        )


chipPlaceSelectionSet : SelectionSet ChipPlace Cambiatus.Object.PlaceCount
chipPlaceSelectionSet =
    SelectionSet.succeed ChipPlace
        |> with (PlaceCount.locality placeSelectionSet)
        |> with PlaceCount.count


homeSelectionSet : SelectionSet (Maybe Place) Cambiatus.Object.User
homeSelectionSet =
    User.locationPlace (LocationPlace.municipality placeSelectionSet)


placeSelectionSet : SelectionSet Place Cambiatus.Object.Locality
placeSelectionSet =
    SelectionSet.succeed Place
        |> with Locality.id
        |> with Locality.name
        |> with Locality.stateCode



-- CHIPS


{-| `home` is "Perto de você"; `places` are the other municípios, the busiest
first; `online` is how many Online offers there are
-}
type alias Chips =
    { home : Maybe ChipPlace
    , places : List ChipPlace
    , online : Int
    }


{-| At most `limit` other places, the busiest first (ties by name). The member's
own município is "Perto de você" and does not repeat in the list. A place nobody
offers in is left out, and so is "Perto de você" when the member has no place.
-}
chips : Int -> Options -> Chips
chips limit { home, places, online } =
    let
        offered =
            List.filter (\chip -> chip.count > 0) places

        homeId =
            Maybe.map .id home
    in
    { home =
        home
            |> Maybe.andThen (\place -> List.filter (\chip -> chip.place.id == place.id) offered |> List.head)
    , places =
        offered
            |> List.filter (\chip -> Just chip.place.id /= homeId)
            |> List.sortBy (\chip -> ( -chip.count, chip.place.name ))
            |> List.take limit
    , online = online
    }


{-| What the shop shows: nothing without the `elm_places` release, and nothing
when there is no place or Online offer to choose from
-}
chipsIf : { released : Bool, limit : Int } -> Maybe Options -> Maybe Chips
chipsIf { released, limit } maybeOptions =
    case ( released, maybeOptions ) of
        ( True, Just options ) ->
            let
                found =
                    chips limit options
            in
            if found.home /= Nothing || not (List.isEmpty found.places) || found.online > 0 then
                Just found

            else
                Nothing

        _ ->
            Nothing
