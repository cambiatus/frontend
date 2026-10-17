module Shop.Where exposing
    ( Where(..), isChosen
    , Place, Options, Entry, query
    , Chips, ChipPlace, chips, chipsIf
    )

{-| The shop's "Onde" filter: where an offer is had. A chip is a município the
member lives in ("Perto de você"), a município the community's offers are had in,
or "Online". Choosing one is passed to `products` as `filters.placeId` or
`filters.online`.

The API has no per-place counts yet, so the chips are counted here from one light
query over the community's offers: an offer counts under each município it
names, or, when it names none, under its seller's município. Offers that are
Online count only there.


# Choice

@docs Where, isChosen


# Reading the options

@docs Place, Options, Entry, query


# Chips

@docs Chips, ChipPlace, chips, chipsIf

-}

import Cambiatus.Object
import Cambiatus.Object.Locality as Locality
import Cambiatus.Object.LocationPlace as LocationPlace
import Cambiatus.Object.Product as Product
import Cambiatus.Object.User as User
import Cambiatus.Query as Query
import Dict exposing (Dict)
import Eos.Account
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import List.Extra



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


{-| What the shop needs to know about one offer to count it
-}
type alias Entry =
    { places : List Place
    , online : Bool
    , sellerPlace : Maybe Place
    }


type alias Options =
    { home : Maybe Place
    , entries : List Entry
    }


query : Eos.Account.Name -> SelectionSet Options RootQuery
query account =
    SelectionSet.map2 Options
        (Query.user { account = Eos.Account.nameToString account } homeSelectionSet
            |> SelectionSet.map (Maybe.andThen identity)
        )
        (Query.products identity entrySelectionSet)


homeSelectionSet : SelectionSet (Maybe Place) Cambiatus.Object.User
homeSelectionSet =
    User.locationPlace (LocationPlace.municipality placeSelectionSet)


entrySelectionSet : SelectionSet Entry Cambiatus.Object.Product
entrySelectionSet =
    SelectionSet.succeed Entry
        |> with (Product.places municipalityOfSelectionSet |> SelectionSet.map (List.filterMap identity))
        |> with Product.online
        |> with (Product.creator homeSelectionSet)


placeSelectionSet : SelectionSet Place Cambiatus.Object.Locality
placeSelectionSet =
    SelectionSet.succeed Place
        |> with Locality.id
        |> with Locality.name
        |> with Locality.stateCode


{-| A município stays itself; a bairro counts under its município
-}
municipalityOfSelectionSet : SelectionSet (Maybe Place) Cambiatus.Object.Locality
municipalityOfSelectionSet =
    SelectionSet.map2
        (\own parent -> Just (Maybe.withDefault own parent))
        placeSelectionSet
        (Locality.municipality placeSelectionSet)



-- CHIPS


type alias ChipPlace =
    { place : Place
    , count : Int
    }


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
chips limit { home, entries } =
    let
        placesOf entry =
            if entry.online && List.isEmpty entry.places then
                []

            else if List.isEmpty entry.places then
                Maybe.map List.singleton entry.sellerPlace |> Maybe.withDefault []

            else
                entry.places

        counted : Dict Int ChipPlace
        counted =
            entries
                |> List.concatMap (placesOf >> List.Extra.uniqueBy .id)
                |> List.foldl
                    (\place ->
                        Dict.update place.id
                            (\existing ->
                                case existing of
                                    Just chip ->
                                        Just { chip | count = chip.count + 1 }

                                    Nothing ->
                                        Just { place = place, count = 1 }
                            )
                    )
                    Dict.empty

        homeId =
            Maybe.map .id home
    in
    { home =
        home
            |> Maybe.andThen (\place -> Dict.get place.id counted)
    , places =
        Dict.values counted
            |> List.filter (\chip -> Just chip.place.id /= homeId)
            |> List.sortBy (\chip -> ( -chip.count, chip.place.name ))
            |> List.take limit
    , online = entries |> List.filter .online |> List.length
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
