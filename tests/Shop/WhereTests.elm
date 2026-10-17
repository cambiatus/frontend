module Shop.WhereTests exposing (all)

import Expect
import PlaceRelease
import Shop.Where as Where
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Shop.Where"
        [ test "an offer counts under its explicit places, not its seller's" <|
            \_ ->
                Where.chips 8
                    { home = Nothing
                    , entries = [ { places = [ teresopolis ], online = False, sellerPlace = Just rio } ]
                    }
                    |> .places
                    |> List.map (\chip -> ( chip.place.name, chip.count ))
                    |> Expect.equal [ ( "Teresópolis", 1 ) ]
        , test "an offer with no explicit place counts under its seller's" <|
            \_ ->
                Where.chips 8
                    { home = Nothing
                    , entries = [ { places = [], online = False, sellerPlace = Just rio } ]
                    }
                    |> .places
                    |> List.map (\chip -> ( chip.place.name, chip.count ))
                    |> Expect.equal [ ( "Rio de Janeiro", 1 ) ]
        , test "an Online offer counts only as Online" <|
            \_ ->
                let
                    chips =
                        Where.chips 8
                            { home = Nothing
                            , entries =
                                [ { places = [], online = True, sellerPlace = Just rio }
                                , { places = [], online = False, sellerPlace = Just rio }
                                ]
                            }
                in
                Expect.equal ( chips.online, List.map (.place >> .name) chips.places ) ( 1, [ "Rio de Janeiro" ] )
        , test "an offer naming a place twice counts once; the busiest place comes first" <|
            \_ ->
                Where.chips 8
                    { home = Nothing
                    , entries =
                        [ { places = [ teresopolis, teresopolis ], online = False, sellerPlace = Nothing }
                        , { places = [ rio ], online = False, sellerPlace = Nothing }
                        , { places = [ rio ], online = False, sellerPlace = Nothing }
                        ]
                    }
                    |> .places
                    |> List.map (\chip -> ( chip.place.name, chip.count ))
                    |> Expect.equal [ ( "Rio de Janeiro", 2 ), ( "Teresópolis", 1 ) ]
        , test "Perto de você is the member's place and does not repeat in the list" <|
            \_ ->
                let
                    chips =
                        Where.chips 8
                            { home = Just rio
                            , entries =
                                [ { places = [ rio ], online = False, sellerPlace = Nothing }
                                , { places = [ teresopolis ], online = False, sellerPlace = Nothing }
                                ]
                            }
                in
                Expect.equal
                    ( Maybe.map (.place >> .name) chips.home, List.map (.place >> .name) chips.places )
                    ( Just "Rio de Janeiro", [ "Teresópolis" ] )
        , test "Perto de você is left out when nobody offers there" <|
            \_ ->
                Where.chips 8
                    { home = Just rio
                    , entries = [ { places = [ teresopolis ], online = False, sellerPlace = Nothing } ]
                    }
                    |> .home
                    |> Expect.equal Nothing
        , test "only the busiest places up to the limit show" <|
            \_ ->
                Where.chips 1
                    { home = Nothing
                    , entries =
                        [ { places = [ rio ], online = False, sellerPlace = Nothing }
                        , { places = [ rio ], online = False, sellerPlace = Nothing }
                        , { places = [ teresopolis ], online = False, sellerPlace = Nothing }
                        ]
                    }
                    |> .places
                    |> List.map (.place >> .name)
                    |> Expect.equal [ "Rio de Janeiro" ]
        , describe "the elm_places release"
            [ test "only the elm_places key releases it" <|
                \_ ->
                    Expect.equal
                        ( PlaceRelease.isReleased [ "location_picker", "elm_places" ]
                        , PlaceRelease.isReleased [ "location_picker" ]
                        )
                        ( True, False )
            , test "without the release the shop shows no chips, however many offers there are" <|
                \_ ->
                    Where.chipsIf { released = False, limit = 8 } (Just options)
                        |> Expect.equal Nothing
            , test "with the release the shop shows the chips" <|
                \_ ->
                    Where.chipsIf { released = True, limit = 8 } (Just options)
                        |> Maybe.map (.places >> List.length)
                        |> Expect.equal (Just 1)
            , test "released, but with nothing to choose from, there are no chips" <|
                \_ ->
                    Where.chipsIf { released = True, limit = 8 } (Just { home = Nothing, entries = [] })
                        |> Expect.equal Nothing
            , test "released, but the options have not arrived, there are no chips" <|
                \_ ->
                    Where.chipsIf { released = True, limit = 8 } Nothing
                        |> Expect.equal Nothing
            ]
        ]


options : Where.Options
options =
    { home = Nothing
    , entries = [ { places = [ rio ], online = False, sellerPlace = Nothing } ]
    }


rio : Where.Place
rio =
    { id = 3304557, name = "Rio de Janeiro", stateCode = Just "RJ" }


teresopolis : Where.Place
teresopolis =
    { id = 3305802, name = "Teresópolis", stateCode = Just "RJ" }
