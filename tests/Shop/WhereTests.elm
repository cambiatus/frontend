module Shop.WhereTests exposing (all)

import Expect
import PlaceRelease
import Shop.Where as Where
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Shop.Where"
        [ test "the busiest place comes first, ties by name" <|
            \_ ->
                Where.chips 8
                    { home = Nothing
                    , places = [ chip teresopolis 1, chip rio 5, chip niteroi 1 ]
                    , online = 0
                    }
                    |> .places
                    |> List.map (\c -> ( c.place.name, c.count ))
                    |> Expect.equal [ ( "Rio de Janeiro", 5 ), ( "Niterói", 1 ), ( "Teresópolis", 1 ) ]
        , test "Online is the backend's count" <|
            \_ ->
                Where.chips 8 { home = Nothing, places = [ chip rio 2 ], online = 7 }
                    |> .online
                    |> Expect.equal 7
        , test "Perto de você is the member's place and does not repeat in the list" <|
            \_ ->
                let
                    chips =
                        Where.chips 8
                            { home = Just rio
                            , places = [ chip rio 2, chip teresopolis 1 ]
                            , online = 0
                            }
                in
                Expect.equal
                    ( Maybe.map (\c -> ( c.place.name, c.count )) chips.home, List.map (.place >> .name) chips.places )
                    ( Just ( "Rio de Janeiro", 2 ), [ "Teresópolis" ] )
        , test "Perto de você is left out when nobody offers there" <|
            \_ ->
                Where.chips 8 { home = Just rio, places = [ chip teresopolis 1 ], online = 0 }
                    |> .home
                    |> Expect.equal Nothing
        , test "a place with no offers is left out" <|
            \_ ->
                Where.chips 8 { home = Nothing, places = [ chip rio 0, chip teresopolis 1 ], online = 0 }
                    |> .places
                    |> List.map (.place >> .name)
                    |> Expect.equal [ "Teresópolis" ]
        , test "only the busiest places up to the limit show" <|
            \_ ->
                Where.chips 1 { home = Nothing, places = [ chip teresopolis 1, chip rio 2 ], online = 0 }
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
                    Where.chipsIf { released = True, limit = 8 } (Just { home = Nothing, places = [], online = 0 })
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
    , places = [ chip rio 1 ]
    , online = 0
    }


chip : Where.Place -> Int -> Where.ChipPlace
chip place count =
    { place = place, count = count }


rio : Where.Place
rio =
    { id = 3304557, name = "Rio de Janeiro", stateCode = Just "RJ" }


teresopolis : Where.Place
teresopolis =
    { id = 3305802, name = "Teresópolis", stateCode = Just "RJ" }


niteroi : Where.Place
niteroi =
    { id = 3303302, name = "Niterói", stateCode = Just "RJ" }
