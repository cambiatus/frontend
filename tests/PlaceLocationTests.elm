module PlaceLocationTests exposing (all)

import Cambiatus.Enum.PlaceSource as PlaceSource
import Expect
import Graphql.Document
import Json.Decode as Decode
import Json.Encode as Encode
import PlaceLocation
import Test exposing (Test, describe, test)


all : Test
all =
    describe "PlaceLocation"
        [ describe "the address note"
            [ test "shows for a member without a default address" <|
                \_ ->
                    PlaceLocation.showAddressNote { dismissed = False } (Just (status False))
                        |> Expect.equal True
            , test "stays away for a member with a default address" <|
                \_ ->
                    PlaceLocation.showAddressNote { dismissed = False } (Just (status True))
                        |> Expect.equal False
            , test "stays away once dismissed this session" <|
                \_ ->
                    PlaceLocation.showAddressNote { dismissed = True } (Just (status False))
                        |> Expect.equal False
            , test "stays away until the status has loaded" <|
                \_ ->
                    PlaceLocation.showAddressNote { dismissed = False } Nothing
                        |> Expect.equal False
            ]
        , describe "the browser's answer"
            [ test "a position" <|
                \_ ->
                    decode [ ( "kind", Encode.string "located" ), ( "lat", Encode.float 51.924 ), ( "lng", Encode.float 4.478 ), ( "noteDismissed", Encode.bool True ) ]
                        |> Expect.equal
                            (Ok (PlaceLocation.Located { latitude = 51.924, longitude = 4.478 } { noteDismissed = True }))
            , test "a refusal" <|
                \_ ->
                    decode [ ( "kind", Encode.string "unavailable" ) ]
                        |> Expect.equal (Ok (PlaceLocation.Unavailable { noteDismissed = False }))
            , test "nothing to send" <|
                \_ ->
                    decode [ ( "kind", Encode.string "skipped" ), ( "noteDismissed", Encode.bool False ) ]
                        |> Expect.equal (Ok (PlaceLocation.Skipped { noteDismissed = False }))
            , test "a position without coordinates is not a position" <|
                \_ ->
                    decode [ ( "kind", Encode.string "located" ) ]
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        , describe "the requests"
            [ test "a position goes to locatePlace as latitude and longitude" <|
                \_ ->
                    Graphql.Document.serializeMutation (PlaceLocation.locate (Just { latitude = 51.924, longitude = 4.478 }))
                        |> expectAll [ "locatePlace(latitude: 51.924, longitude: 4.478)", "hasDefaultAddress" ]
            , test "no position goes to locatePlace without arguments" <|
                \_ ->
                    Graphql.Document.serializeMutation (PlaceLocation.locate Nothing)
                        |> expectAll [ "locatePlace {" ]
            ]
        ]


expectAll : List String -> String -> Expect.Expectation
expectAll needles haystack =
    needles
        |> List.filter (\needle -> not (String.contains needle haystack))
        |> Expect.equal []


decode : List ( String, Encode.Value ) -> Result Decode.Error PlaceLocation.Reply
decode fields =
    Decode.decodeValue PlaceLocation.replyDecoder (Encode.object fields)


status : Bool -> PlaceLocation.Status
status hasDefaultAddress =
    { label = Just "Rotterdam, Países Baixos"
    , source = PlaceSource.Located
    , hasDefaultAddress = hasDefaultAddress
    }
