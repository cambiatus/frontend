module TestUtils exposing
    ( appendGenerators
    , cambiatusUrlFuzzer
    , digitGenerator
    , generateEither
    , nonZeroDigitGenerator
    , randomListWithRandomLength
    )

import Fuzz
import Random
import Random.Char
import Random.String
import Shrink
import Url



-- RANDOM GENERATORS


randomAndMap : Random.Generator a -> Random.Generator (a -> c) -> Random.Generator c
randomAndMap =
    Random.map2 (|>)


nonZeroDigitGenerator : Random.Generator String
nonZeroDigitGenerator =
    Random.int 1 9
        |> Random.map String.fromInt


digitGenerator : Random.Generator String
digitGenerator =
    Random.int 0 9
        |> Random.map String.fromInt


generateEither : a -> a -> Random.Generator a
generateEither first second =
    Random.uniform first [ second ]


randomListWithRandomLength : Int -> Int -> Random.Generator a -> Random.Generator (List a)
randomListWithRandomLength minLength maxLength generator =
    Random.int minLength maxLength
        |> Random.andThen (\length -> Random.list length generator)


appendGenerators : Random.Generator String -> Random.Generator String -> Random.Generator String
appendGenerators secondGenerator firstGenerator =
    firstGenerator
        |> Random.andThen (\first -> Random.map (\second -> first ++ second) secondGenerator)


stringGenerator : Random.Generator String
stringGenerator =
    Random.String.string 255 Random.Char.english


cambiatusUrlGenerator : Maybe String -> Random.Generator Url.Url
cambiatusUrlGenerator maybeAuthority =
    let
        protocolGenerator =
            generateEither Url.Http Url.Https

        host =
            case maybeAuthority of
                Nothing ->
                    stringGenerator
                        |> appendGenerators
                            (Random.uniform ".cambiatus.io"
                                [ ".demo.cambiatus.io"
                                , ".staging.cambiatus.io"
                                , ".localhost"
                                , ".staging.localhost"
                                ]
                            )

                Just authority ->
                    stringGenerator
                        |> appendGenerators (Random.constant authority)

        pathGenerator =
            randomListWithRandomLength 0 5 stringGenerator
                |> Random.map (String.join "/")
                |> Random.map (\path -> "/" ++ path)

        queryGenerator =
            stringGenerator
                |> Random.map (\queryKey -> queryKey ++ "=")
                |> appendGenerators stringGenerator
                |> randomListWithRandomLength 0 5
                |> Random.map (String.join "&")
                |> Random.map
                    (\queryString ->
                        if String.isEmpty queryString then
                            Nothing

                        else
                            Just queryString
                    )

        fragmentGenerator =
            stringGenerator
                |> Random.map (\fragmentString -> fragmentString |> Just)
                |> List.singleton
                |> Random.uniform (Random.constant Nothing)
                |> Random.andThen identity
    in
    Random.constant Url.Url
        |> randomAndMap protocolGenerator
        |> randomAndMap host
        |> randomAndMap (Random.constant Nothing)
        |> randomAndMap pathGenerator
        |> randomAndMap queryGenerator
        |> randomAndMap fragmentGenerator
        |> Random.map
            (\url ->
                if String.endsWith "localhost" url.host then
                    { url | port_ = Just 3000 }

                else
                    { url | port_ = Nothing }
            )



-- SHRINKERS


urlShrinker : Shrink.Shrinker Url.Url
urlShrinker url =
    Shrink.map Url.Url (Shrink.noShrink url.protocol)
        |> Shrink.andMap (Shrink.string url.host)
        |> Shrink.andMap (Shrink.noShrink url.port_)
        |> Shrink.andMap (Shrink.string url.path)
        |> Shrink.andMap (Shrink.maybe Shrink.string url.query)
        |> Shrink.andMap (Shrink.maybe Shrink.string url.fragment)



-- FUZZERS


cambiatusUrlFuzzer : Maybe String -> Fuzz.Fuzzer Url.Url
cambiatusUrlFuzzer maybeAuthority =
    Fuzz.custom (cambiatusUrlGenerator maybeAuthority) urlShrinker
