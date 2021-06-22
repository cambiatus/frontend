module TestUtils exposing
    ( appendGenerators
    , digitGenerator
    , generateEither
    , nonZeroDigitGenerator
    , randomListWithRandomLength
    )

import Random



-- RANDOM GENERATORS


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
