module NewsMovedTests exposing (all)

import Expect
import NewsMoved
import Test exposing (..)


all : Test
all =
    describe "NewsMoved"
        [ test "one story" <|
            \_ ->
                NewsMoved.newsUrl "https://muda.cambiatus.io/" (Just 5)
                    |> Expect.equal "https://muda.cambiatus.io/app/community/news/5"
        , test "the newspaper" <|
            \_ ->
                NewsMoved.newsUrl "https://muda.cambiatus.io" Nothing
                    |> Expect.equal "https://muda.cambiatus.io/app/community/news"
        ]
