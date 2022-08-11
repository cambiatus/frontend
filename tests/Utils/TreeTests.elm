module Utils.TreeTests exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Tree
import Utils.Tree


all : Test
all =
    describe "Utils.Tree"
        [ takeFirst ]


firstTree : Tree.Tree Int
firstTree =
    Tree.tree 0
        [ Tree.tree -1
            [ Tree.tree -10 []
            , Tree.tree -20 []
            ]
        , Tree.tree 1
            [ Tree.tree 10 []
            , Tree.tree 20 []
            ]
        ]


secondTree : Tree.Tree Int
secondTree =
    Tree.tree 100
        [ Tree.tree -100
            [ Tree.tree -110 []
            , Tree.tree -120 []
            ]
        , Tree.tree 101
            [ Tree.tree 110 []
            , Tree.tree 120 []
            ]
        ]


trees : List (Tree.Tree Int)
trees =
    [ firstTree
    , secondTree
    ]


takeFirst : Test
takeFirst =
    describe "takeFirst"
        [ test "returns the first 3 elements of first tree" <|
            \_ ->
                Utils.Tree.takeFirst 3 [ firstTree ]
                    |> Expect.equal [ Tree.tree 0 [ Tree.tree -1 [ Tree.tree -10 [] ] ] ]
        , test "returns all elements of first tree if n = 7" <|
            \_ ->
                Utils.Tree.takeFirst 7 [ firstTree ]
                    |> Expect.equal [ firstTree ]
        , test "returns all elements of first tree if n > 7" <|
            \_ ->
                Utils.Tree.takeFirst 10 [ firstTree ]
                    |> Expect.equal [ firstTree ]
        , test "returns all elements of first tree if n = 7 when giving it trees" <|
            \_ ->
                Utils.Tree.takeFirst 7 trees
                    |> Expect.equal [ firstTree ]
        , test "returns all elements of first tree plus some of secondTree" <|
            \_ ->
                Utils.Tree.takeFirst 10 trees
                    |> Expect.equal
                        [ firstTree
                        , Tree.tree 100 [ Tree.tree -100 [ Tree.tree -110 [] ] ]
                        ]
        , test "returns all elements of trees if n = 14" <|
            \_ ->
                Utils.Tree.takeFirst 14 trees
                    |> Expect.equal trees
        , test "returns all elements of trees if n > 14" <|
            \_ ->
                Utils.Tree.takeFirst 20 trees
                    |> Expect.equal trees
        ]
