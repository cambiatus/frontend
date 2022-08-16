module Utils.TreeTests exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Tree
import Tree.Zipper
import Utils.Tree


all : Test
all =
    describe "Utils.Tree"
        [ findInForest
        , findZipperInForest
        , getAllAncestors
        , toFlatForest
        , fromFlatForest
        ]


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


treesZipper : Tree.Zipper.Zipper Int
treesZipper =
    Tree.Zipper.fromForest firstTree [ secondTree ]


findInForest : Test
findInForest =
    describe "findInForest"
        [ test "returns Nothing for element not present" <|
            \_ ->
                Utils.Tree.findInForest ((==) 50) trees
                    |> Expect.equal Nothing
        , test "returns element if present - root" <|
            \_ ->
                Utils.Tree.findInForest ((==) 0) trees
                    |> Expect.equal (Just 0)
        , test "returns element if present - first child" <|
            \_ ->
                Utils.Tree.findInForest ((==) -100) trees
                    |> Expect.equal (Just -100)
        , test "returns element if present - second child" <|
            \_ ->
                Utils.Tree.findInForest ((==) 1) trees
                    |> Expect.equal (Just 1)
        , test "returns element if present - grandchild" <|
            \_ ->
                Utils.Tree.findInForest ((==) 10) trees
                    |> Expect.equal (Just 10)
        ]


findZipperInForest : Test
findZipperInForest =
    let
        expectLabel expectedLabel =
            Maybe.map Tree.Zipper.label
                >> Expect.equal (Just expectedLabel)
    in
    describe "findZipperInForest"
        [ test "returns Nothing for element not present" <|
            \_ ->
                Utils.Tree.findZipperInForest ((==) 50) trees
                    |> Expect.equal Nothing
        , test "returns element if present - root" <|
            \_ ->
                Utils.Tree.findZipperInForest ((==) 0) trees
                    |> expectLabel 0
        , test "returns element if present - first child" <|
            \_ ->
                Utils.Tree.findZipperInForest ((==) -100) trees
                    |> expectLabel -100
        , test "returns element if present - second child" <|
            \_ ->
                Utils.Tree.findZipperInForest ((==) 1) trees
                    |> expectLabel 1
        , test "returns element if present - grandchild" <|
            \_ ->
                Utils.Tree.findZipperInForest ((==) 10) trees
                    |> expectLabel 10
        ]


getAllAncestors : Test
getAllAncestors =
    let
        expectAncestors : List a -> Maybe (Tree.Zipper.Zipper a) -> Expect.Expectation
        expectAncestors expectedLabels targetZipper =
            case targetZipper of
                Nothing ->
                    Expect.fail "targetZipper is Nothing"

                Just zipper ->
                    Utils.Tree.getAllAncestors zipper
                        |> List.map Tree.Zipper.label
                        |> Expect.equal expectedLabels
    in
    describe "getAllAncestors"
        [ test "returns empty list for root" <|
            \_ ->
                treesZipper
                    |> Tree.Zipper.root
                    |> Utils.Tree.getAllAncestors
                    |> Expect.equal []
        , test "returns list of ancestors for first child" <|
            \_ ->
                treesZipper
                    |> Tree.Zipper.forward
                    |> expectAncestors [ 0 ]
        , test "returns list of ancestors for grandchild" <|
            \_ ->
                treesZipper
                    |> Tree.Zipper.findFromRoot ((==) 10)
                    |> expectAncestors [ 0, 1 ]
        , test "returns list of ancestors for second grandchild of second tree" <|
            \_ ->
                treesZipper
                    |> Tree.Zipper.findFromRoot ((==) 120)
                    |> expectAncestors [ 100, 101 ]
        ]


toFlatForest : Test
toFlatForest =
    describe "toFlatForest"
        [ test "returns trees for treesZipper" <|
            \_ ->
                treesZipper
                    |> Utils.Tree.toFlatForest
                    |> Expect.equal trees
        , test "returns firstTree for firstTree zipper" <|
            \_ ->
                firstTree
                    |> Tree.Zipper.fromTree
                    |> Utils.Tree.toFlatForest
                    |> Expect.equal [ firstTree ]
        , test "returns secondTree for secondTree zipper" <|
            \_ ->
                secondTree
                    |> Tree.Zipper.fromTree
                    |> Utils.Tree.toFlatForest
                    |> Expect.equal [ secondTree ]
        ]


fromFlatForest : Test
fromFlatForest =
    describe "fromFlatForest"
        [ test "returns Nothing for empty list" <|
            \_ ->
                Utils.Tree.fromFlatForest []
                    |> Expect.equal Nothing
        , test "returns treesZipper for trees" <|
            \_ ->
                trees
                    |> Utils.Tree.fromFlatForest
                    |> Expect.equal (Just treesZipper)
        , test "returns firstTree zipper for firstTree" <|
            \_ ->
                [ firstTree ]
                    |> Utils.Tree.fromFlatForest
                    |> Expect.equal (Just (Tree.Zipper.fromTree firstTree))
        , test "returns secondTree zipper for secondTree" <|
            \_ ->
                [ secondTree ]
                    |> Utils.Tree.fromFlatForest
                    |> Expect.equal (Just (Tree.Zipper.fromTree secondTree))
        ]
