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
        , moveZipperToAfter
        , moveZipperToFirstChildOf
        , moveZipperToLastChildOf
        , moveZipperToFirstRootPosition
        , goUp
        ]


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


moveZipperToAfter : Test
moveZipperToAfter =
    let
        go :
            { startingPoint : Int
            , target : Int
            , expected : ( Tree.Tree Int, List (Tree.Tree Int) )
            }
            -> Expect.Expectation
        go { startingPoint, target, expected } =
            case Tree.Zipper.findFromRoot ((==) startingPoint) treesZipper of
                Nothing ->
                    Expect.fail ("Could not find starting point: " ++ String.fromInt startingPoint)

                Just zipper ->
                    case Utils.Tree.moveZipperToAfter target identity zipper of
                        Nothing ->
                            Expect.fail "returned Nothing"

                        Just newZipper ->
                            Expect.all
                                [ Tree.Zipper.toForest
                                    >> Expect.equal expected
                                , Tree.Zipper.label
                                    >> Expect.equal startingPoint
                                ]
                                newZipper
    in
    describe "moveZipperToAfter"
        [ test "can move to after sibling" <|
            \_ ->
                go
                    { startingPoint = 0
                    , target = 100
                    , expected = ( secondTree, [ firstTree ] )
                    }
        , test "can move to sibling's child" <|
            \_ ->
                go
                    { startingPoint = 0
                    , target = -100
                    , expected =
                        ( Tree.tree 100
                            [ treeNegative100
                            , firstTree
                            , tree101
                            ]
                        , []
                        )
                    }
        , test "can move to sibling's grandchild" <|
            \_ ->
                go
                    { startingPoint = 0
                    , target = 110
                    , expected =
                        ( Tree.tree 100
                            [ treeNegative100
                            , Tree.tree 101
                                [ Tree.tree 110 []
                                , firstTree
                                , Tree.tree 120 []
                                ]
                            ]
                        , []
                        )
                    }
        , test "can move child to after itself" <|
            \_ ->
                go
                    { startingPoint = -1
                    , target = 0
                    , expected =
                        ( Tree.tree 0 [ tree1 ]
                        , [ treeNegative1, secondTree ]
                        )
                    }
        , test "can move child to after sibling" <|
            \_ ->
                go
                    { startingPoint = -1
                    , target = 100
                    , expected =
                        ( Tree.tree 0 [ tree1 ]
                        , [ secondTree, treeNegative1 ]
                        )
                    }
        , test "can move child to sibling's children" <|
            \_ ->
                go
                    { startingPoint = 1
                    , target = -100
                    , expected =
                        ( Tree.tree 0 [ treeNegative1 ]
                        , [ Tree.tree 100
                                [ treeNegative100
                                , tree1
                                , tree101
                                ]
                          ]
                        )
                    }
        , test "can move child to sibling's grandchild" <|
            \_ ->
                go
                    { startingPoint = 1
                    , target = -110
                    , expected =
                        ( Tree.tree 0 [ treeNegative1 ]
                        , [ Tree.tree 100
                                [ Tree.tree -100
                                    [ Tree.tree -110 []
                                    , tree1
                                    , Tree.tree -120 []
                                    ]
                                , tree101
                                ]
                          ]
                        )
                    }
        ]


moveZipperToFirstChildOf : Test
moveZipperToFirstChildOf =
    let
        go :
            { startingPoint : Int
            , target : Int
            , expected : ( Tree.Tree Int, List (Tree.Tree Int) )
            }
            -> Expect.Expectation
        go { startingPoint, target, expected } =
            case Tree.Zipper.findFromRoot ((==) startingPoint) treesZipper of
                Nothing ->
                    Expect.fail ("Could not find starting point: " ++ String.fromInt startingPoint)

                Just zipper ->
                    case Utils.Tree.moveZipperToFirstChildOf target identity zipper of
                        Nothing ->
                            Expect.fail "returned Nothing"

                        Just newZipper ->
                            Expect.all
                                [ Tree.Zipper.toForest
                                    >> Expect.equal expected
                                , Tree.Zipper.label
                                    >> Expect.equal startingPoint
                                ]
                                newZipper
    in
    describe "moveZipperToFirstChildOf"
        [ test "can move to next sibling's first child" <|
            \_ ->
                go
                    { startingPoint = 0
                    , target = 100
                    , expected =
                        ( Tree.tree 100
                            [ firstTree
                            , treeNegative100
                            , tree101
                            ]
                        , []
                        )
                    }
        , test "can move to previous sibling's first child" <|
            \_ ->
                go
                    { startingPoint = 100
                    , target = 0
                    , expected =
                        ( Tree.tree 0
                            [ secondTree
                            , treeNegative1
                            , tree1
                            ]
                        , []
                        )
                    }
        , test "can move to sibling's grandchild" <|
            \_ ->
                go
                    { startingPoint = 0
                    , target = -100
                    , expected =
                        ( Tree.tree 100
                            [ Tree.tree -100
                                [ firstTree
                                , Tree.tree -110 []
                                , Tree.tree -120 []
                                ]
                            , tree101
                            ]
                        , []
                        )
                    }
        , test "can move to node that has no children" <|
            \_ ->
                go
                    { startingPoint = 0
                    , target = -110
                    , expected =
                        ( Tree.tree 100
                            [ Tree.tree -100
                                [ Tree.tree -110 [ firstTree ]
                                , Tree.tree -120 []
                                ]
                            , tree101
                            ]
                        , []
                        )
                    }
        ]


moveZipperToLastChildOf : Test
moveZipperToLastChildOf =
    let
        go :
            { startingPoint : Int
            , target : Int
            , expected : ( Tree.Tree Int, List (Tree.Tree Int) )
            }
            -> Expect.Expectation
        go { startingPoint, target, expected } =
            case Tree.Zipper.findFromRoot ((==) startingPoint) treesZipper of
                Nothing ->
                    Expect.fail ("Could not find starting point: " ++ String.fromInt startingPoint)

                Just zipper ->
                    case Utils.Tree.moveZipperToLastChildOf target identity zipper of
                        Nothing ->
                            Expect.fail "returned Nothing"

                        Just newZipper ->
                            Expect.all
                                [ Tree.Zipper.toForest
                                    >> Expect.equal expected
                                , Tree.Zipper.label
                                    >> Expect.equal startingPoint
                                ]
                                newZipper
    in
    describe "moveZipperToLastChildOf"
        [ test "can move to next sibling's last child" <|
            \_ ->
                go
                    { startingPoint = 0
                    , target = 100
                    , expected =
                        ( Tree.tree 100
                            [ treeNegative100
                            , tree101
                            , firstTree
                            ]
                        , []
                        )
                    }
        , test "can move to previous sibling's last child" <|
            \_ ->
                go
                    { startingPoint = 100
                    , target = 0
                    , expected =
                        ( Tree.tree 0
                            [ treeNegative1
                            , tree1
                            , secondTree
                            ]
                        , []
                        )
                    }
        , test "can move to sibling's grandchild" <|
            \_ ->
                go
                    { startingPoint = 0
                    , target = -100
                    , expected =
                        ( Tree.tree 100
                            [ Tree.tree -100
                                [ Tree.tree -110 []
                                , Tree.tree -120 []
                                , firstTree
                                ]
                            , tree101
                            ]
                        , []
                        )
                    }
        , test "can move to node that has no children" <|
            \_ ->
                go
                    { startingPoint = 0
                    , target = -110
                    , expected =
                        ( Tree.tree 100
                            [ Tree.tree -100
                                [ Tree.tree -110 [ firstTree ]
                                , Tree.tree -120 []
                                ]
                            , tree101
                            ]
                        , []
                        )
                    }
        ]


moveZipperToFirstRootPosition : Test
moveZipperToFirstRootPosition =
    let
        go :
            { startingPoint : Int
            , expected : ( Tree.Tree Int, List (Tree.Tree Int) )
            }
            -> Expect.Expectation
        go { startingPoint, expected } =
            case Tree.Zipper.findFromRoot ((==) startingPoint) treesZipper of
                Nothing ->
                    Expect.fail ("Could not find starting point: " ++ String.fromInt startingPoint)

                Just zipper ->
                    case Utils.Tree.moveZipperToFirstRootPosition zipper of
                        Nothing ->
                            Expect.fail "returned Nothing"

                        Just newZipper ->
                            Expect.all
                                [ Tree.Zipper.toForest
                                    >> Expect.equal expected
                                , Tree.Zipper.label
                                    >> Expect.equal startingPoint
                                ]
                                newZipper
    in
    describe "moveZipperToFirstRootPosition"
        [ test "no-op if zipper is already in first root position" <|
            \_ ->
                go
                    { startingPoint = 0
                    , expected = ( firstTree, [ secondTree ] )
                    }
        , test "can move root node to first root position" <|
            \_ ->
                go
                    { startingPoint = 100
                    , expected = ( secondTree, [ firstTree ] )
                    }
        , test "can move child node to first root position" <|
            \_ ->
                go
                    { startingPoint = -1
                    , expected =
                        ( treeNegative1
                        , [ Tree.tree 0 [ tree1 ]
                          , secondTree
                          ]
                        )
                    }
        , test "can move grandchild to first root position" <|
            \_ ->
                go
                    { startingPoint = -110
                    , expected =
                        ( Tree.tree -110 []
                        , [ firstTree
                          , Tree.tree 100
                                [ Tree.tree -100 [ Tree.tree -120 [] ]
                                , tree101
                                ]
                          ]
                        )
                    }
        ]


type GoUpResult
    = FirstRoot
    | FirstChildOf Int
    | After Int


goUp : Test
goUp =
    let
        go :
            { target : Int
            , expected : Maybe GoUpResult
            }
            -> Expect.Expectation
        go { target, expected } =
            case Tree.Zipper.findFromRoot ((==) target) treesZipper of
                Nothing ->
                    Expect.fail ("Could not find starting point: " ++ String.fromInt target)

                Just zipper ->
                    Utils.Tree.goUp zipper
                        |> Expect.equal
                            (case expected of
                                Just FirstRoot ->
                                    Utils.Tree.FirstRoot
                                        |> Just

                                Just (FirstChildOf label) ->
                                    treesZipper
                                        |> Tree.Zipper.findFromRoot ((==) label)
                                        |> Maybe.withDefault treesZipper
                                        |> Utils.Tree.FirstChildOf
                                        |> Just

                                Just (After label) ->
                                    treesZipper
                                        |> Tree.Zipper.findFromRoot ((==) label)
                                        |> Maybe.withDefault treesZipper
                                        |> Utils.Tree.After
                                        |> Just

                                Nothing ->
                                    Nothing
                            )
    in
    describe "goUp"
        [ test "can move up to a previous sibling's child" <|
            \_ ->
                go
                    { target = -20
                    , expected = Just (FirstChildOf -10)
                    }
        , test "can move up to first child" <|
            \_ ->
                go
                    { target = -10
                    , expected = Just (FirstChildOf 0)
                    }
        , test "can move up to first root" <|
            \_ ->
                go
                    { target = -1
                    , expected = Just FirstRoot
                    }
        , test "can not move up if it's the first root category" <|
            \_ ->
                go
                    { target = 0
                    , expected = Nothing
                    }
        , test "can move up to previous sibling's last descendant" <|
            \_ ->
                go
                    { target = 1
                    , expected = Just (FirstChildOf -20)
                    }
        , test "can move up to previous sibling's far last descendant" <|
            \_ ->
                go
                    { target = 100
                    , expected = Just (FirstChildOf 20)
                    }
        , test "can move up to previous sibling's first descendant in root" <|
            \_ ->
                go
                    { target = -100
                    , expected = Just (After 0)
                    }
        ]



-- VALUE HELPERS


firstTree : Tree.Tree Int
firstTree =
    Tree.tree 0
        [ treeNegative1
        , tree1
        ]


secondTree : Tree.Tree Int
secondTree =
    Tree.tree 100
        [ treeNegative100
        , tree101
        ]


treeNegative1 : Tree.Tree Int
treeNegative1 =
    Tree.tree -1
        [ Tree.tree -10 []
        , Tree.tree -20 []
        ]


tree1 : Tree.Tree Int
tree1 =
    Tree.tree 1
        [ Tree.tree 10 []
        , Tree.tree 20 []
        ]


treeNegative100 : Tree.Tree Int
treeNegative100 =
    Tree.tree -100
        [ Tree.tree -110 []
        , Tree.tree -120 []
        ]


tree101 : Tree.Tree Int
tree101 =
    Tree.tree 101
        [ Tree.tree 110 []
        , Tree.tree 120 []
        ]


{-|

    [ 0
        [ -1 [ -10, -20 ]
        , 1 [ 10, 20 ]
        ]
    , 100
        [ -100 [ -110, -120 ]
        , 101 [ 110, 120 ]
        ]
    ]

-}
trees : List (Tree.Tree Int)
trees =
    [ firstTree
    , secondTree
    ]


treesZipper : Tree.Zipper.Zipper Int
treesZipper =
    Tree.Zipper.fromForest firstTree [ secondTree ]
