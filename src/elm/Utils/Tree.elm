module Utils.Tree exposing
    ( findInTrees, findInForest, getAllAncestors
    , toFlatForest, fromFlatForest
    , goUpWithoutChildren, goDownWithoutChildren
    )

{-| Helper functions that deal with Trees from zwilias/elm-rosetree


## Finding elements

@docs findInTrees, findInForest, getAllAncestors


## Transforming from and to Zipper

@docs toFlatForest, fromFlatForest


## Traversing through the tree

@docs goUpWithoutChildren, goDownWithoutChildren

-}

-- TODO - Create tests

import List.Extra
import Tree
import Tree.Zipper


findInTrees : (a -> Bool) -> List (Tree.Tree a) -> Maybe a
findInTrees fn trees =
    trees
        |> List.concatMap Tree.flatten
        |> List.Extra.find fn


findInForest : (a -> Bool) -> List (Tree.Tree a) -> Maybe (Tree.Zipper.Zipper a)
findInForest fn trees =
    case trees of
        [] ->
            Nothing

        firstTree :: otherTrees ->
            Tree.Zipper.fromForest firstTree otherTrees
                |> Tree.Zipper.findFromRoot fn


getAllAncestors : Tree.Zipper.Zipper a -> List (Tree.Zipper.Zipper a)
getAllAncestors zipper =
    getAllAncestorsHelper zipper []


getAllAncestorsHelper : Tree.Zipper.Zipper a -> List (Tree.Zipper.Zipper a) -> List (Tree.Zipper.Zipper a)
getAllAncestorsHelper zipper ancestors =
    case Tree.Zipper.parent zipper of
        Nothing ->
            ancestors

        Just parent ->
            getAllAncestorsHelper parent (parent :: ancestors)


toFlatForest : Tree.Zipper.Zipper a -> List (Tree.Tree a)
toFlatForest zipper =
    zipper
        |> Tree.Zipper.toForest
        |> (\( first, others ) -> first :: others)


fromFlatForest : List (Tree.Tree a) -> Maybe (Tree.Zipper.Zipper a)
fromFlatForest trees =
    case trees of
        first :: others ->
            Just (Tree.Zipper.fromForest first others)

        [] ->
            Nothing


goUpWithoutChildren : Tree.Zipper.Zipper a -> Maybe (Tree.Zipper.Zipper a)
goUpWithoutChildren zipper =
    Tree.Zipper.backward zipper
        |> Maybe.andThen
            (\backwardZipper ->
                if Tree.Zipper.parent zipper == Just backwardZipper then
                    Tree.Zipper.parent backwardZipper

                else
                    Just backwardZipper
            )


goDownWithoutChildren : Tree.Zipper.Zipper a -> Maybe (Tree.Zipper.Zipper a)
goDownWithoutChildren zipper =
    case Tree.Zipper.nextSibling zipper of
        Nothing ->
            Tree.Zipper.parent zipper
                |> Maybe.andThen Tree.Zipper.parent

        Just firstSibling ->
            Just firstSibling
