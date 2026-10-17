module Utils.Tree exposing
    ( findInForest, findZipperInForest, getAllAncestors
    , toFlatForest, fromFlatForest
    )

{-| Helper functions that deal with Trees from zwilias/elm-rosetree


## Finding elements

@docs findInForest, findZipperInForest, getAllAncestors


## Transforming from and to Zipper

@docs toFlatForest, fromFlatForest


## Traversing through the tree

-}

import Tree
import Tree.Zipper


findInForest : (a -> Bool) -> List (Tree.Tree a) -> Maybe a
findInForest fn trees =
    findZipperInForest fn trees
        |> Maybe.map Tree.Zipper.label


findZipperInForest : (a -> Bool) -> List (Tree.Tree a) -> Maybe (Tree.Zipper.Zipper a)
findZipperInForest fn trees =
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
