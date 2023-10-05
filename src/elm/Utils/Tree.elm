module Utils.Tree exposing
    ( findInForest, findZipperInForest, getAllAncestors
    , toFlatForest, fromFlatForest
    , goUp, goDown, InsertAt(..)
    , moveZipperToAfter, moveZipperToFirstChildOf, moveZipperToLastChildOf, moveZipperToFirstRootPosition
    )

{-| Helper functions that deal with Trees from zwilias/elm-rosetree


## Finding elements

@docs findInForest, findZipperInForest, getAllAncestors


## Transforming from and to Zipper

@docs toFlatForest, fromFlatForest


## Traversing through the tree

@docs goUp, goDown, InsertAt


## Rearranging trees/zippers

@docs moveZipperToAfter, moveZipperToFirstChildOf, moveZipperToLastChildOf, moveZipperToFirstRootPosition

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


type InsertAt a
    = FirstRoot
    | After (Tree.Zipper.Zipper a)
    | FirstChildOf (Tree.Zipper.Zipper a)


goUp : Tree.Zipper.Zipper a -> Maybe (InsertAt a)
goUp zipper =
    case Tree.Zipper.previousSibling zipper of
        Just previousSibling ->
            previousSibling
                |> Tree.Zipper.lastDescendant
                |> FirstChildOf
                |> Just

        Nothing ->
            case Tree.Zipper.parent zipper of
                Just parent ->
                    case Tree.Zipper.previousSibling parent of
                        Just previousSibling ->
                            Just (After previousSibling)

                        Nothing ->
                            case Tree.Zipper.parent parent of
                                Just grandParent ->
                                    Just (FirstChildOf grandParent)

                                Nothing ->
                                    Just FirstRoot

                Nothing ->
                    -- There is no previous sibling and no parent, so we are at the first root element
                    Nothing


goDown : Tree.Zipper.Zipper a -> Maybe (InsertAt a)
goDown zipper =
    case Tree.Zipper.nextSibling zipper of
        Just nextSibling ->
            Just (FirstChildOf nextSibling)

        Nothing ->
            case Tree.Zipper.parent zipper of
                Just parent ->
                    Just (After parent)

                Nothing ->
                    Nothing


moveZipperToAfter : id -> (model -> id) -> Tree.Zipper.Zipper model -> Maybe (Tree.Zipper.Zipper model)
moveZipperToAfter target toId zipper =
    zipper
        |> Tree.Zipper.removeTree
        |> Maybe.andThen (Tree.Zipper.findFromRoot (\model -> toId model == target))
        |> Maybe.andThen (Tree.Zipper.append (Tree.Zipper.tree zipper) >> Tree.Zipper.nextSibling)


moveZipperToFirstChildOf : id -> (model -> id) -> Tree.Zipper.Zipper model -> Maybe (Tree.Zipper.Zipper model)
moveZipperToFirstChildOf target toId zipper =
    zipper
        |> Tree.Zipper.removeTree
        |> Maybe.andThen (Tree.Zipper.findFromRoot (\model -> toId model == target))
        |> Maybe.andThen
            (Tree.Zipper.mapTree (Tree.prependChild (Tree.Zipper.tree zipper))
                >> Tree.Zipper.firstChild
            )


moveZipperToLastChildOf : id -> (model -> id) -> Tree.Zipper.Zipper model -> Maybe (Tree.Zipper.Zipper model)
moveZipperToLastChildOf target toId zipper =
    zipper
        |> Tree.Zipper.removeTree
        |> Maybe.andThen (Tree.Zipper.findFromRoot (\model -> toId model == target))
        |> Maybe.andThen
            (Tree.Zipper.mapTree (Tree.appendChild (Tree.Zipper.tree zipper))
                >> Tree.Zipper.lastChild
            )


moveZipperToFirstRootPosition : Tree.Zipper.Zipper model -> Maybe (Tree.Zipper.Zipper model)
moveZipperToFirstRootPosition zipper =
    zipper
        |> Tree.Zipper.removeTree
        |> Maybe.andThen
            (Tree.Zipper.root
                >> Tree.Zipper.prepend (Tree.Zipper.tree zipper)
                >> Tree.Zipper.previousSibling
            )
