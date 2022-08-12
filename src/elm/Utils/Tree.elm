module Utils.Tree exposing (findInForest, takeFirst)

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


takeFirst : Int -> List (Tree.Tree a) -> List (Tree.Tree a)
takeFirst n children =
    takeFirstHelp n children
        |> Tuple.second


takeFirstHelp : Int -> List (Tree.Tree a) -> ( Int, List (Tree.Tree a) )
takeFirstHelp n children =
    if n == 0 then
        ( 0, [] )

    else
        children
            |> List.reverse
            |> List.foldr
                (\child ( currN, currChildren ) ->
                    if currN == 0 then
                        ( 0, currChildren )

                    else
                        let
                            ( newN, newChildren ) =
                                takeFirstHelp (currN - 1) (Tree.children child)
                        in
                        ( newN
                        , Tree.tree (Tree.label child) newChildren :: currChildren
                        )
                )
                ( n, [] )
            |> Tuple.mapSecond List.reverse
