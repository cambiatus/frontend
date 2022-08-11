module Utils.Tree exposing (takeFirst)

import Tree


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
