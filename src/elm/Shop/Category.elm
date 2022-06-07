module Shop.Category exposing (Model, Tree, selectionSet)

import Cambiatus.Object
import Cambiatus.Object.Category
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Tree



-- MODEL


type alias Model =
    { id : Id
    , name : String

    -- TODO - Should this be `Markdown`?
    , description : String
    , icon : Maybe String
    , image : Maybe String
    }


selectionSet : SelectionSet Model Cambiatus.Object.Category
selectionSet =
    SelectionSet.succeed Model
        |> SelectionSet.with idSelectionSet
        |> SelectionSet.with Cambiatus.Object.Category.name
        |> SelectionSet.with Cambiatus.Object.Category.description
        |> SelectionSet.with Cambiatus.Object.Category.iconUri
        |> SelectionSet.with Cambiatus.Object.Category.imageUri



-- TREE


type alias Tree =
    -- TODO - Do we want this type alias?
    Tree.Tree Model


treeSelectionSet : SelectionSet Tree Cambiatus.Object.Category
treeSelectionSet =
    let
        unfolder : TreeSeed -> ( Model, List TreeSeed )
        unfolder (TreeSeed root) =
            ( root.model, root.children )
    in
    treeSeedSelectionSet maxDepth
        |> SelectionSet.map (\root -> Tree.unfold unfolder root)


type TreeSeed
    = TreeSeed
        { model : Model
        , children : List TreeSeed
        }


treeSeedSelectionSet : Int -> SelectionSet TreeSeed Cambiatus.Object.Category
treeSeedSelectionSet remainingChildren =
    let
        childrenSelectionSet =
            if remainingChildren == 0 then
                SelectionSet.succeed []

            else
                treeSeedSelectionSet (remainingChildren - 1)
                    |> Cambiatus.Object.Category.categories
                    |> SelectionSet.map (Maybe.withDefault [])
    in
    SelectionSet.succeed
        (\model children ->
            TreeSeed
                { model = model
                , children = children
                }
        )
        |> SelectionSet.with selectionSet
        |> SelectionSet.with childrenSelectionSet



-- ID


type Id
    = Id Int


idSelectionSet : SelectionSet Id Cambiatus.Object.Category
idSelectionSet =
    SelectionSet.map Id Cambiatus.Object.Category.id



-- INTERNAL


maxDepth : Int
maxDepth =
    5
