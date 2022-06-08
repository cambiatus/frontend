module Shop.Category exposing (Id, Model, Tree, create, selectionSet, treeSelectionSet)

import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.Category
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Slug exposing (Slug)
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


create :
    { name : String
    , description : String
    , slug : Slug
    , icon : Maybe String
    , image : Maybe String
    , parentId : Maybe Id
    }
    -> SelectionSet (Maybe Model) RootMutation
create { name, slug, description, icon, image, parentId } =
    Cambiatus.Mutation.category
        (\_ ->
            { parentCategoryId =
                parentId
                    |> Maybe.map (\(Id id) -> id)
                    |> OptionalArgument.fromMaybe
            , iconUri = OptionalArgument.fromMaybe icon
            , imageUri = OptionalArgument.fromMaybe image
            , id = OptionalArgument.Absent
            , metaDescription = OptionalArgument.Absent
            , metaKeywords = OptionalArgument.Absent
            , metaTitle = OptionalArgument.Absent
            , slug = OptionalArgument.Present (Slug.toString slug)
            }
        )
        { name = name
        , description = description
        , categories = []
        }
        selectionSet


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
