module Shop.Category exposing (Id, Model, Tree, create, selectionSet, treesSelectionSet)

import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.Category
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Maybe.Extra
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
    , parentId : Maybe Id
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
        |> SelectionSet.with (Cambiatus.Object.Category.parentCategory idSelectionSet)



-- TREE


type alias Tree =
    -- TODO - Do we want this type alias?
    Tree.Tree Model


treesSelectionSet :
    (SelectionSet Model Cambiatus.Object.Category -> SelectionSet (List Model) typeLock)
    -> SelectionSet (List Tree) typeLock
treesSelectionSet categoriesSelectionSet =
    let
        isRoot : Model -> Bool
        isRoot category =
            Maybe.Extra.isNothing category.parentId

        createTree : List Model -> Model -> Tree
        createTree children root =
            Tree.tree root
                (List.filterMap
                    (\child ->
                        if child.parentId == Just root.id then
                            Just (createTree children child)

                        else
                            Nothing
                    )
                    children
                )
    in
    categoriesSelectionSet selectionSet
        |> SelectionSet.map
            (\allCategories ->
                List.filterMap
                    (\category ->
                        if isRoot category then
                            Just (createTree allCategories category)

                        else
                            Nothing
                    )
                    allCategories
            )



-- ID


type Id
    = Id Int


idSelectionSet : SelectionSet Id Cambiatus.Object.Category
idSelectionSet =
    SelectionSet.map Id Cambiatus.Object.Category.id
