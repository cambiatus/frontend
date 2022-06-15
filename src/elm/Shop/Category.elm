module Shop.Category exposing (Id, Model, Tree, addChild, create, delete, encodeId, idSelectionSet, moveToRoot, selectionSet, treesSelectionSet, update, updateMetadata)

import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.Category
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Encode
import Markdown exposing (Markdown)
import Maybe.Extra
import Slug exposing (Slug)
import Tree



-- MODEL


type alias Model =
    { id : Id
    , name : String
    , slug : Maybe Slug
    , description : Markdown
    , icon : Maybe String
    , image : Maybe String
    , parentId : Maybe Id
    }


create :
    { name : String
    , description : Markdown
    , slug : Slug
    , parentId : Maybe Id
    }
    -> SelectionSet decodesTo Cambiatus.Object.Category
    -> SelectionSet (Maybe decodesTo) RootMutation
create { name, slug, description, parentId } =
    Cambiatus.Mutation.category
        (\optionals ->
            { optionals
                | parentId =
                    parentId
                        |> Maybe.map (\(Id id) -> id)
                        |> OptionalArgument.fromMaybe
                , slug = OptionalArgument.Present (Slug.toString slug)
                , name = OptionalArgument.Present name
                , description = OptionalArgument.Present (Markdown.toRawString description)
            }
        )


update :
    Model
    -> { name : String, description : Markdown, slug : Slug }
    -> SelectionSet decodesTo Cambiatus.Object.Category
    -> SelectionSet (Maybe decodesTo) RootMutation
update model { name, description, slug } =
    Cambiatus.Mutation.category
        (\optionals ->
            { optionals
                | id = OptionalArgument.Present (unwrapId model.id)
                , slug = OptionalArgument.Present (Slug.toString slug)
                , name = OptionalArgument.Present name
                , description = OptionalArgument.Present (Markdown.toRawString description)
            }
        )


updateMetadata :
    Model
    -> { metaTitle : String, metaDescription : String, metaKeywords : String }
    -> SelectionSet decodesTo Cambiatus.Object.Category
    -> SelectionSet (Maybe decodesTo) RootMutation
updateMetadata model { metaTitle, metaDescription, metaKeywords } =
    Cambiatus.Mutation.category
        (\optionals ->
            { optionals
                | id = OptionalArgument.Present (unwrapId model.id)
                , metaDescription = OptionalArgument.Present metaDescription
                , metaKeywords = OptionalArgument.Present metaKeywords
                , metaTitle = OptionalArgument.Present metaTitle
            }
        )


addChild : Tree -> Id -> SelectionSet decodesTo Cambiatus.Object.Category -> SelectionSet (Maybe decodesTo) RootMutation
addChild tree (Id newChildId) =
    Cambiatus.Mutation.category
        (\optionals ->
            { optionals
                | categories =
                    Tree.children tree
                        |> List.map
                            (Tree.label
                                >> .id
                                >> unwrapId
                                >> (\id -> { id = id })
                            )
                        |> (\existingChildren -> existingChildren ++ [ { id = newChildId } ])
                        |> OptionalArgument.Present
                , id =
                    Tree.label tree
                        |> .id
                        |> unwrapId
                        |> OptionalArgument.Present
            }
        )


moveToRoot : Id -> SelectionSet decodesTo Cambiatus.Object.Category -> SelectionSet (Maybe decodesTo) RootMutation
moveToRoot (Id id) =
    Cambiatus.Mutation.category
        (\optionals ->
            { optionals
                | id = OptionalArgument.Present id
                , parentId = OptionalArgument.Null
            }
        )


delete :
    Id
    -> SelectionSet decodesTo Cambiatus.Object.DeleteStatus
    -> SelectionSet (Maybe decodesTo) RootMutation
delete (Id id) =
    Cambiatus.Mutation.deleteCategory { id = id }


selectionSet : SelectionSet Model Cambiatus.Object.Category
selectionSet =
    SelectionSet.succeed Model
        |> SelectionSet.with idSelectionSet
        |> SelectionSet.with Cambiatus.Object.Category.name
        |> SelectionSet.with (Cambiatus.Object.Category.slug |> SelectionSet.map (Maybe.andThen Slug.parse))
        |> SelectionSet.with (Markdown.selectionSet Cambiatus.Object.Category.description)
        |> SelectionSet.with Cambiatus.Object.Category.iconUri
        |> SelectionSet.with Cambiatus.Object.Category.imageUri
        |> SelectionSet.with (Cambiatus.Object.Category.parent idSelectionSet)



-- TREE


type alias Tree =
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


encodeId : Id -> Json.Encode.Value
encodeId (Id id) =
    Json.Encode.int id


unwrapId : Id -> Int
unwrapId (Id id) =
    id
