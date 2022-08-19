module Shop.Category exposing
    ( Id
    , Model
    , Tree
    , addChild
    , create
    , delete
    , encodeId
    , idFromString
    , idSelectionSet
    , idToInt
    , idToString
    , moveToRoot
    , selectionSet
    , treesSelectionSet
    , update
    , updateMetadata
    )

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
    , metaTitle : Maybe String
    , metaDescription : Maybe String
    , metaKeywords : List String
    , position : Int
    }


create :
    { icon : Maybe String
    , name : String
    , description : Markdown
    , slug : Slug
    , image : Maybe String
    , parentId : Maybe Id
    , position : Int
    }
    -> SelectionSet decodesTo Cambiatus.Object.Category
    -> SelectionSet (Maybe decodesTo) RootMutation
create { icon, name, slug, description, image, parentId, position } =
    Cambiatus.Mutation.category
        (\optionals ->
            { optionals
                | parentId =
                    parentId
                        |> Maybe.map (\(Id id) -> id)
                        |> OptionalArgument.fromMaybe
                , iconUri = OptionalArgument.fromMaybe icon
                , slug = OptionalArgument.Present (Slug.toString slug)
                , name = OptionalArgument.Present name
                , description = OptionalArgument.Present (Markdown.toRawString description)
                , imageUri = OptionalArgument.fromMaybe image
                , position = OptionalArgument.Present position
            }
        )


update :
    Model
    ->
        { icon : Maybe String
        , name : String
        , description : Markdown
        , slug : Slug
        , image : Maybe String
        }
    -> SelectionSet decodesTo Cambiatus.Object.Category
    -> SelectionSet (Maybe decodesTo) RootMutation
update model { icon, name, description, slug, image } =
    Cambiatus.Mutation.category
        (\optionals ->
            { optionals
                | id = OptionalArgument.Present (idToInt model.id)
                , iconUri = OptionalArgument.fromMaybeWithNull icon
                , imageUri = OptionalArgument.fromMaybeWithNull image
                , slug = OptionalArgument.Present (Slug.toString slug)
                , name = OptionalArgument.Present name
                , description = OptionalArgument.Present (Markdown.toRawString description)
            }
        )


updateMetadata :
    Model
    -> { metaTitle : Maybe String, metaDescription : Maybe String, metaKeywords : List String }
    -> SelectionSet decodesTo Cambiatus.Object.Category
    -> SelectionSet (Maybe decodesTo) RootMutation
updateMetadata model { metaTitle, metaDescription, metaKeywords } =
    Cambiatus.Mutation.category
        (\optionals ->
            { optionals
                | id = OptionalArgument.Present (idToInt model.id)
                , metaDescription = OptionalArgument.fromMaybe metaDescription
                , metaKeywords = OptionalArgument.Present (String.join ", " metaKeywords)
                , metaTitle = OptionalArgument.fromMaybe metaTitle
            }
        )


addChild : Tree -> Id -> SelectionSet decodesTo Cambiatus.Object.Category -> SelectionSet (Maybe decodesTo) RootMutation
addChild tree newChildId =
    let
        toSubcategoryInput index categoryId =
            { id = idToInt categoryId
            , position = 0
            }
    in
    Cambiatus.Mutation.category
        (\optionals ->
            { optionals
                | categories =
                    Tree.children tree
                        |> List.map (Tree.label >> .id)
                        |> (\childrenIds -> newChildId :: childrenIds)
                        |> List.indexedMap toSubcategoryInput
                        |> OptionalArgument.Present
                , id =
                    Tree.label tree
                        |> .id
                        |> idToInt
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
        |> SelectionSet.with Cambiatus.Object.Category.metaTitle
        |> SelectionSet.with Cambiatus.Object.Category.metaDescription
        |> SelectionSet.with
            (Cambiatus.Object.Category.metaKeywords
                |> SelectionSet.map
                    (Maybe.map (String.split "," >> List.map String.trim)
                        >> Maybe.withDefault []
                    )
            )
        |> SelectionSet.with Cambiatus.Object.Category.position



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
                    |> List.sortBy (Tree.label >> .position)
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
                    |> List.sortBy (Tree.label >> .position)
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


idToInt : Id -> Int
idToInt (Id id) =
    id


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


idFromString : String -> Maybe Id
idFromString id =
    String.toInt id
        |> Maybe.map Id
