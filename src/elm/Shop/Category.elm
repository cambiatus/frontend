module Shop.Category exposing
    ( Id
    , Model
    , Tree
    , idFromString
    , idToInt
    , idToString
    , selectionSet
    , treesSelectionSet
    )

import Cambiatus.Object
import Cambiatus.Object.Category
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
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
