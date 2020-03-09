module Api.Relay exposing (Edge, MetadataConnection, PageConnection, PageInfo, PaginationArgs, pageInfoSelectionSet)

import Cambiatus.Object
import Cambiatus.Object.PageInfo
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)


type alias PageInfo =
    { endCursor : Maybe String
    , hasNextPage : Bool
    , hasPreviousPage : Bool
    , startCursor : Maybe String
    }


type alias Edge nodeType =
    { cursor : String
    , node : Maybe nodeType
    }


type alias PageConnection nodeType =
    { edges : Maybe (List (Maybe (Edge nodeType)))
    , pageInfo : PageInfo
    }


type alias MetadataConnection =
    { totalCount : Maybe Int
    , fetchedCount : Maybe Int
    }


type alias PaginationArgs =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


pageInfoSelectionSet : SelectionSet PageInfo Cambiatus.Object.PageInfo
pageInfoSelectionSet =
    SelectionSet.succeed PageInfo
        |> with Cambiatus.Object.PageInfo.endCursor
        |> with Cambiatus.Object.PageInfo.hasNextPage
        |> with Cambiatus.Object.PageInfo.hasPreviousPage
        |> with Cambiatus.Object.PageInfo.startCursor
