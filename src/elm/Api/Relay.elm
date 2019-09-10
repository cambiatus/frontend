module Api.Relay exposing (Edge, MetadataConnection, PageConnection, PageInfo, PaginationArgs, pageInfoSelectionSet)

import Bespiral.Object
import Bespiral.Object.PageInfo
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


pageInfoSelectionSet : SelectionSet PageInfo Bespiral.Object.PageInfo
pageInfoSelectionSet =
    SelectionSet.succeed PageInfo
        |> with Bespiral.Object.PageInfo.endCursor
        |> with Bespiral.Object.PageInfo.hasNextPage
        |> with Bespiral.Object.PageInfo.hasPreviousPage
        |> with Bespiral.Object.PageInfo.startCursor
