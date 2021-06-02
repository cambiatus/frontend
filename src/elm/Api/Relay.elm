module Api.Relay exposing (Edge, PageConnection, PageInfo, pageInfoSelectionSet)

import Cambiatus.Object
import Cambiatus.Object.PageInfo
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)


type alias PageInfo =
    { endCursor : Maybe String
    , hasNextPage : Bool
    , hasPreviousPage : Bool
    , startCursor : Maybe String
    }


type alias Edge nodeType =
    { cursor : Maybe String
    , node : Maybe nodeType
    }


type alias PageConnection nodeType =
    { edges : Maybe (List (Maybe (Edge nodeType)))
    , pageInfo : PageInfo
    }


pageInfoSelectionSet : SelectionSet PageInfo Cambiatus.Object.PageInfo
pageInfoSelectionSet =
    SelectionSet.succeed PageInfo
        |> with Cambiatus.Object.PageInfo.endCursor
        |> with Cambiatus.Object.PageInfo.hasNextPage
        |> with Cambiatus.Object.PageInfo.hasPreviousPage
        |> with Cambiatus.Object.PageInfo.startCursor
