module Community.News exposing (Model, selectionSet)

import Cambiatus.Object
import Cambiatus.Object.News as News
import Cambiatus.Object.NewsVersion as Version
import Cambiatus.Scalar
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Iso8601
import Profile
import Time


type alias Model =
    { description : String
    , id : Int
    , title : String
    , scheduling : Maybe Time.Posix
    , updatedAt : Time.Posix
    , lastEditor : Maybe Profile.Minimal
    }


selectionSet : SelectionSet Model Cambiatus.Object.News
selectionSet =
    SelectionSet.succeed Model
        |> SelectionSet.with News.description
        |> SelectionSet.with News.id
        |> SelectionSet.with News.title
        |> SelectionSet.with
            (News.scheduling
                |> SelectionSet.map
                    (Maybe.andThen
                        (\(Cambiatus.Scalar.DateTime dateTime) ->
                            Iso8601.toTime dateTime
                                |> Result.toMaybe
                        )
                    )
            )
        |> SelectionSet.with
            (News.updatedAt
                |> SelectionSet.map
                    (\(Cambiatus.Scalar.NaiveDateTime dateTime) ->
                        Iso8601.toTime dateTime
                            |> Result.withDefault (Time.millisToPosix 0)
                    )
            )
        |> SelectionSet.with
            (News.versions
                (Version.user Profile.minimalSelectionSet)
                |> SelectionSet.map (List.filterMap identity >> List.head)
            )
