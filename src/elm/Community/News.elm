module Community.News exposing (Model, Receipt, isPublished, receiptSelectionSet, selectionSet, viewList)

import Cambiatus.Object
import Cambiatus.Object.News as News
import Cambiatus.Object.NewsReceipt as NewsReceipt
import Cambiatus.Scalar
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, div, h2, p, span, text)
import Html.Attributes exposing (class, classList, style)
import Icons
import Iso8601
import List.Extra
import Profile
import Route
import Session.Shared exposing (Shared)
import Time
import Utils
import View.Components
import View.MarkdownEditor



-- TYPES


type alias Model =
    { description : String
    , id : Int
    , title : String
    , receipt : Maybe Receipt
    , scheduling : Maybe Time.Posix
    , insertedAt : Time.Posix
    , updatedAt : Time.Posix
    , lastEditor : Maybe Profile.Minimal
    }


type alias Receipt =
    { reactions : List String
    }



-- HELPERS


isPublished : Time.Posix -> Model -> Bool
isPublished now model =
    case model.scheduling of
        Nothing ->
            True

        Just scheduling ->
            Time.posixToMillis now >= Time.posixToMillis scheduling



-- GRAPHQL


selectionSet : SelectionSet Model Cambiatus.Object.News
selectionSet =
    SelectionSet.succeed Model
        |> SelectionSet.with News.description
        |> SelectionSet.with News.id
        |> SelectionSet.with News.title
        -- |> SelectionSet.with (News.newsReceipt receiptSelectionSet)
        -- TODO - There is an issue where the subscription doesn't work if we include this field
        |> SelectionSet.hardcoded Nothing
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
        |> SelectionSet.with (SelectionSet.map timeFromNaiveDateTime News.insertedAt)
        |> SelectionSet.with (SelectionSet.map timeFromNaiveDateTime News.updatedAt)
        -- |> SelectionSet.with
        --     (Version.user Profile.minimalSelectionSet
        --         |> News.versions
        --         |> SelectionSet.map List.head
        --     )
        -- TODO - There is an issue where we can only query this field as the admin
        |> SelectionSet.hardcoded Nothing


receiptSelectionSet : SelectionSet Receipt Cambiatus.Object.NewsReceipt
receiptSelectionSet =
    SelectionSet.succeed Receipt
        |> SelectionSet.with NewsReceipt.reactions



-- GRAPHQL HELPERS


timeFromNaiveDateTime : Cambiatus.Scalar.NaiveDateTime -> Time.Posix
timeFromNaiveDateTime (Cambiatus.Scalar.NaiveDateTime dateTime) =
    Iso8601.toTime dateTime
        |> Result.withDefault (Time.millisToPosix 0)



-- VIEW


viewList : Shared -> List (Html.Attribute msg) -> List Model -> Html msg
viewList shared attrs news =
    div (class "divide-y divide-gray-500 space-y-4" :: attrs)
        (news
            |> List.Extra.groupWhile
                (\news1 news2 ->
                    Utils.areSameDay shared.timezone
                        news1.insertedAt
                        news2.insertedAt
                )
            |> List.map
                (\( firstNews, otherNews ) ->
                    div [ class "pt-4 first:pt-0" ]
                        [ View.Components.dateViewer
                            [ class "text-black text-sm font-bold uppercase" ]
                            identity
                            shared
                            firstNews.insertedAt
                        , div [ class "divide-y divide-gray-500" ]
                            (List.map
                                (\theseNews ->
                                    -- TODO - Use real data for hasRead
                                    viewSummary (modBy 2 theseNews.id == 0)
                                        theseNews
                                )
                                (firstNews :: otherNews)
                            )
                        ]
                )
        )


viewSummary : Bool -> Model -> Html msg
viewSummary hasRead news =
    a
        [ class "grid items-center py-4 focus-ring rounded-sm"
        , classList
            [ ( "text-gray-900 hover:text-gray-400", hasRead )
            , ( "text-purple-500 hover:opacity-80", not hasRead )
            ]
        , style "grid-template-columns" "28px 1fr 80px"
        , Route.href (Route.News (Just news.id))
        ]
        [ Icons.speechBubble "flex-shrink-0 stroke-current"
        , div [ class "truncate ml-4 mr-16" ]
            [ h2 [ class "font-bold truncate" ] [ text news.title ]
            , p [ class "truncate" ]
                [ text <| View.MarkdownEditor.removeFormatting news.description ]
            ]
        , if not hasRead then
            span
                [ class "button button-primary w-auto px-4"
                ]
                -- TODO - I18N
                [ text "Read" ]

          else
            Icons.arrowDown "-rotate-90 fill-current ml-auto"
        ]
