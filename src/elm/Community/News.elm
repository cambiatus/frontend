module Community.News exposing (Model, Reaction, Receipt, isPublished, listReactions, mockSelectedReactions, reactToNews, reactionName, reactionToString, receiptSelectionSet, selectionSet, viewList)

import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.News as News
import Cambiatus.Object.NewsReceipt as NewsReceipt
import Cambiatus.Scalar
import Graphql.Operation
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, div, h2, p, span, text)
import Html.Attributes exposing (class, classList, style)
import Html.Attributes.Aria exposing (ariaHidden)
import Icons
import Iso8601
import List.Extra
import Profile
import Route
import Session.Shared exposing (Shared, Translators)
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
    , creator : Profile.Minimal
    }


type alias Receipt =
    { reactions : List String
    }


type Reaction
    = Smile
    | HeartEyes
    | Frown
    | RaisedEyebrow
    | ThumbsUp
    | ThumbsDown
    | Clap
    | Tada
    | Heart
    | Rocket



-- HELPERS


isPublished : Time.Posix -> Model -> Bool
isPublished now model =
    case model.scheduling of
        Nothing ->
            True

        Just scheduling ->
            Time.posixToMillis now >= Time.posixToMillis scheduling


reactionToString : Reaction -> String
reactionToString reaction =
    case reaction of
        Smile ->
            "😃"

        HeartEyes ->
            "😍"

        Frown ->
            "🙁"

        RaisedEyebrow ->
            "\u{1F928}"

        ThumbsUp ->
            "👍"

        ThumbsDown ->
            "👎"

        Clap ->
            "👏"

        Tada ->
            "🎉"

        Heart ->
            "❤️"

        Rocket ->
            "🚀"


reactionName : Reaction -> String
reactionName reaction =
    case reaction of
        Smile ->
            "smile"

        HeartEyes ->
            "heartEyes"

        Frown ->
            "frown"

        RaisedEyebrow ->
            "raisedEyebrow"

        ThumbsUp ->
            "thumbsUp"

        ThumbsDown ->
            "thumbsDown"

        Clap ->
            "clap"

        Tada ->
            "tada"

        Heart ->
            "heart"

        Rocket ->
            "rocket"


listReactions : List Reaction
listReactions =
    [ Smile
    , HeartEyes
    , Frown
    , RaisedEyebrow
    , ThumbsUp
    , ThumbsDown
    , Clap
    , Tada
    , Heart
    , Rocket
    ]


mockSelectedReactions : List { reaction : Reaction, count : Int }
mockSelectedReactions =
    [ { reaction = Smile, count = 5 }
    , { reaction = Frown, count = 0 }
    , { reaction = RaisedEyebrow, count = 10 }
    ]



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
        |> SelectionSet.with (News.user Profile.minimalSelectionSet)


receiptSelectionSet : SelectionSet Receipt Cambiatus.Object.NewsReceipt
receiptSelectionSet =
    SelectionSet.succeed Receipt
        |> SelectionSet.with NewsReceipt.reactions


reactToNews : { newsId : Int, reactions : List Reaction } -> SelectionSet (Maybe Receipt) Graphql.Operation.RootMutation
reactToNews args =
    Cambiatus.Mutation.reactToNews
        { newsId = args.newsId
        , reactions = List.map reactionName args.reactions
        }
        receiptSelectionSet



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
                                    viewSummary shared.translators
                                        (modBy 2 theseNews.id == 0)
                                        theseNews
                                )
                                (firstNews :: otherNews)
                            )
                        ]
                )
        )


viewSummary : Translators -> Bool -> Model -> Html msg
viewSummary { t } hasRead news =
    a
        [ class "grid items-center py-4 focus-ring rounded-sm"
        , classList
            [ ( "text-gray-900 hover:text-gray-400", hasRead )
            , ( "text-purple-500 hover:opacity-80", not hasRead )
            ]
        , style "grid-template-columns" "28px 1fr 80px"
        , Route.href (Route.News (Just news.id))
        ]
        [ Icons.speechBubble [ ariaHidden True ] "flex-shrink-0 stroke-current"
        , div [ class "truncate ml-4 mr-16" ]
            [ h2 [ class "font-bold truncate" ] [ text news.title ]
            , p [ class "truncate" ]
                [ text <| View.MarkdownEditor.removeFormatting news.description ]
            ]
        , if not hasRead then
            span
                [ class "button button-primary w-auto px-4"
                ]
                [ text <| t "news.read" ]

          else
            Icons.arrowDown "-rotate-90 fill-current ml-auto"
        ]
