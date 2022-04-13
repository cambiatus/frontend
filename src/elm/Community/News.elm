module Community.News exposing (Model, Reaction, Receipt, isPublished, listReactions, reactToNews, reactionName, reactionToString, receiptSelectionSet, selectionSet, viewList)

import Cambiatus.Enum.ReactionEnum as Reaction exposing (ReactionEnum(..))
import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.News as News
import Cambiatus.Object.NewsReceipt as NewsReceipt
import Cambiatus.Object.ReactionType
import Cambiatus.Scalar
import Graphql.Operation
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, div, h3, h4, p, text)
import Html.Attributes exposing (class, classList, style, title)
import Html.Attributes.Aria exposing (ariaHidden)
import Icons
import Iso8601
import List.Extra
import Markdown exposing (Markdown)
import Maybe.Extra
import Profile
import Route
import Session.Shared exposing (Shared, Translators)
import Time
import Utils
import View.Components



-- TYPES


type alias Model =
    { description : Markdown
    , id : Int
    , title : String
    , receipt : Maybe Receipt
    , reactions : List ReactionWithCount
    , scheduling : Maybe Time.Posix
    , insertedAt : Time.Posix
    , updatedAt : Time.Posix
    , creator : Profile.Minimal
    }


type alias Receipt =
    { reactions : List Reaction
    }


type alias Reaction =
    ReactionEnum


type alias ReactionWithCount =
    { count : Int, reaction : Reaction }



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
        ClappingHands ->
            "👏"

        FaceWithRaisedEyebrow ->
            "🤨"

        GrinningFaceWithBigEyes ->
            "😃"

        PartyPopper ->
            "🎉"

        RedHeart ->
            "❤️"

        Rocket ->
            "🚀"

        SlightlyFrowningFace ->
            "🙁"

        SmilingFaceWithHeartEyes ->
            "😍"

        ThumbsDown ->
            "👎"

        ThumbsUp ->
            "👍"


reactionName : Reaction -> String
reactionName =
    Reaction.toString
        >> String.replace "_" " "


listReactions : List Reaction
listReactions =
    Reaction.list



-- GRAPHQL


selectionSet : SelectionSet Model Cambiatus.Object.News
selectionSet =
    SelectionSet.succeed Model
        |> SelectionSet.with (Markdown.selectionSet News.description)
        |> SelectionSet.with News.id
        |> SelectionSet.with News.title
        |> SelectionSet.with (News.receipt receiptSelectionSet)
        |> SelectionSet.with (News.reactions reactionWithCountSelectionSet)
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


reactionWithCountSelectionSet : SelectionSet ReactionWithCount Cambiatus.Object.ReactionType
reactionWithCountSelectionSet =
    SelectionSet.succeed ReactionWithCount
        |> SelectionSet.with Cambiatus.Object.ReactionType.count
        |> SelectionSet.with Cambiatus.Object.ReactionType.reaction


reactToNews : Model -> { newsId : Int, reaction : Reaction } -> SelectionSet (Maybe Receipt) Graphql.Operation.RootMutation
reactToNews news args =
    Cambiatus.Mutation.reactToNews
        { newsId = args.newsId
        , reactions =
            case news.receipt of
                Nothing ->
                    [ args.reaction ]

                Just receipt ->
                    if List.member args.reaction receipt.reactions then
                        List.filter ((/=) args.reaction) receipt.reactions

                    else
                        args.reaction :: receipt.reactions
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
    div (class "divide-y divide-gray-100 space-y-4" :: attrs)
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
                        [ h3 []
                            [ View.Components.dateViewer
                                [ class "text-black text-sm font-bold uppercase" ]
                                identity
                                shared
                                firstNews.insertedAt
                            ]
                        , div [ class "divide-y divide-gray-100" ]
                            (List.map
                                (\theseNews ->
                                    viewSummary shared.translators
                                        (Maybe.Extra.isJust theseNews.receipt)
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
        [ class "grid items-center py-4 focus-ring rounded-sm hover:opacity-70"
        , style "grid-template-columns" "32px 1fr 80px"
        , title news.title
        , Route.href (Route.News { selectedNews = Just news.id, showOthers = False })
        ]
        [ Icons.speechBubble
            [ ariaHidden True
            ]
            "h-8 w-8 stroke-current text-blue bg-gray-100 rounded-full p-[6px] overflow-visible"
        , div [ class "truncate mx-4" ]
            [ h4
                [ class "font-bold truncate"
                , classList
                    [ ( "text-purple-500", not hasRead )
                    , ( "text-gray-333", hasRead )
                    ]
                ]
                [ text news.title ]
            , p
                [ class "truncate text-gray-333"
                , ariaHidden True
                ]
                [ text <| Markdown.toUnformattedString news.description ]
            , p [ class "sr-only" ]
                [ if hasRead then
                    text <| t "news.already_read"

                  else
                    text <| t "news.not_read_yet"
                ]
            ]
        , Icons.arrowDown "-rotate-90 fill-current text-orange-300 ml-auto rounded-full bg-gray-100"
        ]
