module Page.Community.Supporters exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Browser.Dom
import Community
import Eos.Account
import Html exposing (Html, a, button, div, li, p, span, text, ul)
import Html.Attributes exposing (class, tabindex)
import Html.Events exposing (onClick)
import List.Extra
import Page
import Profile
import Profile.Summary
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Task
import UpdateResult as UR
import Utils
import View.Components



-- MODEL


type alias Model =
    { profileSummaries : List Profile.Summary.Model }


init : LoggedIn.Model -> UpdateResult
init _ =
    { profileSummaries = [] }
        |> UR.init
        |> UR.addCmd
            (Browser.Dom.setViewport 0 0
                |> Task.perform (\_ -> NoOp)
            )
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ContributionsField)



-- TYPES


type Msg
    = NoOp
    | CompletedLoadContributions (List Community.Contribution)
    | GotProfileSummaryMsg Int Profile.Summary.Msg


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model _ =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadContributions contributions ->
            { model
                | profileSummaries =
                    List.length contributions
                        |> Profile.Summary.initMany False
            }
                |> UR.init

        GotProfileSummaryMsg index subMsg ->
            { model | profileSummaries = List.Extra.updateAt index (Profile.Summary.update subMsg) model.profileSummaries }
                |> UR.init



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            [ "community.index.our_supporters"
            , "community.index.supporters"
            ]
                |> List.map loggedIn.shared.translators.t
                |> String.join " "

        content =
            div [ class "flex flex-col flex-grow" ]
                [ Page.viewHeader loggedIn title
                , case Community.getField loggedIn.selectedCommunity .contributions of
                    RemoteData.Success ( _, contributions ) ->
                        view_ loggedIn contributions model

                    RemoteData.Loading ->
                        Page.fullPageLoading loggedIn.shared

                    RemoteData.NotAsked ->
                        Page.fullPageLoading loggedIn.shared

                    RemoteData.Failure (Community.CommunityError error) ->
                        Page.fullPageGraphQLError title error

                    RemoteData.Failure (Community.FieldError error) ->
                        Page.fullPageGraphQLError title error
                ]
    in
    { title = title
    , content = content
    }


view_ : LoggedIn.Model -> List Community.Contribution -> Model -> Html Msg
view_ loggedIn contributions model =
    let
        { t } =
            loggedIn.shared.translators
    in
    div []
        [ div [ class "container mx-auto px-4" ]
            [ p [ class "text-heading py-4" ]
                [ span [ class "text-gray-900" ] [ text <| t "community.index.our_supporters" ]
                , text " "
                , span [ class "text-indigo-500 font-bold" ] [ text <| t "community.index.supporters" ]
                ]
            ]
        , div [ class "w-full bg-white flex-grow pt-5" ]
            [ ul [ class "container mx-auto px-4 space-y-4" ]
                (contributions
                    |> List.Extra.groupWhile
                        (\c1 c2 ->
                            Utils.areSameDay loggedIn.shared.timezone
                                c1.insertedAt
                                c2.insertedAt
                        )
                    |> List.foldl
                        (\( firstContribution, otherContributions ) ( currentIndex, currentList ) ->
                            let
                                contributionsLength =
                                    List.length (firstContribution :: otherContributions)
                            in
                            ( currentIndex + contributionsLength
                            , li []
                                [ View.Components.dateViewer [ class "text-caption text-black uppercase" ]
                                    identity
                                    loggedIn.shared
                                    firstContribution.insertedAt
                                , ul [ class "divide-y" ]
                                    (List.map3 (viewSupporter loggedIn)
                                        (List.range currentIndex (currentIndex + contributionsLength))
                                        (List.drop currentIndex model.profileSummaries)
                                        (List.map .user (firstContribution :: otherContributions))
                                    )
                                ]
                                :: currentList
                            )
                        )
                        ( 0, [] )
                    |> Tuple.second
                    |> List.reverse
                )
            ]
        ]


viewSupporter : LoggedIn.Model -> Int -> Profile.Summary.Model -> Profile.Minimal -> Html Msg
viewSupporter loggedIn index profileSummary profile =
    let
        containerClasses =
            "flex items-center p-4 w-full hover:bg-gray-100 focus:outline-none focus:ring"

        content =
            [ profileSummary
                |> Profile.Summary.withoutName
                |> Profile.Summary.withImageSize "w-14 h-14"
                |> Profile.Summary.withAttrs [ tabindex -1 ]
                |> Profile.Summary.view loggedIn.shared.translators loggedIn.accountName profile
                |> Html.map (GotProfileSummaryMsg index)
            , p [ class "ml-4 font-bold text-black" ]
                [ text
                    (profile.name
                        |> Maybe.withDefault (Eos.Account.nameToString profile.account)
                    )
                ]
            ]
    in
    li []
        [ button
            [ class (containerClasses ++ " md:hidden")
            , onClick
                (Profile.Summary.expand
                    |> GotProfileSummaryMsg index
                )
            ]
            content
        , a
            [ class (containerClasses ++ " hidden md:flex")
            , Route.href (Route.Profile profile.account)
            ]
            content
        ]



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityFieldLoaded _ (Community.ContributionsValue contributions) ->
            Just (CompletedLoadContributions contributions)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadContributions _ ->
            [ "CompletedLoadContributions" ]

        GotProfileSummaryMsg _ subMsg ->
            "GotProfileSummaryMsg" :: Profile.Summary.msgToString subMsg
