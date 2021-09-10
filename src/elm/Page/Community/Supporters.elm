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
import Time
import UpdateResult as UR
import View.Components



-- MODEL


type alias Model =
    { profileSummaries : List Profile.Summary.Model }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { profileSummaries = []
      }
    , Cmd.batch
        [ LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
        , Browser.Dom.setViewport 0 0
            |> Task.perform (\_ -> NoOp)
        ]
    )



-- TYPES


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | GotProfileSummaryMsg Int Profile.Summary.Msg


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model _ =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            { model
                | profileSummaries =
                    List.length community.contributions
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
                , case loggedIn.selectedCommunity of
                    RemoteData.Success community ->
                        view_ loggedIn community model

                    RemoteData.Loading ->
                        Page.fullPageLoading loggedIn.shared

                    RemoteData.NotAsked ->
                        Page.fullPageLoading loggedIn.shared

                    RemoteData.Failure error ->
                        Page.fullPageGraphQLError title error
                ]
    in
    { title = title
    , content = content
    }


view_ : LoggedIn.Model -> Community.Model -> Model -> Html Msg
view_ loggedIn community model =
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
                (community.contributions
                    -- TODO - Group by date
                    |> List.repeat 2
                    |> List.map
                        (\day ->
                            li []
                                [ View.Components.dateViewer [ class "text-caption text-black uppercase" ]
                                    identity
                                    loggedIn.shared
                                    (Time.millisToPosix 123123)
                                , ul [ class "divide-y" ]
                                    (List.map3 (viewSupporter loggedIn)
                                        (List.range 0 (List.length model.profileSummaries))
                                        model.profileSummaries
                                        (List.map .user day)
                                    )
                                ]
                        )
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
                |> Profile.Summary.view loggedIn.shared loggedIn.accountName profile
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
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        GotProfileSummaryMsg _ subMsg ->
            "GotProfileSummaryMsg" :: Profile.Summary.msgToString subMsg
