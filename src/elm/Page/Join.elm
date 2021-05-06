module Page.Join exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Page
import RemoteData
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import UpdateResult as UR
import View.Components



-- MODEL


type alias Model =
    {}


init : Page.Session -> ( Model, Cmd Msg )
init session =
    ( {}
    , case session of
        Page.Guest _ ->
            Cmd.none

        Page.LoggedIn loggedIn ->
            LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- UPDATE


type Msg
    = ClickedJoinCommunity
    | CompletedLoadCommunity Community.Model


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Page.Session -> Msg -> Model -> UpdateResult
update session msg model =
    case msg of
        ClickedJoinCommunity ->
            case session of
                Page.Guest guest ->
                    UR.init model
                        |> UR.addCmd
                            (Route.pushUrl guest.shared.navKey (Route.Register Nothing (Just Route.Dashboard)))

                Page.LoggedIn loggedIn ->
                    -- TODO - Sign up for community and redirect to Dashboard
                    UR.init model

        CompletedLoadCommunity community ->
            case session of
                Page.Guest _ ->
                    UR.init model
                        |> UR.logImpossible msg [ "AsGuest" ]

                Page.LoggedIn loggedIn ->
                    let
                        isMember =
                            List.map .account community.members
                                |> List.member loggedIn.accountName

                        redirectToDashboard =
                            if isMember then
                                UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.Dashboard)

                            else
                                identity
                    in
                    UR.init model
                        |> redirectToDashboard



-- VIEW


view : Page.Session -> Model -> { title : String, content : Html Msg }
view session _ =
    let
        { translators } =
            Page.toShared session

        maybeCommunityName =
            case session of
                Page.Guest guest ->
                    guest.community
                        |> RemoteData.map .name
                        |> RemoteData.toMaybe

                Page.LoggedIn loggedIn ->
                    loggedIn.selectedCommunity
                        |> RemoteData.map .name
                        |> RemoteData.toMaybe

        title =
            case maybeCommunityName of
                Nothing ->
                    translators.t "community.join.title_generic"

                Just name ->
                    translators.tr "community.join.title_with_community"
                        [ ( "community_name", name ) ]

        content =
            case session of
                Page.Guest guest ->
                    viewAsGuest title guest

                Page.LoggedIn loggedIn ->
                    viewAsLoggedIn title loggedIn
    in
    { title = title
    , content = content
    }


type alias GenericCommunity community =
    { community
        | name : String
        , hasAutoInvite : Bool
        , description : String
        , uploads : List String
        , memberCount : Int
        , website : Maybe String
    }


view_ : Bool -> Shared -> GenericCommunity community -> Html Msg
view_ isGuest ({ translators } as shared) community =
    div
        [ class "bg-purple-500 flex-grow flex flex-col md:justify-center items-center p-4 md:p-0"
        , classList [ ( "w-1/2", not isGuest ) ]
        ]
        [ div [ class "bg-white rounded-b md:rounded md:w-4/5 lg:w-7/12" ]
            [ Community.communityPreviewImage False shared community
            , div [ class "p-4" ]
                (span [ class "text-gray-900 text-sm block mb-7" ]
                    [ text community.description ]
                    :: (if community.hasAutoInvite then
                            [ button
                                [ class "button button-primary w-full cursor-pointer"
                                , onClick ClickedJoinCommunity
                                ]
                                [ text
                                    (translators.tr
                                        "community.join.title_with_community"
                                        [ ( "community_name", community.name ) ]
                                    )
                                ]
                            ]

                        else
                            [ span [ class "block text-center text-sm" ]
                                [ text (translators.t "community.join.only_invited") ]
                            , case community.website of
                                Nothing ->
                                    text ""

                                Just website ->
                                    a
                                        [ class "button button-primary w-full mt-4 cursor-pointer"
                                        , href website
                                        ]
                                        [ text (translators.t "community.join.visit_website") ]
                            ]
                       )
                )
            ]
        ]


viewLoading : Shared -> Html msg
viewLoading shared =
    div [ class "flex flex-col flex-grow items-center md:justify-center" ]
        [ View.Components.loadingLogoAnimated shared.translators "" ]


viewAsGuest : String -> Guest.Model -> Html Msg
viewAsGuest title guest =
    case guest.community of
        RemoteData.Success communityPreview ->
            view_ True guest.shared communityPreview

        RemoteData.Failure err ->
            Page.fullPageGraphQLError title err

        _ ->
            viewLoading guest.shared


viewAsLoggedIn : String -> LoggedIn.Model -> Html Msg
viewAsLoggedIn title loggedIn =
    case loggedIn.selectedCommunity of
        RemoteData.Success community ->
            div [ class "flex-grow flex" ]
                [ Community.communityPreviewImage True loggedIn.shared community
                , view_ False loggedIn.shared community
                ]

        RemoteData.Failure err ->
            Page.fullPageGraphQLError title err

        _ ->
            viewLoading loggedIn.shared



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
        ClickedJoinCommunity ->
            [ "ClickedJoinCommunity" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]
