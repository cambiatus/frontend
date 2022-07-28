module Page.Join exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Markdown exposing (Markdown)
import Page
import RemoteData
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import UpdateResult as UR
import Utils
import View.Components



-- MODEL


type alias Model =
    { maybeRedirect : Maybe Route.Route }


init : Page.Session -> Maybe Route.Route -> ( Model, Cmd Msg )
init session maybeRedirect =
    ( { maybeRedirect = maybeRedirect }
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
    | SignedIn


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
                            (Route.pushUrl guest.shared.navKey (Route.Register Nothing model.maybeRedirect))

                Page.LoggedIn _ ->
                    UR.init model
                        |> UR.addExt (LoggedIn.RequiredAuthToken { callbackCmd = \_ -> Utils.spawnMessage SignedIn })

        CompletedLoadCommunity community ->
            case session of
                Page.Guest _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Completed load community, but is still guest"
                            Nothing
                            { moduleName = "Page.Join", function = "update" }
                            []

                Page.LoggedIn loggedIn ->
                    let
                        isMember =
                            List.map .account community.members
                                |> List.member loggedIn.accountName

                        addAction =
                            if isMember then
                                model.maybeRedirect
                                    |> Maybe.withDefault Route.Dashboard
                                    |> Route.pushUrl loggedIn.shared.navKey
                                    |> UR.addCmd

                            else
                                UR.addExt (LoggedIn.RequestedCommunityField Community.UploadsField)
                    in
                    UR.init model
                        |> addAction

        SignedIn ->
            case session of
                Page.Guest _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Signed in, but is still guest"
                            Nothing
                            { moduleName = "Page.Join"
                            , function = "update"
                            }
                            []

                Page.LoggedIn loggedIn ->
                    let
                        addCommunity =
                            case ( loggedIn.selectedCommunity, loggedIn.profile ) of
                                ( RemoteData.Success community, RemoteData.Success profile ) ->
                                    let
                                        communityInfo =
                                            { symbol = community.symbol
                                            , name = community.name
                                            , logo = community.logo
                                            , subdomain = community.subdomain
                                            , hasShop = community.hasShop
                                            , hasActions = community.hasObjectives
                                            , hasKyc = community.hasKyc
                                            }

                                        minimalProfile =
                                            { name = profile.name
                                            , account = profile.account
                                            , avatar = profile.avatar
                                            , email = profile.email
                                            , bio = profile.bio
                                            , contacts = profile.contacts
                                            }
                                    in
                                    UR.addExt
                                        (LoggedIn.CommunityLoaded { community | members = minimalProfile :: community.members }
                                            |> LoggedIn.ExternalBroadcast
                                        )
                                        >> UR.addExt
                                            (LoggedIn.ProfileLoaded { profile | communities = communityInfo :: profile.communities }
                                                |> LoggedIn.ExternalBroadcast
                                            )

                                _ ->
                                    UR.logImpossible msg
                                        "Completed sign in, but community or profile weren't loaded"
                                        (Just loggedIn.accountName)
                                        { moduleName = "Page.Join", function = "update" }
                                        []
                    in
                    model
                        |> UR.init
                        |> addCommunity
                        |> UR.addCmd
                            (model.maybeRedirect
                                |> Maybe.withDefault Route.Dashboard
                                |> Route.replaceUrl loggedIn.shared.navKey
                            )



-- VIEW


view : Page.Session -> Model -> { title : String, content : Html Msg }
view session model =
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
                    viewAsGuest title guest model

                Page.LoggedIn loggedIn ->
                    viewAsLoggedIn title loggedIn model
    in
    { title = title
    , content = content
    }


type alias GenericCommunity community =
    { community
        | name : String
        , hasAutoInvite : Bool
        , description : Markdown
        , uploads : List String
        , memberCount : Int
        , website : Maybe String
    }


view_ : Bool -> Shared -> GenericCommunity community -> Model -> Html Msg
view_ isGuest ({ translators } as shared) community model =
    div
        [ class "bg-purple-500 flex-grow flex flex-col md:justify-center items-center p-4 lg:p-0"
        , classList [ ( "w-1/2", not isGuest ) ]
        ]
        [ div [ class "bg-white rounded-b md:rounded w-full lg:w-4/5 xl:w-7/12" ]
            [ Community.communityPreviewImage False shared community
            , div [ class "p-4" ]
                (Markdown.view [ class "text-gray-333 block mb-7" ] community.description
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
                            [ span [ class "block text-center text-black font-bold" ]
                                [ text (translators.t "community.join.only_invited") ]
                            , a
                                [ class "button button-primary w-full mt-4"
                                , Route.href (Route.Login Nothing model.maybeRedirect)
                                ]
                                [ text (translators.t "community.join.already_member") ]
                            ]
                       )
                    ++ (case community.website of
                            Nothing ->
                                [ text "" ]

                            Just website ->
                                [ a
                                    [ class "button button-secondary w-full mt-4 cursor-pointer"
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


viewAsGuest : String -> Guest.Model -> Model -> Html Msg
viewAsGuest title guest model =
    case guest.community of
        RemoteData.Success communityPreview ->
            view_ True guest.shared communityPreview model

        RemoteData.Failure err ->
            Page.fullPageGraphQLError title err

        _ ->
            viewLoading guest.shared


viewAsLoggedIn : String -> LoggedIn.Model -> Model -> Html Msg
viewAsLoggedIn title loggedIn model =
    case Community.getField loggedIn.selectedCommunity .uploads of
        RemoteData.Success ( community, uploads ) ->
            let
                normalizedCommunity =
                    { name = community.name
                    , hasAutoInvite = community.hasAutoInvite
                    , description = community.description
                    , uploads = uploads
                    , memberCount = community.memberCount
                    , website = community.website
                    }
            in
            div [ class "flex-grow flex" ]
                [ Community.communityPreviewImage True loggedIn.shared normalizedCommunity
                , view_ False loggedIn.shared normalizedCommunity model
                ]

        RemoteData.Failure (Community.CommunityError err) ->
            Page.fullPageGraphQLError title err

        RemoteData.Failure (Community.FieldError err) ->
            Page.fullPageGraphQLError title err

        RemoteData.Loading ->
            viewLoading loggedIn.shared

        RemoteData.NotAsked ->
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

        SignedIn ->
            [ "SignedIn" ]
