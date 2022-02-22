module Page.Profile.Contributions exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Cambiatus.Enum.ContributionStatusType
import Community
import Dict
import Eos.Account
import Graphql.Http
import Html exposing (Html, div, img, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, src)
import Icons
import List.Extra
import Page
import Profile
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import UpdateResult as UR
import Utils
import View.Components



-- MODEL


type alias Model =
    { profileName : Eos.Account.Name
    , contributions : RemoteData (Graphql.Http.Error (Maybe (List Profile.Contribution))) (List Profile.Contribution)
    }


init : LoggedIn.Model -> Eos.Account.Name -> ( Model, Cmd Msg )
init loggedIn profileName =
    ( { profileName = profileName
      , contributions = RemoteData.NotAsked
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- TYPES


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type Msg
    = CompletedLoadCommunity Community.Model
    | CompletedLoadContributions (RemoteData (Graphql.Http.Error (Maybe (List Profile.Contribution))) (Maybe (List Profile.Contribution)))



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadCommunity community ->
            { model | contributions = RemoteData.Loading }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.query loggedIn
                        (Profile.contributionsQuery community.symbol model.profileName)
                        CompletedLoadContributions
                    )

        CompletedLoadContributions (RemoteData.Success contributions) ->
            { model | contributions = RemoteData.Success (Maybe.withDefault [] contributions) }
                |> UR.init

        CompletedLoadContributions (RemoteData.Failure err) ->
            { model | contributions = RemoteData.Failure err }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading profile contributions"
                    { moduleName = "Page.Profile.Contributions", function = "update" }
                    [ { name = "Profile"
                      , extras = Dict.fromList [ ( "profile", Eos.Account.encodeName model.profileName ) ]
                      }
                    ]
                    err

        CompletedLoadContributions RemoteData.NotAsked ->
            UR.init model

        CompletedLoadContributions RemoteData.Loading ->
            UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html msg }
view loggedIn model =
    let
        title =
            if loggedIn.accountName == model.profileName then
                loggedIn.shared.translators.t "dashboard.my_contributions"

            else
                loggedIn.shared.translators.tr "profile.contributions.profile_contributions"
                    [ ( "profile_name", Eos.Account.nameToString model.profileName ) ]

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    if loggedIn.accountName == model.profileName || loggedIn.accountName == community.creator then
                        case model.contributions of
                            RemoteData.Success contributions ->
                                view_ loggedIn contributions title

                            RemoteData.Loading ->
                                Page.fullPageLoading loggedIn.shared

                            RemoteData.NotAsked ->
                                Page.fullPageLoading loggedIn.shared

                            RemoteData.Failure err ->
                                Page.fullPageGraphQLError title err

                    else
                        Page.fullPageNotFound
                            (loggedIn.shared.translators.t "profile.contributions.not_allowed_title")
                            (loggedIn.shared.translators.t "profile.contributions.not_allowed")

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { title = title
    , content = content
    }


view_ : LoggedIn.Model -> List Profile.Contribution -> String -> Html msg
view_ loggedIn profileContributions title =
    div [ class "flex flex-grow flex-col" ]
        [ Page.viewHeader loggedIn title
        , div [ class "flex flex-grow container mx-auto p-4" ]
            [ if List.isEmpty profileContributions then
                div [ class "flex-grow flex flex-col items-center justify-center bg-white rounded px-4" ]
                    [ img [ src "/images/empty-analysis.svg", class "h-32 mb-3" ] []
                    , p [ class "text-gray" ]
                        [ text (loggedIn.shared.translators.t "profile.contributions.no_contributions") ]
                    ]

              else
                ul [ class "flex-grow bg-white rounded divide-y px-4" ]
                    (profileContributions
                        |> List.Extra.groupWhile
                            (\c1 c2 ->
                                Utils.areSameDay loggedIn.shared.timezone
                                    c1.insertedAt
                                    c2.insertedAt
                            )
                        |> List.indexedMap
                            (\index ( firstContribution, otherContributions ) ->
                                li
                                    [ classList
                                        [ ( "pt-6", index /= 0 )
                                        , ( "pt-4", index == 0 )
                                        ]
                                    ]
                                    [ View.Components.dateViewer [ class "text-sm text-black uppercase font-bold px-1" ]
                                        identity
                                        loggedIn.shared
                                        firstContribution.insertedAt
                                    , ul [ class "divide-y" ]
                                        (List.map (viewContribution loggedIn.shared.translators)
                                            (firstContribution :: otherContributions)
                                        )
                                    ]
                            )
                    )
            ]
        ]


viewContribution : Translators -> Profile.Contribution -> Html msg
viewContribution ({ t } as translators) contribution =
    let
        isApprovedOrCaptured =
            case contribution.status of
                Cambiatus.Enum.ContributionStatusType.Approved ->
                    True

                Cambiatus.Enum.ContributionStatusType.Captured ->
                    True

                _ ->
                    False

        isFailedOrRejected =
            case contribution.status of
                Cambiatus.Enum.ContributionStatusType.Failed ->
                    True

                Cambiatus.Enum.ContributionStatusType.Rejected ->
                    True

                _ ->
                    False

        statusString =
            if isApprovedOrCaptured then
                t "profile.contributions.approved"

            else if isFailedOrRejected then
                t "profile.contributions.failed"

            else
                t "profile.contributions.pending"
    in
    li [ class "flex items-center px-1 py-4 flex-wrap" ]
        [ div [ class "w-14 h-14 bg-gray-100 flex items-center justify-center rounded" ]
            [ Icons.coinHeart "" ]
        , p [ class "flex space-x-1 text-black ml-3 mr-2" ]
            [ span [ class "text-lg" ]
                [ text (Utils.formatFloat (Just translators) 0 contribution.amount) ]
            , span [ class "text-sm uppercase self-end mb-2" ]
                [ contribution
                    |> Community.currencyTranslationKey
                    |> t
                    |> text
                ]
            ]
        , span
            [ class "ml-auto py-0.5 px-5 bg-gray-100 uppercase text-sm"
            , classList
                [ ( "font-bold text-green", isApprovedOrCaptured )
                , ( "font-bold text-red", isFailedOrRejected )
                ]
            ]
            [ text statusString ]
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
        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        CompletedLoadContributions r ->
            [ "CompletedLoadContributions", UR.remoteDataToString r ]
