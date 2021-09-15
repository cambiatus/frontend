module Page.Profile.Contributions exposing (Model, Msg, init, msgToString, update, view)

import Community
import Eos.Account
import Html exposing (Html, div, li, p, span, text, ul)
import Html.Attributes exposing (class, classList)
import Icons
import List.Extra
import Page
import RemoteData
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import UpdateResult as UR
import Utils
import View.Components



-- MODEL


type alias Model =
    { profileName : Eos.Account.Name }


init : Eos.Account.Name -> LoggedIn.Model -> ( Model, Cmd Msg )
init profileName loggedIn =
    ( { profileName = profileName }, Cmd.none )



-- TYPES


type Msg
    = NoOp


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model _ =
    case msg of
        NoOp ->
            UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
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
                        let
                            profileContributions =
                                community.contributions
                                    |> List.filter (\contribution -> contribution.user.account == model.profileName)
                        in
                        view_ loggedIn profileContributions title

                    else
                        Page.fullPageNotFound
                            (loggedIn.shared.translators.t "profile.contributions.not_allowed_title")
                            (loggedIn.shared.translators.t "profile.contributions.not_allowed")

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { title = title
    , content = content
    }


view_ : LoggedIn.Model -> List Community.Contribution -> String -> Html Msg
view_ loggedIn profileContributions title =
    div [ class "flex flex-grow flex-col" ]
        [ Page.viewHeader loggedIn title
        , div [ class "flex flex-grow container mx-auto p-4" ]
            [ ul [ class "flex-grow bg-white rounded divide-y px-4" ]
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
                                [ -- TODO - Use new text size class (#622)
                                  View.Components.dateViewer [ class "text-[12px] text-black uppercase font-bold px-1" ]
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


viewContribution : Translators -> Community.Contribution -> Html Msg
viewContribution { t } contribution =
    li [ class "flex items-center px-1 py-4" ]
        [ div [ class "w-14 h-14 bg-gray-100 flex items-center justify-center rounded" ]
            [ Icons.coinHeart "" ]
        , p [ class "flex space-x-1 text-black" ]
            -- TODO - Use new text size classes (#622)
            [ span [ class "text-[22px]" ]
                [ text (Utils.formatFloat contribution.amount 0 True) ]
            , span [ class "text-[12px] uppercase self-end mb-1" ]
                [ contribution
                    |> Community.currencyTranslationKey
                    |> t
                    |> text
                ]
            ]
        ]



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]
