module Page.Community.Settings.Sponsorship exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Form.Toggle
import Html exposing (Html, a, button, div, h2, p, text)
import Html.Attributes exposing (class, disabled)
import Maybe.Extra
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import UpdateResult as UR



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( {}
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- TYPES


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            let
                maybeRedirect =
                    if community.creator /= loggedIn.accountName then
                        UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.Dashboard)

                    else
                        identity
            in
            model
                |> UR.init
                |> maybeRedirect



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn _ =
    let
        title =
            loggedIn.shared.translators.t "sponsorship.title"

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , view_ loggedIn.shared community
                        ]

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { title = title, content = content }


view_ : Shared -> Community.Model -> Html Msg
view_ { translators } community =
    let
        text_ =
            translators.t >> text

        card : String -> String -> Html Msg -> Html Msg
        card title description bottomChild =
            div
                [ class "bg-white rounded p-4" ]
                [ h2 [ class "font-bold" ] [ text_ title ]
                , p [ class "text-gray-900 mt-2 mb-4" ] [ text_ description ]
                , bottomChild
                ]
    in
    div [ class "mb-4" ]
        [ case community.contributionConfiguration of
            Nothing ->
                div [ class "bg-white" ]
                    [ div [ class "container mx-auto px-8 py-6" ]
                        [ h2 [ class "font-bold" ] [ text_ "sponsorship.guidance" ]
                        , p [ class "text-gray-900" ] [ text_ "sponsorship.guidance_description" ]
                        ]
                    ]

            Just _ ->
                text ""
        , div [ class "container mx-auto px-4 mt-4 space-y-4" ]
            [ card "sponsorship.cards.fiat.title"
                "sponsorship.cards.fiat.description"
                (div []
                    [ Form.Toggle.init { label = text_ "sponsorship.cards.fiat.toggle_label", id = "fiat-money-toggle" }
                        |> Form.Toggle.withDisabled True
                        |> (\options ->
                                Form.Toggle.view options
                                    { onToggle = \_ -> NoOp
                                    , onBlur = NoOp
                                    , value =
                                        community.contributionConfiguration
                                            |> Maybe.andThen .paypalAccount
                                            |> Maybe.Extra.isJust
                                    , error = text ""
                                    , hasError = False
                                    , isRequired = False
                                    , translators = translators
                                    }
                           )
                    , case community.contributionConfiguration of
                        Nothing ->
                            text ""

                        Just _ ->
                            a
                                [ class "button button-primary w-full cursor-pointer mt-6"
                                , Route.href Route.CommunitySettingsSponsorshipFiat
                                ]
                                [ text_ "sponsorship.cards.fiat.settings" ]
                    ]
                )
            , card "sponsorship.cards.crypto.title"
                "sponsorship.cards.crypto.description"
                (Form.Toggle.init
                    { label = text_ "sponsorship.cards.crypto.toggle_label"
                    , id = "cryptocurrency-toggle"
                    }
                    |> Form.Toggle.withDisabled True
                    |> (\options ->
                            Form.Toggle.view options
                                { onToggle = \_ -> NoOp
                                , onBlur = NoOp
                                , value = False
                                , error = text ""
                                , hasError = False
                                , isRequired = False
                                , translators = translators
                                }
                       )
                )
            , card "sponsorship.cards.message.title"
                "sponsorship.cards.message.description"
                (case community.contributionConfiguration of
                    Nothing ->
                        button
                            [ class "button button-primary w-full"
                            , disabled True
                            ]
                            [ text_ "sponsorship.cards.message.button_label" ]

                    Just _ ->
                        a
                            [ class "button button-primary w-full cursor-pointer"
                            , Route.href Route.CommunitySettingsSponsorshipThankYouMessage
                            ]
                            [ text_ "sponsorship.cards.message.button_label" ]
                )
            ]
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
