module Page.Community.Settings.Sponsorship exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Html exposing (Html, a, button, div, h2, p, text)
import Html.Attributes exposing (class, disabled)
import Maybe.Extra
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import UpdateResult as UR
import View.Form.Toggle



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
        card : String -> String -> Html Msg -> Html Msg
        card title description bottomChild =
            div
                [ class "bg-white rounded p-4" ]
                [ h2 [ class "font-bold" ] [ text title ]
                , p [ class "text-gray-900 mt-2 mb-4" ] [ text description ]
                , bottomChild
                ]
    in
    div [ class "mb-4" ]
        [ case community.contributionConfiguration of
            Nothing ->
                div [ class "bg-white" ]
                    [ div [ class "container mx-auto px-8 py-6" ]
                        [ h2 [ class "font-bold" ] [ text "Guidance" ]
                        , p [ class "text-gray-900" ] [ text "To activate this feature, ask for Cambiatus help on your Slack Support Channel" ]
                        ]
                    ]

            Just _ ->
                text ""
        , div [ class "container mx-auto px-4 mt-4 space-y-4" ]
            -- TODO - I18N
            [ card "Types of currencies accepted"
                "Allow the community to accept USD or other fiat currency via PayPal"
                (div []
                    [ View.Form.Toggle.init
                        { label = text "Fiat money"
                        , id = "fiat-money-toggle"
                        , onToggle = \_ -> NoOp
                        , disabled = True
                        , value =
                            community.contributionConfiguration
                                |> Maybe.andThen .paypalAccount
                                |> Maybe.Extra.isJust
                        }
                        |> View.Form.Toggle.toHtml translators
                    , case community.contributionConfiguration of
                        Nothing ->
                            text ""

                        Just _ ->
                            a
                                [ class "button button-primary w-full cursor-pointer mt-6"

                                -- TODO - Use correct route
                                , Route.href Route.Dashboard
                                ]
                                [ text "Settings" ]
                    ]
                )
            , card "Other currencies accepted"
                "Allow the community to accept cryptocurrency"
                (View.Form.Toggle.init
                    { label = text "Cryptocurrency"
                    , id = "cryptocurrency-toggle"
                    , onToggle = \_ -> NoOp
                    , disabled = True
                    , value = False
                    }
                    |> View.Form.Toggle.toHtml translators
                )
            , card "\"Thank You\" message"
                "Edit the \"Thank you\" message to fit your community"
                (case community.contributionConfiguration of
                    Nothing ->
                        button
                            [ class "button button-primary w-full"
                            , disabled True
                            ]
                            [ text "Edit the message" ]

                    Just _ ->
                        a
                            [ class "button button-primary w-full cursor-pointer"

                            -- TODO - Use correct route
                            , Route.href Route.Dashboard
                            ]
                            [ text "Edit the message" ]
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
