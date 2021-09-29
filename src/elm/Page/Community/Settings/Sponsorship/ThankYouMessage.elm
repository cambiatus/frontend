module Page.Community.Settings.Sponsorship.ThankYouMessage exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import UpdateResult as UR
import View.Form.Input
import View.MarkdownEditor



-- MODEL


type alias Model =
    { descriptionInput : View.MarkdownEditor.Model
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { descriptionInput = View.MarkdownEditor.init "thank-you-message-description-input" }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- TYPES


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | GotDescriptionEditorMsg View.MarkdownEditor.Msg


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
            { model
                | descriptionInput =
                    community.contributionConfiguration
                        |> Maybe.andThen .thankYouDescription
                        |> Maybe.withDefault (loggedIn.shared.translators.t "sponsorship.thank_you_message.default_message")
                        |> (\contents -> View.MarkdownEditor.setContents contents model.descriptionInput)
            }
                |> UR.init
                |> maybeRedirect

        GotDescriptionEditorMsg subMsg ->
            let
                ( descriptionInput, descriptionInputCmd ) =
                    View.MarkdownEditor.update subMsg model.descriptionInput
            in
            { model | descriptionInput = descriptionInput }
                |> UR.init
                |> UR.addCmd (Cmd.map GotDescriptionEditorMsg descriptionInputCmd)



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            loggedIn.shared.translators.t "sponsorship.cards.message.title"

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    div [ class "flex-grow flex flex-col" ]
                        [ Page.viewHeader loggedIn title
                        , view_ loggedIn.shared community model
                        ]

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { title = title, content = content }


view_ : Shared -> Community.Model -> Model -> Html Msg
view_ { translators } community model =
    div [ class "bg-white flex-grow" ]
        [ div [ class "container mx-auto p-4" ]
            [ View.Form.Input.init
                { label = translators.t "sponsorship.thank_you_message.title"
                , id = "thank-you-title-input"
                , onInput = \_ -> NoOp
                , disabled = True
                , value =
                    community.contributionConfiguration
                        |> Maybe.andThen .thankYouTitle
                        |> Maybe.withDefault (translators.t "sponsorship.thank_you_message.default_title")
                , placeholder = Just (translators.t "sponsorship.thank_you_message.default_title")
                , problems = Nothing
                , translators = translators
                }
                |> View.Form.Input.toHtml
            , View.MarkdownEditor.view
                { translators = translators
                , placeholder = Just (translators.t "sponsorship.thank_you_message.default_message")
                , label = translators.t "sponsorship.thank_you_message.message"
                , problem = Nothing
                , disabled = True
                }
                []
                model.descriptionInput
                |> Html.map GotDescriptionEditorMsg
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

        GotDescriptionEditorMsg subMsg ->
            "GotDescriptionEditorMsg" :: View.MarkdownEditor.msgToString subMsg
