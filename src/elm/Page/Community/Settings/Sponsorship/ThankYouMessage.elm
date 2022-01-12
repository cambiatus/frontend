module Page.Community.Settings.Sponsorship.ThankYouMessage exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Form
import Form.RichText
import Form.Text
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Markdown exposing (Markdown)
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared exposing (Shared)
import UpdateResult as UR



-- MODEL


type alias Model =
    { form : FormStatus
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { form = Loading }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )


createForm : Shared.Translators -> Community.Model -> Form.Form msg FormInput FormOutput
createForm { t, tr } community =
    Form.succeed FormOutput
        |> Form.with
            (Form.Text.init
                { label = t "sponsorship.thank_you_message.title"
                , id = "title-input"
                }
                |> Form.Text.withPlaceholder
                    (tr "sponsorship.thank_you_message.default_title"
                        [ ( "community", community.name ) ]
                    )
                |> Form.textField
                    { parser = Ok
                    , value = .title
                    , update = \title input -> { input | title = title }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.RichText.init
                { label = t "sponsorship.thank_you_message.message"
                }
                |> Form.RichText.withPlaceholder (t "sponsorship.thank_you_message.default_message")
                |> Form.richText
                    { parser = Ok
                    , value = .message
                    , update = \message input -> { input | message = message }
                    , externalError = always Nothing
                    }
            )



-- TYPES


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | GotFormMsg (Form.Msg FormInput)


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type FormStatus
    = Loading
    | Loaded (Form.Model FormInput)


type alias FormInput =
    { title : String
    , message : Form.RichText.Model
    }


type alias FormOutput =
    { title : String
    , message : Markdown
    }



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

                defaultTitle =
                    loggedIn.shared.translators.tr "sponsorship.thank_you_message.default_title"
                        [ ( "community", community.name ) ]

                defaultMessage =
                    Markdown.fromTranslation loggedIn.shared.translators
                        "sponsorship.thank_you_message.default_message"
            in
            { model
                | form =
                    { title =
                        community.contributionConfiguration
                            |> Maybe.andThen .thankYouTitle
                            |> Maybe.withDefault defaultTitle
                    , message =
                        community.contributionConfiguration
                            |> Maybe.andThen .thankYouDescription
                            |> Maybe.withDefault defaultMessage
                            |> Just
                            |> Form.RichText.initModel "message-input"
                    }
                        |> Form.init
                        |> Loaded
            }
                |> UR.init
                |> maybeRedirect

        GotFormMsg subMsg ->
            case model.form of
                Loading ->
                    UR.init model

                Loaded form ->
                    Form.update loggedIn.shared subMsg form
                        |> UR.fromChild (\newForm -> { model | form = Loaded newForm })
                            GotFormMsg
                            LoggedIn.addFeedback
                            model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            loggedIn.shared.translators.t "sponsorship.cards.message.title"

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    case model.form of
                        Loading ->
                            Page.fullPageLoading loggedIn.shared

                        Loaded form ->
                            div [ class "flex-grow flex flex-col" ]
                                [ Page.viewHeader loggedIn title
                                , view_ loggedIn.shared community form
                                ]

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { title = title, content = content }


view_ : Shared -> Community.Model -> Form.Model FormInput -> Html Msg
view_ { translators } community form =
    div [ class "bg-white flex-grow" ]
        [ Form.view [ class "container mx-auto p-4" ]
            translators
            (\_ -> [])
            (createForm translators community)
            (Form.withDisabled True form)
            { toMsg = GotFormMsg
            , onSubmit = \_ -> NoOp
            }
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

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg
