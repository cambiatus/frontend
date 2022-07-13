module Page.Community.Settings.News.Editor exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Api.Graphql
import Cambiatus.Mutation
import Cambiatus.Query
import Cambiatus.Scalar
import Community
import Community.News
import Date
import Eos.Account
import Form
import Form.DatePicker
import Form.Radio
import Form.RichText
import Form.Text
import Form.Validate
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument
import Html exposing (Html, a, div, hr, p, text)
import Html.Attributes exposing (class)
import Iso8601
import Log
import Markdown exposing (Markdown)
import Page
import Profile
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Time
import Time.Extra
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback



-- MODEL


type Model
    = WaitingNewsToCopy
    | WaitingNewsToEdit
    | Editing Action (Form.Model FormInput)
    | NewsNotFound
    | WithError (Graphql.Http.Error (Maybe Community.News.Model))


init : Route.NewsEditorKind -> LoggedIn.Model -> UpdateResult
init kind loggedIn =
    let
        queryForNews newsId =
            LoggedIn.query loggedIn
                (Cambiatus.Query.news { newsId = newsId }
                    Community.News.selectionSet
                )
                CompletedLoadNews
                |> UR.addExt

        initWithCommunity =
            LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    in
    case kind of
        Route.CreateNews ->
            Editing CreateNew (emptyForm loggedIn.shared)
                |> UR.init
                |> UR.addCmd initWithCommunity

        Route.EditNews newsId ->
            let
                model =
                    WaitingNewsToEdit
            in
            model
                |> UR.init
                |> UR.addCmd initWithCommunity
                |> queryForNews newsId

        Route.CopyNews newsId ->
            let
                model =
                    WaitingNewsToCopy
            in
            model
                |> UR.init
                |> UR.addCmd initWithCommunity
                |> queryForNews newsId


emptyForm : Shared -> Form.Model FormInput
emptyForm shared =
    Form.init
        { title = ""
        , description = Form.RichText.initModel "description-input" Nothing
        , latestEditions = Nothing
        , publicationMode = PublishImmediately
        , publicationDate = Form.DatePicker.initModel (Date.fromPosix shared.timezone shared.now)
        , publicationTime = "13:00"
        }


formFromExistingNews : Shared -> Community.News.Model -> Form.Model FormInput
formFromExistingNews shared news =
    Form.init
        { title = news.title
        , description = Form.RichText.initModel "description-input" (Just news.description)
        , latestEditions =
            if news.updatedAt == news.insertedAt then
                Nothing

            else
                Just
                    { creator = news.creator
                    , profileSummary = Profile.Summary.init False
                    , updatedAt = news.updatedAt
                    }
        , publicationMode =
            case news.scheduling of
                Nothing ->
                    PublishImmediately

                Just _ ->
                    SchedulePublication
        , publicationDate =
            news.scheduling
                |> Maybe.withDefault shared.now
                |> Date.fromPosix shared.timezone
                |> Form.DatePicker.initModel
        , publicationTime =
            news.scheduling
                |> Maybe.map
                    (\posix ->
                        String.fromInt (Time.toHour shared.timezone posix)
                            ++ ":"
                            ++ String.fromInt (Time.toMinute shared.timezone posix)
                    )
                |> Maybe.withDefault "13:00"
        }



-- TYPES


type Msg
    = CompletedLoadCommunity Community.Model
    | CompletedLoadNews (RemoteData (Graphql.Http.Error (Maybe Community.News.Model)) (Maybe Community.News.Model))
    | GotFormMsg (Form.Msg FormInput)
    | SubmittedForm FormOutput
    | CompletedSaving (RemoteData (Graphql.Http.Error (Maybe Community.News.Model)) (Maybe Community.News.Model))


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type Action
    = CreateNew
    | EditExisting Community.News.Model


type PublicationMode
    = PublishImmediately
    | SchedulePublication



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                UR.init model
                    |> UR.addExt (LoggedIn.ReloadResource LoggedIn.TimeResource)

            else
                UR.init model
                    |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.Dashboard)

        CompletedLoadNews (RemoteData.Success (Just news)) ->
            case model of
                Editing _ _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Loaded news, but was already editing"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

                NewsNotFound ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Loaded news, but state said news haven't been found"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

                WithError _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Loaded news, but state said there was already an error"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

                WaitingNewsToCopy ->
                    Editing CreateNew (formFromExistingNews loggedIn.shared news)
                        |> UR.init

                WaitingNewsToEdit ->
                    formFromExistingNews loggedIn.shared news
                        |> Editing (EditExisting news)
                        |> UR.init

        CompletedLoadNews (RemoteData.Success Nothing) ->
            NewsNotFound
                |> UR.init

        CompletedLoadNews (RemoteData.Failure err) ->
            if Api.Graphql.isNewsNotFoundError err then
                NewsNotFound
                    |> UR.init

            else
                WithError err
                    |> UR.init
                    |> UR.logGraphqlError msg
                        (Just loggedIn.accountName)
                        "Got an error when loading news to edit"
                        { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                        []
                        err

        CompletedLoadNews RemoteData.NotAsked ->
            UR.init model

        CompletedLoadNews RemoteData.Loading ->
            UR.init model

        GotFormMsg subMsg ->
            case model of
                Editing action form ->
                    Form.update loggedIn.shared subMsg form
                        |> UR.fromChild (Editing action)
                            GotFormMsg
                            LoggedIn.addFeedback
                            model

                WaitingNewsToCopy ->
                    UR.init model
                        |> UR.logIncompatibleMsg msg
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            []

                WaitingNewsToEdit ->
                    UR.init model
                        |> UR.logIncompatibleMsg msg
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            []

                NewsNotFound ->
                    UR.init model
                        |> UR.logIncompatibleMsg msg
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            []

                WithError _ ->
                    UR.init model
                        |> UR.logIncompatibleMsg msg
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            []

        SubmittedForm formOutput ->
            case model of
                Editing action _ ->
                    let
                        schedulingTime =
                            formOutput.publicationDate
                                |> Maybe.map (Iso8601.fromTime >> Cambiatus.Scalar.DateTime)

                        optionalArgs optionals =
                            case action of
                                CreateNew ->
                                    { optionals | scheduling = OptionalArgument.fromMaybe schedulingTime }

                                EditExisting news ->
                                    { optionals
                                        | id = OptionalArgument.Present news.id
                                        , scheduling = OptionalArgument.fromMaybeWithNull schedulingTime
                                    }

                        mutation =
                            Cambiatus.Mutation.news
                                optionalArgs
                                { description = Markdown.toRawString formOutput.description
                                , title = formOutput.title
                                }
                                Community.News.selectionSet
                    in
                    model
                        |> setDisabled True
                        |> UR.init
                        |> UR.addExt (LoggedIn.mutation loggedIn mutation CompletedSaving)

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Tried saving communication, but wasn't editing"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

        CompletedSaving (RemoteData.Success maybeNews) ->
            let
                setAsHighlighted =
                    case maybeNews of
                        Nothing ->
                            identity

                        Just news ->
                            case news.scheduling of
                                Nothing ->
                                    setHighlighted news

                                Just scheduling ->
                                    if Time.posixToMillis scheduling <= Time.posixToMillis loggedIn.shared.now then
                                        setHighlighted news

                                    else
                                        identity

                setHighlighted news =
                    UR.addExt
                        (LoggedIn.UpdatedLoggedIn
                            { loggedIn
                                | selectedCommunity =
                                    RemoteData.map (\community -> { community | highlightedNews = Just news })
                                        loggedIn.selectedCommunity
                            }
                        )
            in
            model
                |> setDisabled False
                |> UR.init
                |> setAsHighlighted
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Success
                        (loggedIn.shared.translators.t "news.saved")
                    )
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.CommunitySettingsNews)

        CompletedSaving (RemoteData.Failure _) ->
            model
                |> setDisabled False
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (loggedIn.shared.translators.t "news.failed_saving")
                    )

        CompletedSaving RemoteData.NotAsked ->
            UR.init model

        CompletedSaving RemoteData.Loading ->
            UR.init model


setDisabled : Bool -> Model -> Model
setDisabled isDisabled model =
    case model of
        Editing action form ->
            Editing action (Form.withDisabled isDisabled form)

        _ ->
            model


type alias FormInput =
    { title : String
    , description : Form.RichText.Model
    , publicationMode : PublicationMode
    , latestEditions :
        Maybe
            { creator : Profile.Minimal
            , profileSummary : Profile.Summary.Model
            , updatedAt : Time.Posix
            }
    , publicationDate : Form.DatePicker.Model
    , publicationTime : String
    }


type alias FormOutput =
    { title : String
    , description : Markdown
    , publicationDate : Maybe Time.Posix
    }


createForm : LoggedIn.Model -> Form.Form msg FormInput FormOutput
createForm loggedIn =
    let
        { t } =
            loggedIn.shared.translators
    in
    Form.succeed FormOutput
        |> Form.with
            (Form.Text.init { label = t "news.editor.field.title", id = "title-input" }
                |> Form.textField
                    { parser =
                        \title ->
                            if String.isEmpty title then
                                Err <| t "news.editor.error.title"

                            else
                                Ok title
                    , value = .title
                    , update = \title input -> { input | title = title }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.RichText.init { label = t "news.editor.field.description" }
                |> Form.RichText.withEditorContainerAttrs [ class "mb-10" ]
                |> Form.richText
                    { parser =
                        \description ->
                            if description == Markdown.empty then
                                Err <| t "news.editor.error.description"

                            else
                                Ok description
                    , value = .description
                    , update = \description input -> { input | description = description }
                    , externalError = always Nothing
                    }
            )
        |> Form.withNoOutput
            ((\{ latestEditions } ->
                case latestEditions of
                    Nothing ->
                        Form.succeed ()

                    Just { profileSummary, creator, updatedAt } ->
                        div []
                            [ p [ class "label mb-6" ] [ text "Latest editions" ]
                            , div [ class "flex items-center" ]
                                [ profileSummary
                                    |> Profile.Summary.withoutName
                                    |> Profile.Summary.withImageSize "h-8 w-8"
                                    |> Profile.Summary.view loggedIn.shared.translators
                                        loggedIn.accountName
                                        creator
                                    |> Html.map
                                        (\summarySubMsg ->
                                            \values ->
                                                { values
                                                    | latestEditions =
                                                        values.latestEditions
                                                            |> Maybe.map
                                                                (\latestEdit ->
                                                                    { latestEdit
                                                                        | profileSummary =
                                                                            Profile.Summary.update summarySubMsg latestEdit.profileSummary
                                                                    }
                                                                )
                                                }
                                        )
                                , p [ class "text-gray-900 ml-2" ]
                                    [ text <| t "news.edited_by"
                                    , a
                                        [ class "font-bold hover:underline"
                                        , Route.href (Route.Profile creator.account)
                                        ]
                                        [ creator.name
                                            |> Maybe.withDefault (Eos.Account.nameToString creator.account)
                                            |> text
                                        ]
                                    , View.Components.dateViewer []
                                        (\translations ->
                                            { translations
                                                | today = Nothing
                                                , yesterday = Nothing
                                                , other = t "news.edited_date"
                                            }
                                        )
                                        loggedIn.shared
                                        updatedAt
                                    ]
                                ]
                            ]
                            |> Form.arbitrary
             )
                |> Form.introspect
            )
        |> Form.withDecoration (hr [ class "mt-5 mb-10 text-gray-500" ] [])
        |> Form.with (publicationDateForm loggedIn.shared)


publicationDateForm : Shared -> Form.Form msg FormInput (Maybe Time.Posix)
publicationDateForm shared =
    let
        ({ t } as translators) =
            shared.translators
    in
    Form.succeed
        (\publicationMode publicationDate { hour, minute } ->
            case publicationMode of
                PublishImmediately ->
                    Nothing

                SchedulePublication ->
                    Just
                        (Time.Extra.partsToPosix shared.timezone
                            { year = Date.year publicationDate
                            , month = Date.month publicationDate
                            , day = Date.day publicationDate
                            , hour = hour
                            , minute = minute
                            , second = 0
                            , millisecond = 0
                            }
                        )
        )
        |> Form.with
            (Form.Radio.init
                { label = t "news.editor.field.publication_mode"
                , id = "publication-mode-radio"
                , optionToString = publicationModeToString
                }
                |> Form.Radio.withOption PublishImmediately (text <| t "news.editor.field.publish_immediately")
                |> Form.Radio.withOption SchedulePublication (text <| t "news.editor.field.schedule")
                |> Form.Radio.withDirection Form.Radio.Vertical
                |> Form.Radio.withContainerAttrs [ class "mb-6" ]
                |> Form.radio (publicationModeFromString >> Maybe.withDefault PublishImmediately)
                    { parser = Ok
                    , value = .publicationMode
                    , update = \publicationMode input -> { input | publicationMode = publicationMode }
                    , externalError = always Nothing
                    }
            )
        |> Form.withGroup [ class "grid md:grid-cols-2 md:gap-8" ]
            ((\values ->
                case values.publicationMode of
                    PublishImmediately ->
                        Form.succeed (Date.fromPosix shared.timezone shared.now)

                    SchedulePublication ->
                        Form.DatePicker.init { label = t "news.editor.field.initial_date", id = "publication-date-input" }
                            |> Form.DatePicker.withAbsolutePositioning False
                            |> Form.DatePicker.withContainerAttrs [ class "mb-10" ]
                            |> Form.datePicker
                                { parser =
                                    Form.Validate.succeed
                                        >> Form.Validate.required
                                        >> Form.Validate.futureDate shared.timezone shared.now
                                        >> Form.Validate.validate translators
                                , value = .publicationDate
                                , update = \publicationDate input -> { input | publicationDate = publicationDate }
                                , externalError = always Nothing
                                }
             )
                |> Form.introspect
            )
            ((\values ->
                case values.publicationMode of
                    PublishImmediately ->
                        Form.succeed { hour = 0, minute = 0 }

                    SchedulePublication ->
                        Form.Text.init { label = t "news.editor.field.time", id = "publication-time-input" }
                            |> Form.Text.withType Form.Text.Time
                            |> Form.textField
                                { parser =
                                    Form.Validate.succeed
                                        >> Form.Validate.time
                                        >> Form.Validate.custom
                                            (\time ->
                                                case Form.DatePicker.getDate values.publicationDate of
                                                    Nothing ->
                                                        Ok time

                                                    Just date ->
                                                        let
                                                            selectedTime =
                                                                Time.Extra.partsToPosix shared.timezone
                                                                    { year = Date.year date
                                                                    , month = Date.month date
                                                                    , day = Date.day date
                                                                    , hour = time.hour
                                                                    , minute = time.minute
                                                                    , second = 0
                                                                    , millisecond = 0
                                                                    }
                                                                    |> Time.posixToMillis
                                                        in
                                                        if selectedTime < Time.posixToMillis shared.now then
                                                            Err (\translators_ -> translators_.t "news.editor.error.future_time")

                                                        else
                                                            Ok time
                                            )
                                        >> Form.Validate.validate translators
                                , value = .publicationTime
                                , update = \publicationTime input -> { input | publicationTime = publicationTime }
                                , externalError = always Nothing
                                }
             )
                |> Form.introspect
            )


publicationModeToString : PublicationMode -> String
publicationModeToString publicationMode =
    case publicationMode of
        PublishImmediately ->
            "publish-immediately"

        SchedulePublication ->
            "schedule-publication"


publicationModeFromString : String -> Maybe PublicationMode
publicationModeFromString publicationMode =
    case publicationMode of
        "publish-immediately" ->
            Just PublishImmediately

        "schedule-publication" ->
            Just SchedulePublication

        _ ->
            Nothing



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            t "news.editor.title"
    in
    { title = title
    , content =
        div [ class "bg-white" ]
            [ Page.viewHeader loggedIn title
            , case model of
                Editing _ form ->
                    div [ class "container mx-auto pt-4 pb-10" ]
                        [ Form.view [ class "px-4" ]
                            loggedIn.shared.translators
                            (\submitButton ->
                                [ submitButton [ class "button button-primary w-full" ]
                                    [ text <| t "menu.save" ]
                                ]
                            )
                            (createForm loggedIn)
                            form
                            { toMsg = GotFormMsg
                            , onSubmit = SubmittedForm
                            }
                        ]

                NewsNotFound ->
                    Page.fullPageNotFound (t "news.not_found_title")
                        (t "news.not_found_description")

                WithError err ->
                    Page.fullPageGraphQLError
                        (t "news.error_fetching")
                        err

                WaitingNewsToCopy ->
                    Page.fullPageLoading loggedIn.shared

                WaitingNewsToEdit ->
                    Page.fullPageLoading loggedIn.shared
            ]
    }



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

        CompletedLoadNews r ->
            [ "CompletedLoadNews", UR.remoteDataToString r ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        SubmittedForm _ ->
            [ "SubmittedForm" ]

        CompletedSaving r ->
            [ "CompletedSaving", UR.remoteDataToString r ]
