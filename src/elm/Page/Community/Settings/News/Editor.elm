module Page.Community.Settings.News.Editor exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Api.Graphql
import Browser.Dom
import Cambiatus.Mutation
import Cambiatus.Query
import Cambiatus.Scalar
import Community
import Community.News
import Date
import DatePicker
import Eos
import Eos.Account
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, hr, img, p, span, text)
import Html.Attributes exposing (class, disabled, src, tabindex, type_)
import Html.Events exposing (onSubmit)
import Iso8601
import Log
import Maybe.Extra
import Page
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Task
import Time
import Time.Extra
import UpdateResult as UR
import Utils
import View.Components
import View.Feedback as Feedback
import View.Form
import View.Form.Input as Input
import View.Form.Radio as Radio
import View.MarkdownEditor as MarkdownEditor



-- MODEL


type Model
    = WaitingNewsToCopy
    | WaitingNewsToEdit
    | Editing Form
    | NewsNotFound
    | WithError (Graphql.Http.Error (Maybe Community.News.Model))


init : Route.NewsEditorKind -> LoggedIn.Model -> ( Model, Cmd Msg )
init kind loggedIn =
    let
        queryForNews newsId =
            Api.Graphql.query loggedIn.shared
                (Just loggedIn.authToken)
                (Cambiatus.Query.news { newsId = newsId }
                    Community.News.selectionSet
                )
                CompletedLoadNews

        initWithCommunity =
            LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    in
    case kind of
        Route.CreateNews ->
            ( Editing emptyForm, initWithCommunity )

        Route.EditNews newsId ->
            ( WaitingNewsToEdit
            , Cmd.batch [ queryForNews newsId, initWithCommunity ]
            )

        Route.CopyNews newsId ->
            ( WaitingNewsToCopy
            , Cmd.batch [ queryForNews newsId, initWithCommunity ]
            )


type alias Form =
    { title : String
    , titleError : Maybe String
    , descriptionEditor : MarkdownEditor.Model
    , descriptionError : Maybe String
    , publicationMode : PublicationMode
    , action : Action
    , isSaving : Bool
    }


descriptionEditorId : String
descriptionEditorId =
    "description-editor"


emptyForm : Form
emptyForm =
    { title = ""
    , titleError = Nothing
    , descriptionEditor = MarkdownEditor.init descriptionEditorId
    , descriptionError = Nothing
    , publicationMode = PublishImmediately
    , action = CreateNew
    , isSaving = False
    }


emptySchedulingForm : Shared -> SchedulingForm
emptySchedulingForm shared =
    let
        defaultDate =
            shared.now
                |> Date.fromPosix shared.timezone
                |> Date.add Date.Days 1
    in
    { datePicker = DatePicker.initFromDate defaultDate
    , selectedDate = defaultDate
    , selectedTime = "13:00"
    , timeError = Nothing
    , dateError = Nothing
    }


formFromExistingNews : Time.Zone -> Community.News.Model -> Action -> ( Form, Cmd Msg )
formFromExistingNews timezone news action =
    let
        ( markdownEditor, cmd ) =
            MarkdownEditor.init descriptionEditorId
                |> MarkdownEditor.forceSetContents news.description
    in
    ( { title = news.title
      , titleError = Nothing
      , descriptionEditor = markdownEditor
      , descriptionError = Nothing
      , publicationMode = publicationModeFromMaybePosix timezone news.scheduling
      , action = action
      , isSaving = False
      }
    , Cmd.map GotInitialDescriptionEditorMsg cmd
    )


publicationModeFromMaybePosix : Time.Zone -> Maybe Time.Posix -> PublicationMode
publicationModeFromMaybePosix timezone maybeTime =
    case maybeTime of
        Nothing ->
            PublishImmediately

        Just time ->
            let
                date =
                    Date.fromPosix timezone time

                hour =
                    Time.toHour timezone time
                        |> String.fromInt

                minute =
                    Time.toMinute timezone time
                        |> String.fromInt
            in
            SchedulePublication
                { datePicker = DatePicker.initFromDate date
                , selectedDate = date
                , selectedTime = String.join ":" [ hour, minute ]
                , timeError = Nothing
                , dateError = Nothing
                }



-- TYPES


type Msg
    = CompletedLoadCommunity Community.Model
    | CompletedLoadNews (RemoteData (Graphql.Http.Error (Maybe Community.News.Model)) (Maybe Community.News.Model))
    | GotInitialDescriptionEditorMsg MarkdownEditor.Msg
    | GotFormMsg FormMsg


type FormMsg
    = NoOp
    | EnteredTitle String
    | GotDescriptionEditorMsg MarkdownEditor.Msg
    | SelectedPublicationMode PublicationMode
    | SetDatePicker DatePicker.Msg
    | EnteredPublicationTime String
    | ClickedSave
    | CompletedSaving (RemoteData (Graphql.Http.Error (Maybe Community.News.Model)) (Maybe Community.News.Model))
    | GotEditorSummaryMsg Profile.Summary.Msg


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type alias FormUpdateResult =
    UR.UpdateResult Form FormMsg (LoggedIn.External Msg)


type Action
    = CreateNew
    | EditExisting Community.News.Model Profile.Summary.Model


type PublicationMode
    = PublishImmediately
    | SchedulePublication SchedulingForm


type alias SchedulingForm =
    { datePicker : DatePicker.DatePicker
    , selectedDate : Date.Date
    , selectedTime : String
    , timeError : Maybe String
    , dateError : Maybe String
    }


type ParsedDateTime
    = NoTimeToParse
    | InvalidTime
    | ValidTime Cambiatus.Scalar.DateTime



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
                Editing _ ->
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
                    let
                        ( form, cmd ) =
                            formFromExistingNews loggedIn.shared.timezone news CreateNew
                    in
                    Editing form
                        |> UR.init
                        |> UR.addCmd cmd

                WaitingNewsToEdit ->
                    let
                        ( form, cmd ) =
                            formFromExistingNews loggedIn.shared.timezone
                                news
                                (EditExisting news (Profile.Summary.init False))
                    in
                    Editing form
                        |> UR.init
                        |> UR.addCmd cmd

        CompletedLoadNews (RemoteData.Success Nothing) ->
            NewsNotFound
                |> UR.init

        CompletedLoadNews (RemoteData.Failure err) ->
            if Utils.errorToString err == "News not found" then
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

        GotInitialDescriptionEditorMsg subMsg ->
            case model of
                Editing form ->
                    let
                        ( descriptionEditor, cmd ) =
                            MarkdownEditor.update subMsg form.descriptionEditor
                    in
                    { form | descriptionEditor = descriptionEditor }
                        |> Editing
                        |> UR.init
                        |> UR.addCmd (Cmd.map GotInitialDescriptionEditorMsg cmd)

                _ ->
                    UR.init model

        GotFormMsg subMsg ->
            case model of
                Editing form ->
                    updateForm subMsg form loggedIn
                        |> UR.map Editing GotFormMsg UR.addExt

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


updateForm : FormMsg -> Form -> LoggedIn.Model -> FormUpdateResult
updateForm msg form loggedIn =
    case msg of
        NoOp ->
            UR.init form

        EnteredTitle title ->
            { form
                | title = title
                , titleError =
                    if String.isEmpty title then
                        Just "news.editor.error.title"

                    else
                        Nothing
            }
                |> UR.init

        GotDescriptionEditorMsg subMsg ->
            let
                ( descriptionEditor, cmd ) =
                    MarkdownEditor.update subMsg form.descriptionEditor

                previousContents =
                    String.trim form.descriptionEditor.contents

                newContents =
                    String.trim descriptionEditor.contents

                contentChanged =
                    newContents /= previousContents

                hasError =
                    case form.descriptionError of
                        Nothing ->
                            False

                        Just _ ->
                            True
            in
            { form
                | descriptionEditor = descriptionEditor
                , descriptionError =
                    if
                        String.isEmpty newContents
                            && (contentChanged || hasError)
                    then
                        Just "news.editor.error.description"

                    else
                        Nothing
            }
                |> UR.init
                |> UR.addCmd (Cmd.map GotDescriptionEditorMsg cmd)

        SelectedPublicationMode publicationMode ->
            { form | publicationMode = publicationMode }
                |> UR.init

        SetDatePicker subMsg ->
            case form.publicationMode of
                PublishImmediately ->
                    form
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried to change communication publication date, but is set to publish immediately"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "updateForm" }
                            []

                SchedulePublication scheduling ->
                    let
                        ( newDatePicker, dateEvent ) =
                            DatePicker.update (datePickerSettings (Maybe.Extra.isJust scheduling.dateError))
                                subMsg
                                scheduling.datePicker

                        newSelectedDate =
                            case dateEvent of
                                DatePicker.Picked newDate ->
                                    newDate

                                _ ->
                                    scheduling.selectedDate

                        schedulingWithDate =
                            { scheduling
                                | datePicker = newDatePicker
                                , selectedDate = newSelectedDate
                            }
                    in
                    { form
                        | publicationMode =
                            SchedulePublication
                                { schedulingWithDate
                                    | dateError =
                                        if Maybe.Extra.isJust scheduling.timeError then
                                            Nothing

                                        else
                                            case parseSchedulingForm loggedIn.shared.timezone schedulingWithDate of
                                                Just time ->
                                                    if Time.posixToMillis loggedIn.shared.now >= Time.posixToMillis time then
                                                        Just "news.editor.error.future_date"

                                                    else
                                                        Nothing

                                                Nothing ->
                                                    Just "news.editor.error.invalid_date"
                                }
                    }
                        |> UR.init

        EnteredPublicationTime publicationTime ->
            case form.publicationMode of
                PublishImmediately ->
                    form
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried to change communication publication time, but is set to publish immediately"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "updateForm" }
                            []

                SchedulePublication scheduling ->
                    let
                        schedulingWithTime =
                            { scheduling | selectedTime = publicationTime }
                    in
                    { form
                        | publicationMode =
                            SchedulePublication
                                { schedulingWithTime
                                    | timeError =
                                        case parseSchedulingForm loggedIn.shared.timezone schedulingWithTime of
                                            Just time ->
                                                if Time.posixToMillis loggedIn.shared.now >= Time.posixToMillis time then
                                                    Just "news.editor.error.future_time"

                                                else
                                                    Nothing

                                            Nothing ->
                                                Just "news.editor.error.invalid_time"
                                }
                    }
                        |> UR.init

        ClickedSave ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        mutation : Maybe Cambiatus.Scalar.DateTime -> SelectionSet (Maybe Community.News.Model) RootMutation
                        mutation scheduling =
                            case form.action of
                                CreateNew ->
                                    Cambiatus.Mutation.news
                                        (\optionals ->
                                            { optionals | scheduling = OptionalArgument.fromMaybe scheduling }
                                        )
                                        { communityId = Eos.symbolToString community.symbol
                                        , description = String.trim form.descriptionEditor.contents
                                        , title = form.title
                                        }
                                        Community.News.selectionSet

                                EditExisting news _ ->
                                    Cambiatus.Mutation.updateNews
                                        (\optionals ->
                                            { optionals
                                                | description = OptionalArgument.Present (String.trim form.descriptionEditor.contents)
                                                , scheduling = OptionalArgument.fromMaybeWithNull scheduling
                                                , title = OptionalArgument.Present form.title
                                            }
                                        )
                                        { id = news.id }
                                        Community.News.selectionSet

                        saveNews : Maybe Cambiatus.Scalar.DateTime -> Cmd FormMsg
                        saveNews scheduling =
                            Api.Graphql.mutation loggedIn.shared
                                (Just loggedIn.authToken)
                                (mutation scheduling)
                                CompletedSaving

                        isModelValid =
                            Maybe.Extra.isNothing form.titleError
                                && not (String.isEmpty form.title)
                                && Maybe.Extra.isNothing form.descriptionError
                                && not (String.isEmpty form.descriptionEditor.contents)
                    in
                    if not isModelValid then
                        { form
                            | titleError =
                                if String.isEmpty form.title then
                                    Just "news.editor.error.title"

                                else
                                    Nothing
                            , descriptionError =
                                if String.isEmpty form.descriptionEditor.contents then
                                    Just "news.editor.error.description"

                                else
                                    Nothing
                        }
                            |> UR.init
                            |> UR.addCmd
                                (Browser.Dom.setViewport 0 0
                                    |> Task.perform (\_ -> NoOp)
                                )

                    else
                        case parseDateTime loggedIn.shared.timezone form.publicationMode of
                            NoTimeToParse ->
                                { form | isSaving = True }
                                    |> UR.init
                                    |> UR.addCmd (saveNews Nothing)

                            ValidTime time ->
                                { form | isSaving = True }
                                    |> UR.init
                                    |> UR.addCmd (saveNews (Just time))

                            InvalidTime ->
                                UR.init form

                _ ->
                    form
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried saving communication, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "updateForm" }
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
            { form | isSaving = False }
                |> UR.init
                |> setAsHighlighted
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Success
                        (loggedIn.shared.translators.t "news.saved")
                    )
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.CommunitySettingsNews)

        CompletedSaving (RemoteData.Failure _) ->
            { form | isSaving = False }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (loggedIn.shared.translators.t "news.failed_saving")
                    )

        CompletedSaving RemoteData.NotAsked ->
            UR.init form

        CompletedSaving RemoteData.Loading ->
            UR.init form

        GotEditorSummaryMsg subMsg ->
            case form.action of
                CreateNew ->
                    UR.init form

                EditExisting news profileSummary ->
                    { form
                        | action =
                            profileSummary
                                |> Profile.Summary.update subMsg
                                |> EditExisting news
                    }
                        |> UR.init


parseDateTime : Time.Zone -> PublicationMode -> ParsedDateTime
parseDateTime timezone publicationMode =
    case publicationMode of
        PublishImmediately ->
            NoTimeToParse

        SchedulePublication schedulingForm ->
            schedulingForm
                |> parseSchedulingForm timezone
                |> Maybe.map
                    (Iso8601.fromTime
                        >> Cambiatus.Scalar.DateTime
                        >> ValidTime
                    )
                |> Maybe.withDefault InvalidTime


parseSchedulingForm : Time.Zone -> SchedulingForm -> Maybe Time.Posix
parseSchedulingForm timezone { selectedDate, selectedTime } =
    case String.split ":" selectedTime of
        [ hourString, minuteString ] ->
            Maybe.map2
                (\hour minute ->
                    Time.Extra.partsToPosix timezone
                        { year = Date.year selectedDate
                        , month = Date.month selectedDate
                        , day = Date.day selectedDate
                        , hour = hour
                        , minute = minute
                        , second = 0
                        , millisecond = 0
                        }
                )
                (String.toInt hourString)
                (String.toInt minuteString)

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
                Editing form ->
                    div [ class "container mx-auto pt-4 pb-10" ]
                        [ viewForm loggedIn form
                            |> Html.map GotFormMsg
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


viewForm : LoggedIn.Model -> Form -> Html FormMsg
viewForm ({ shared } as loggedIn) form =
    let
        { translators } =
            shared
    in
    Html.form
        [ class "px-4"
        , onSubmit ClickedSave
        ]
        [ Input.init
            { label = translators.t "news.editor.field.title"
            , id = "title-input"
            , onInput = EnteredTitle
            , disabled = form.isSaving
            , value = form.title
            , placeholder = Just "Lorem ipsum dolor"
            , problems = Maybe.map (translators.t >> List.singleton) form.titleError
            , translators = translators
            }
            |> Input.toHtml
        , MarkdownEditor.view
            { translators = translators
            , placeholder = Nothing
            , label = translators.t "news.editor.field.description"
            , problem = Maybe.map translators.t form.descriptionError
            , disabled = form.isSaving
            }
            []
            form.descriptionEditor
            |> Html.map GotDescriptionEditorMsg
        , case form.action of
            CreateNew ->
                text ""

            EditExisting news profileSummary ->
                viewLatestEditions loggedIn news profileSummary
        , hr [ class "mt-5 mb-10 text-gray-500" ] []
        , Radio.init
            { label = translators.t "news.editor.field.publication_mode"
            , name = "publish-mode-radio"
            , optionToString =
                \option ->
                    case option of
                        PublishImmediately ->
                            "publish-immediately"

                        SchedulePublication _ ->
                            "schedule-publication"
            , activeOption = form.publicationMode
            , onSelect = SelectedPublicationMode
            , areOptionsEqual =
                \option1 option2 ->
                    case ( option1, option2 ) of
                        ( SchedulePublication _, SchedulePublication _ ) ->
                            True

                        ( PublishImmediately, PublishImmediately ) ->
                            True

                        _ ->
                            False
            }
            |> Radio.withOption PublishImmediately
                (\_ -> text <| translators.t "news.editor.field.publish_immediately")
            |> Radio.withOption (SchedulePublication (emptySchedulingForm shared))
                (\_ -> text <| translators.t "news.editor.field.schedule")
            |> Radio.withVertical True
            |> Radio.withDisabled form.isSaving
            |> Radio.toHtml translators
        , case form.publicationMode of
            PublishImmediately ->
                text ""

            SchedulePublication scheduling ->
                div [ class "flex space-x-4" ]
                    [ div [ class "w-full mb-4" ]
                        [ View.Form.label []
                            "datepicker-input"
                            (translators.t "news.editor.field.initial_date")
                        , div [ class "relative" ]
                            [ DatePicker.view (Just scheduling.selectedDate)
                                (datePickerSettings (Maybe.Extra.isJust scheduling.dateError))
                                scheduling.datePicker
                                |> Html.map SetDatePicker
                            , img
                                [ src "/icons/calendar.svg"
                                , tabindex -1
                                , class "absolute right-0 top-0 h-full pointer-events-none"
                                ]
                                []
                            ]
                        , case scheduling.dateError of
                            Nothing ->
                                text ""

                            Just error ->
                                span [ class "form-error" ]
                                    [ text <| translators.t error ]
                        ]
                    , Input.init
                        { label = translators.t "news.editor.field.time"
                        , id = "time-input"
                        , onInput = EnteredPublicationTime
                        , disabled = form.isSaving
                        , value = scheduling.selectedTime
                        , placeholder = Nothing
                        , problems = Maybe.map (translators.t >> List.singleton) scheduling.timeError
                        , translators = translators
                        }
                        |> Input.withContainerAttrs [ class "w-full" ]
                        |> Input.withType Input.Time
                        |> Input.toHtml
                    ]
        , button
            [ type_ "submit"
            , disabled form.isSaving
            , class "button button-primary w-full"
            ]
            [ text <| translators.t "menu.save" ]
        ]


viewLatestEditions : LoggedIn.Model -> Community.News.Model -> Profile.Summary.Model -> Html FormMsg
viewLatestEditions ({ shared } as loggedIn) news profileSummary =
    if news.insertedAt == news.updatedAt then
        text ""

    else
        div []
            [ p [ class "label mb-6" ] [ text "Latest editions" ]
            , div [ class "flex items-center" ]
                [ profileSummary
                    |> Profile.Summary.withoutName
                    |> Profile.Summary.withImageSize "h-8 w-8"
                    |> Profile.Summary.view shared
                        loggedIn.accountName
                        news.creator
                    |> Html.map GotEditorSummaryMsg
                , p [ class "text-gray-900 ml-2" ]
                    [ text <| shared.translators.t "news.edited_by"
                    , a
                        [ class "font-bold hover:underline"
                        , Route.href (Route.Profile news.creator.account)
                        ]
                        [ news.creator.name
                            |> Maybe.withDefault (Eos.Account.nameToString news.creator.account)
                            |> text
                        ]
                    , View.Components.dateViewer []
                        (\translations ->
                            { translations
                                | today = Nothing
                                , yesterday = Nothing
                                , other = shared.translators.t "news.edited_date"
                            }
                        )
                        shared
                        news.updatedAt
                    ]
                ]
            ]


datePickerSettings : Bool -> DatePicker.Settings
datePickerSettings hasError =
    let
        defaultSettings =
            DatePicker.defaultSettings
    in
    { defaultSettings
        | changeYear = DatePicker.off
        , inputId = Just "datepicker-input"
        , inputClassList =
            [ ( "input w-full", True )
            , ( "with-error", hasError )
            ]
        , containerClassList = [ ( "relative-table", True ) ]
        , dateFormatter = Date.format "E, d MMM y"
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

        GotInitialDescriptionEditorMsg subMsg ->
            "GotInitialDescriptionEditorMsg" :: MarkdownEditor.msgToString subMsg

        GotFormMsg subMsg ->
            "GotFormMsg" :: formMsgToString subMsg


formMsgToString : FormMsg -> List String
formMsgToString formMsg =
    case formMsg of
        NoOp ->
            [ "NoOp" ]

        EnteredTitle _ ->
            [ "EnteredTitle" ]

        GotDescriptionEditorMsg subMsg ->
            "GotDescriptionEditorMsg" :: MarkdownEditor.msgToString subMsg

        SelectedPublicationMode _ ->
            [ "SelectedPublicationMode" ]

        SetDatePicker _ ->
            [ "SetDatePicker" ]

        EnteredPublicationTime _ ->
            [ "EnteredPublicationTime" ]

        ClickedSave ->
            [ "ClickedSave" ]

        CompletedSaving _ ->
            [ "CompletedSaving" ]

        GotEditorSummaryMsg subMsg ->
            "GotEditorSummaryMsg" :: Profile.Summary.msgToString subMsg
