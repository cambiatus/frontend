module Page.Community.Settings.News.Editor exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Api.Graphql
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
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, hr, img, p, text)
import Html.Attributes exposing (class, disabled, src, tabindex, type_)
import Html.Events exposing (onSubmit)
import Iso8601
import Log
import Page
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
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
    = WaitingNewsToCopy Int
    | WaitingNewsToEdit Int
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
            ( WaitingNewsToEdit newsId
            , Cmd.batch [ queryForNews newsId, initWithCommunity ]
            )

        Route.CopyNews newsId ->
            ( WaitingNewsToCopy newsId
            , Cmd.batch [ queryForNews newsId, initWithCommunity ]
            )


type alias Form =
    { title : String
    , descriptionEditor : MarkdownEditor.Model
    , publicationMode : PublicationMode
    , timeError : Maybe String
    , action : Action
    , isSaving : Bool
    }


descriptionEditorId : String
descriptionEditorId =
    "description-editor"


emptyForm : Form
emptyForm =
    { title = ""
    , descriptionEditor = MarkdownEditor.init descriptionEditorId
    , publicationMode = PublishImmediately
    , timeError = Nothing
    , action = CreateNew
    , isSaving = False
    }


formFromExistingNews : Time.Zone -> Community.News.Model -> Action -> ( Form, Cmd Msg )
formFromExistingNews timezone news action =
    let
        ( markdownEditor, cmd ) =
            MarkdownEditor.init descriptionEditorId
                |> MarkdownEditor.forceSetContents news.description
    in
    ( { title = news.title
      , descriptionEditor = markdownEditor
      , publicationMode = publicationModeFromMaybePosix timezone news.scheduling
      , timeError = Nothing
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
            SchedulePublication (DatePicker.initFromDate date)
                date
                (String.join ":" [ hour, minute ])



-- TYPES


type Msg
    = CompletedLoadCommunity Community.Model
    | CompletedLoadNews (RemoteData (Graphql.Http.Error (Maybe Community.News.Model)) (Maybe Community.News.Model))
    | GotInitialDescriptionEditorMsg MarkdownEditor.Msg
    | GotFormMsg FormMsg


type FormMsg
    = EnteredTitle String
    | GotDescriptionEditorMsg MarkdownEditor.Msg
    | SelectedPublicationMode PublicationMode
    | SetDatePicker DatePicker.Msg
    | EnteredPublicationTime String
    | ClickedSave
    | CompletedSaving (RemoteData (Graphql.Http.Error (Maybe ())) (Maybe ()))
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
    | SchedulePublication DatePicker.DatePicker Date.Date String


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

                WaitingNewsToCopy _ ->
                    let
                        ( form, cmd ) =
                            formFromExistingNews loggedIn.shared.timezone news CreateNew
                    in
                    Editing form
                        |> UR.init
                        |> UR.addCmd cmd

                WaitingNewsToEdit _ ->
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

                WaitingNewsToCopy _ ->
                    UR.init model
                        |> UR.logIncompatibleMsg msg
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            []

                WaitingNewsToEdit _ ->
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
        EnteredTitle title ->
            { form | title = title }
                |> UR.init

        GotDescriptionEditorMsg subMsg ->
            let
                ( descriptionEditor, cmd ) =
                    MarkdownEditor.update subMsg form.descriptionEditor
            in
            { form | descriptionEditor = descriptionEditor }
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
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            []

                SchedulePublication datePicker selectedDate publicationTime ->
                    let
                        ( newDatePicker, dateEvent ) =
                            DatePicker.update datePickerSettings subMsg datePicker

                        newSelectedDate =
                            case dateEvent of
                                DatePicker.Picked newDate ->
                                    newDate

                                _ ->
                                    selectedDate
                    in
                    { form | publicationMode = SchedulePublication newDatePicker newSelectedDate publicationTime }
                        |> UR.init

        EnteredPublicationTime publicationTime ->
            case form.publicationMode of
                PublishImmediately ->
                    form
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried to change communication publication time, but is set to publish immediately"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            []

                SchedulePublication datePicker selectedDate _ ->
                    { form
                        | publicationMode =
                            SchedulePublication datePicker
                                selectedDate
                                publicationTime
                        , timeError = Nothing
                    }
                        |> UR.init

        ClickedSave ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        mutation : Maybe Cambiatus.Scalar.DateTime -> SelectionSet (Maybe ()) RootMutation
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
                                        SelectionSet.empty

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
                                        SelectionSet.empty

                        saveNews : Maybe Cambiatus.Scalar.DateTime -> Cmd FormMsg
                        saveNews scheduling =
                            Api.Graphql.mutation loggedIn.shared
                                (Just loggedIn.authToken)
                                (mutation scheduling)
                                CompletedSaving
                    in
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
                            -- TODO - I18N, better error message
                            { form | timeError = Just "Invalid time" }
                                |> UR.init

                _ ->
                    form
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried saving communication, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "updateForm" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

        CompletedSaving (RemoteData.Success _) ->
            { form | isSaving = False }
                |> UR.init
                -- TODO - I18N
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success "The communication is active")
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.CommunitySettingsNews)

        CompletedSaving (RemoteData.Failure _) ->
            { form | isSaving = False }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Something wrong happened when saving the communication")

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

        SchedulePublication _ selectedDate selectedTime ->
            let
                time =
                    case String.split ":" selectedTime of
                        [ hourString, minuteString ] ->
                            Maybe.map2 (\hour minute -> { hour = hour, minute = minute })
                                (String.toInt hourString)
                                (String.toInt minuteString)

                        _ ->
                            Nothing
            in
            case time of
                Nothing ->
                    InvalidTime

                Just { hour, minute } ->
                    Time.Extra.partsToPosix timezone
                        { year = Date.year selectedDate
                        , month = Date.month selectedDate
                        , day = Date.day selectedDate
                        , hour = hour
                        , minute = minute
                        , second = 0
                        , millisecond = 0
                        }
                        |> Iso8601.fromTime
                        |> Cambiatus.Scalar.DateTime
                        |> ValidTime



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    { title = "News editor"
    , content =
        div [ class "bg-white" ]
            [ Page.viewHeader loggedIn "News editor"
            , case model of
                Editing form ->
                    div [ class "container mx-auto pt-4 pb-10" ]
                        [ viewForm loggedIn form
                            |> Html.map GotFormMsg
                        ]

                NewsNotFound ->
                    -- TODO - I18N
                    Page.fullPageNotFound "Could not find communication"
                        "Try again with a valid communication"

                WithError err ->
                    -- TODO - I18N
                    Page.fullPageGraphQLError "Got an error when fetching communication"
                        err

                WaitingNewsToCopy _ ->
                    Page.fullPageLoading loggedIn.shared

                WaitingNewsToEdit _ ->
                    Page.fullPageLoading loggedIn.shared
            ]
    }


viewForm : LoggedIn.Model -> Form -> Html FormMsg
viewForm ({ shared } as loggedIn) form =
    let
        { translators } =
            shared

        defaultSchedulingDate =
            shared.now
                |> Date.fromPosix shared.timezone
                |> Date.add Date.Days 1

        defaultDatePicker =
            DatePicker.initFromDate defaultSchedulingDate
    in
    Html.form
        [ class "px-4"
        , onSubmit ClickedSave
        ]
        [ Input.init
            { -- TODO - I18N
              label = "Title"
            , id = "title-input"
            , onInput = EnteredTitle
            , disabled = form.isSaving
            , value = form.title
            , placeholder = Just "Lorem ipsum dolor"
            , problems = Nothing
            , translators = translators
            }
            |> Input.toHtml
        , MarkdownEditor.view
            { translators = translators
            , placeholder = Nothing

            -- TODO - I18N
            , label = "Description"
            , problem = Nothing
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
            { -- TODO - I18N
              label = "Publish or schedule"
            , name = "publish-mode-radio"
            , optionToString =
                \option ->
                    case option of
                        PublishImmediately ->
                            "publish-immediately"

                        SchedulePublication _ _ _ ->
                            "schedule-publication"
            , activeOption = form.publicationMode
            , onSelect = SelectedPublicationMode
            , areOptionsEqual =
                \option1 option2 ->
                    case ( option1, option2 ) of
                        ( SchedulePublication _ _ _, SchedulePublication _ _ _ ) ->
                            True

                        ( PublishImmediately, PublishImmediately ) ->
                            True

                        _ ->
                            False
            }
            -- TODO - I18N
            |> Radio.withOption PublishImmediately
                (\_ -> text "Publish immediately")
            -- TODO - I18N
            |> Radio.withOption
                (SchedulePublication defaultDatePicker defaultSchedulingDate "")
                (\_ -> text "Schedule publication")
            |> Radio.withVertical True
            |> Radio.withDisabled form.isSaving
            |> Radio.toHtml translators
        , case form.publicationMode of
            PublishImmediately ->
                text ""

            SchedulePublication datePicker selectedDate publicationTime ->
                div [ class "flex space-x-4" ]
                    [ div [ class "w-full" ]
                        [ -- TODO - I18N
                          View.Form.label [] "datepicker-input" "Initial date"
                        , div [ class "relative" ]
                            [ DatePicker.view (Just selectedDate)
                                datePickerSettings
                                datePicker
                                |> Html.map SetDatePicker
                            , img
                                [ src "/icons/calendar.svg"
                                , tabindex -1
                                , class "absolute right-0 top-0 h-full pointer-events-none"
                                ]
                                []
                            ]
                        ]
                    , Input.init
                        { -- TODO - I18N
                          label = "Time"
                        , id = "time-input"
                        , onInput = EnteredPublicationTime
                        , disabled = form.isSaving
                        , value = publicationTime
                        , placeholder = Nothing
                        , problems = form.timeError |> Maybe.map List.singleton
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
            -- TODO - I18N
            [ text "Save" ]
        ]


viewLatestEditions : LoggedIn.Model -> Community.News.Model -> Profile.Summary.Model -> Html FormMsg
viewLatestEditions ({ shared } as loggedIn) news profileSummary =
    case news.lastEditor of
        Nothing ->
            text ""

        Just lastEditor ->
            div []
                [ p [ class "label mb-6" ] [ text "Latest editions" ]
                , div [ class "flex items-center" ]
                    [ profileSummary
                        |> Profile.Summary.withoutName
                        |> Profile.Summary.withImageSize "h-8 w-8"
                        |> Profile.Summary.view shared
                            loggedIn.accountName
                            lastEditor
                        |> Html.map GotEditorSummaryMsg
                    , p [ class "text-gray-900 ml-2" ]
                        -- TODO - I18N
                        [ text "Edited by "
                        , a
                            [ class "font-bold hover:underline"
                            , Route.href (Route.Profile lastEditor.account)
                            ]
                            [ lastEditor.name
                                |> Maybe.withDefault (Eos.Account.nameToString lastEditor.account)
                                |> text
                            ]
                        , View.Components.dateViewer []
                            (\translations ->
                                { translations
                                    | today = Nothing
                                    , yesterday = Nothing

                                    -- TODO - I18N
                                    , other = " on {{date}}"
                                }
                            )
                            shared
                            news.updatedAt
                        ]
                    ]
                ]


datePickerSettings : DatePicker.Settings
datePickerSettings =
    let
        defaultSettings =
            DatePicker.defaultSettings
    in
    { defaultSettings
        | changeYear = DatePicker.off
        , inputId = Just "datepicker-input"
        , inputClassList = [ ( "input w-full", True ) ]
        , containerClassList = [ ( "relative-table mb-4", True ) ]
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
