module Page.Community.Settings.News.Editor exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Cambiatus.Mutation
import Cambiatus.Scalar
import Date
import DatePicker
import Eos
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet
import Html exposing (Html, button, div, form, img, text)
import Html.Attributes exposing (class, disabled, src, tabindex, type_)
import Html.Events exposing (onSubmit)
import Iso8601
import Page
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Time
import Time.Extra
import UpdateResult as UR
import View.Form
import View.Form.Input as Input
import View.Form.Radio as Radio
import View.MarkdownEditor as MarkdownEditor



-- MODEL


type alias Model =
    { title : String
    , descriptionEditor : MarkdownEditor.Model
    , publicationMode : PublicationMode
    , isSaving : Bool
    , timeError : Maybe String
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( { title = ""
      , descriptionEditor = MarkdownEditor.init "description-editor"
      , publicationMode = PublishImmediately
      , isSaving = False
      , timeError = Nothing
      }
    , Cmd.none
    )



-- TYPES


type Msg
    = NoOp
    | EnteredTitle String
    | GotDescriptionEditorMsg MarkdownEditor.Msg
    | SelectedPublicationMode PublicationMode
    | SetDatePicker DatePicker.Msg
    | EnteredPublicationTime String
    | ClickedSave
    | CompletedSaving (RemoteData (Graphql.Http.Error (Maybe ())) (Maybe ()))


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


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
        NoOp ->
            UR.init model

        EnteredTitle title ->
            { model | title = title }
                |> UR.init

        GotDescriptionEditorMsg subMsg ->
            let
                ( descriptionEditor, cmd ) =
                    MarkdownEditor.update subMsg model.descriptionEditor
            in
            { model | descriptionEditor = descriptionEditor }
                |> UR.init
                |> UR.addCmd (Cmd.map GotDescriptionEditorMsg cmd)

        SelectedPublicationMode publicationMode ->
            { model | publicationMode = publicationMode }
                |> UR.init

        SetDatePicker subMsg ->
            case model.publicationMode of
                PublishImmediately ->
                    model
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
                    { model | publicationMode = SchedulePublication newDatePicker newSelectedDate publicationTime }
                        |> UR.init

        EnteredPublicationTime publicationTime ->
            case model.publicationMode of
                PublishImmediately ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried to change communication publication time, but is set to publish immediately"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News.Editor", function = "update" }
                            []

                SchedulePublication datePicker selectedDate _ ->
                    { model
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
                        createNews : Maybe Cambiatus.Scalar.DateTime -> Cmd Msg
                        createNews scheduling =
                            Api.Graphql.mutation loggedIn.shared
                                (Just loggedIn.authToken)
                                (Cambiatus.Mutation.news
                                    (\optionals -> { optionals | scheduling = OptionalArgument.fromMaybe scheduling })
                                    { communityId = Eos.symbolToString community.symbol
                                    , description = String.trim model.descriptionEditor.contents
                                    , title = model.title
                                    }
                                    SelectionSet.empty
                                )
                                CompletedSaving
                    in
                    case parseDateTime loggedIn.shared.timezone model.publicationMode of
                        NoTimeToParse ->
                            { model | isSaving = True }
                                |> UR.init
                                |> UR.addCmd (createNews Nothing)

                        ValidTime time ->
                            { model | isSaving = True }
                                |> UR.init
                                |> UR.addCmd (createNews (Just time))

                        InvalidTime ->
                            -- TODO - I18N, better error message
                            { model | timeError = Just "Invalid time" }
                                |> UR.init

                _ ->
                    model
                        |> UR.init

        CompletedSaving _ ->
            { model | isSaving = False }
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
            , div [ class "container mx-auto pt-4 pb-10" ]
                [ viewForm loggedIn.shared model
                ]
            ]
    }


viewForm : Shared -> Model -> Html Msg
viewForm ({ translators } as shared) model =
    let
        defaultSchedulingDate =
            shared.now
                |> Date.fromPosix shared.timezone
                |> Date.add Date.Days 1

        defaultDatePicker =
            DatePicker.initFromDate defaultSchedulingDate
    in
    -- TODO - Check spacings
    form
        [ class "px-4"
        , onSubmit ClickedSave
        ]
        [ Input.init
            { -- TODO - I18N
              label = "Title"
            , id = "title-input"
            , onInput = EnteredTitle
            , disabled = model.isSaving
            , value = model.title
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
            , disabled = model.isSaving
            }
            []
            model.descriptionEditor
            |> Html.map GotDescriptionEditorMsg
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
            , activeOption = model.publicationMode
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
            |> Radio.withDisabled model.isSaving
            |> Radio.toHtml translators
        , case model.publicationMode of
            PublishImmediately ->
                text ""

            SchedulePublication datePicker selectedDate publicationTime ->
                div [ class "flex space-x-4" ]
                    [ div [ class "w-full" ]
                        [ -- TODO - I18N
                          View.Form.label "datepicker-input" "Initial date"
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
                        , disabled = model.isSaving
                        , value = publicationTime
                        , placeholder = Nothing
                        , problems = model.timeError |> Maybe.map List.singleton
                        , translators = translators
                        }
                        |> Input.withContainerAttrs [ class "w-full" ]
                        |> Input.withType Input.Time
                        |> Input.toHtml
                    ]
        , button
            [ type_ "submit"
            , disabled model.isSaving
            , class "button button-primary w-full"
            ]
            -- TODO - I18N
            [ text "Save" ]
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


msgToString : Msg -> List String
msgToString msg =
    case msg of
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
