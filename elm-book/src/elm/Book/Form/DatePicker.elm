module Book.Form.DatePicker exposing (Model, Msg, chapter, initModel, updateSharedState)

import Book.Helpers
import Date
import ElmBook.Chapter as Chapter exposing (CustomChapter)
import Form.DatePicker
import Html
import Time



-- MODEL


type alias Model =
    { absolute : Form.DatePicker.Model
    , relative : Form.DatePicker.Model
    , withError : Form.DatePicker.Model
    }


initModel : Model
initModel =
    -- TODO - Make this date dynamic
    { absolute = Form.DatePicker.initModel (Date.fromCalendarDate 2021 Time.Sep 19)
    , relative = Form.DatePicker.initModel (Date.fromCalendarDate 2021 Time.Sep 19)
    , withError = Form.DatePicker.initModel (Date.fromCalendarDate 2021 Time.Sep 19)
    }



-- UPDATE


type Msg
    = NoOp
    | GotRelativeDatePickerMsg (Form.DatePicker.Options Msg) Form.DatePicker.Msg
    | GotAbsoluteDatePickerMsg (Form.DatePicker.Options Msg) Form.DatePicker.Msg
    | GotErrorDatePickerMsg (Form.DatePicker.Options Msg) Form.DatePicker.Msg


type alias SharedState x =
    { x | datepickerModel : Model }


updateSharedState : Msg -> SharedState x -> ( SharedState x, Cmd Msg )
updateSharedState msg sharedState =
    let
        model =
            sharedState.datepickerModel
    in
    case msg of
        NoOp ->
            ( sharedState, Cmd.none )

        GotRelativeDatePickerMsg options subMsg ->
            let
                ( newModel, subCmd ) =
                    Form.DatePicker.update options
                        subMsg
                        sharedState.datepickerModel.relative
            in
            ( { sharedState | datepickerModel = { model | relative = newModel } }
            , Cmd.map (GotRelativeDatePickerMsg options) subCmd
            )

        GotAbsoluteDatePickerMsg options subMsg ->
            let
                ( newModel, subCmd ) =
                    Form.DatePicker.update options
                        subMsg
                        sharedState.datepickerModel.absolute
            in
            ( { sharedState | datepickerModel = { model | absolute = newModel } }
            , Cmd.map (GotAbsoluteDatePickerMsg options) subCmd
            )

        GotErrorDatePickerMsg options subMsg ->
            let
                ( newModel, subCmd ) =
                    Form.DatePicker.update options
                        subMsg
                        sharedState.datepickerModel.withError
            in
            ( { sharedState | datepickerModel = { model | withError = newModel } }
            , Cmd.map (GotErrorDatePickerMsg options) subCmd
            )



-- CHAPTER


chapter : CustomChapter (SharedState x) Msg
chapter =
    Chapter.chapter "Date picker"
        |> Chapter.withStatefulComponentList
            [ ( "Live example without absolute positioning"
              , \sharedState ->
                    Form.DatePicker.init { label = "Pick a date", id = "relative-datepicker" }
                        |> Form.DatePicker.withAbsolutePositioning False
                        |> (\options ->
                                Form.DatePicker.view options
                                    { value = sharedState.datepickerModel.relative
                                    , error = Html.text ""
                                    , hasError = False
                                    , isRequired = True
                                    , translators = Book.Helpers.mockTranslators
                                    }
                                    (GotRelativeDatePickerMsg options)
                           )
              )
            , ( "Live example with absolute positioning"
              , \sharedState ->
                    Form.DatePicker.init { label = "Pick a date", id = "absolute-datepicker" }
                        |> (\options ->
                                Form.DatePicker.view options
                                    { value = sharedState.datepickerModel.absolute
                                    , error = Html.text ""
                                    , hasError = False
                                    , isRequired = True
                                    , translators = Book.Helpers.mockTranslators
                                    }
                                    (GotAbsoluteDatePickerMsg options)
                           )
              )
            , ( "Live example with error and disabled"
              , \sharedState ->
                    Form.DatePicker.init { label = "Pick a date", id = "disabled-datepicker" }
                        |> Form.DatePicker.withDisabled True
                        |> (\options ->
                                Form.DatePicker.view options
                                    { value = sharedState.datepickerModel.withError
                                    , error = Book.Helpers.viewError [] True (Just "Errors are displayed below the input")
                                    , hasError = True
                                    , isRequired = True
                                    , translators = Book.Helpers.mockTranslators
                                    }
                                    (GotAbsoluteDatePickerMsg options)
                           )
              )
            ]
        |> Chapter.render """
Date pickers are useful when we want the user to select a date. We use it in
places like the action creation form and news creation form. We offer a calendar
so the user can pick the date they want, and it looks like this:

<component with-label="Live example without absolute positioning" />

In the above example, the calendar displaces other elements (note how the white
box around it grows when you open it), but we can also make the calendar "float"
above other elements:

<component with-label="Live example with absolute positioning" />

The issue with the floating calendar is that sometimes we don't have quite
enough vertical space to display it floating, and so it creates a scrollbar, and
the user needs to scroll down to view it entirely.

Similar to other inputs, we can also display and error, and disable the
datepicker:

<component with-label="Live example with error and disabled" />
"""
