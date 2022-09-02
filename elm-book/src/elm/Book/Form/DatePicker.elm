module Book.Form.DatePicker exposing (Model, chapter, initModel)

import Book.Helpers
import Date
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
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
    { absolute = Form.DatePicker.initModel (Date.fromCalendarDate 2021 Time.Sep 19)
    , relative = Form.DatePicker.initModel (Date.fromCalendarDate 2021 Time.Sep 19)
    , withError = Form.DatePicker.initModel (Date.fromCalendarDate 2021 Time.Sep 19)
    }



-- UPDATE


type alias SharedState x =
    { x | datepickerModel : Model }



-- CHAPTER


chapter : Chapter (SharedState x)
chapter =
    Chapter.chapter "Date picker"
        |> Chapter.withStatefulComponentList
            [ ( "Live example without absolute positioning"
              , \sharedState ->
                    Form.DatePicker.init { label = "Pick a date", id = "relative-datepicker" }
                        |> Form.DatePicker.withAbsolutePositioning False
                        |> (\options ->
                                let
                                    viewConfig =
                                        { value = sharedState.datepickerModel.relative
                                        , error = Html.text ""
                                        , hasError = False
                                        , isRequired = True
                                        , translators = Book.Helpers.mockTranslators
                                        }
                                in
                                Form.DatePicker.view options
                                    viewConfig
                                    identity
                                    |> Html.map
                                        (Actions.mapUpdateWithCmd
                                            { fromState = .datepickerModel >> .relative
                                            , toState =
                                                \shared model ->
                                                    let
                                                        prevDatePicker =
                                                            shared.datepickerModel
                                                    in
                                                    { shared | datepickerModel = { prevDatePicker | relative = model } }
                                            , update =
                                                Form.DatePicker.update options
                                                    viewConfig
                                            }
                                        )
                           )
              )
            , ( "Live example with absolute positioning"
              , \sharedState ->
                    Form.DatePicker.init { label = "Pick a date", id = "absolute-datepicker" }
                        |> (\options ->
                                let
                                    viewConfig =
                                        { value = sharedState.datepickerModel.absolute
                                        , error = Html.text ""
                                        , hasError = False
                                        , isRequired = True
                                        , translators = Book.Helpers.mockTranslators
                                        }
                                in
                                Form.DatePicker.view options
                                    viewConfig
                                    identity
                                    |> Html.map
                                        (Actions.mapUpdateWithCmd
                                            { fromState = .datepickerModel >> .absolute
                                            , toState =
                                                \shared model ->
                                                    let
                                                        prevDatePicker =
                                                            shared.datepickerModel
                                                    in
                                                    { shared | datepickerModel = { prevDatePicker | absolute = model } }
                                            , update =
                                                Form.DatePicker.update options
                                                    viewConfig
                                            }
                                        )
                           )
              )
            , ( "Live example with error and disabled"
              , \sharedState ->
                    Form.DatePicker.init { label = "Pick a date", id = "disabled-datepicker" }
                        |> Form.DatePicker.withDisabled True
                        |> (\options ->
                                let
                                    viewConfig =
                                        { value = sharedState.datepickerModel.withError
                                        , error = Book.Helpers.viewError [] True (Just "Errors are displayed below the input")
                                        , hasError = True
                                        , isRequired = True
                                        , translators = Book.Helpers.mockTranslators
                                        }
                                in
                                Form.DatePicker.view options
                                    viewConfig
                                    identity
                                    |> Html.map
                                        (Actions.mapUpdateWithCmd
                                            { fromState = .datepickerModel >> .withError
                                            , toState =
                                                \shared model ->
                                                    let
                                                        prevDatePicker =
                                                            shared.datepickerModel
                                                    in
                                                    { shared | datepickerModel = { prevDatePicker | withError = model } }
                                            , update =
                                                Form.DatePicker.update options
                                                    viewConfig
                                            }
                                        )
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
