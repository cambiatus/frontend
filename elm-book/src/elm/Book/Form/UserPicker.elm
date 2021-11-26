module Book.Form.UserPicker exposing (Model, Msg, chapter, initModel, updateSharedState)

import Book.Helpers
import ElmBook.Chapter as Chapter exposing (CustomChapter)
import Eos.Account
import Form.UserPicker
import Html



-- MODEL


type alias Model =
    { single : Form.UserPicker.Model
    , multiple : Form.UserPicker.Model
    , disabledWithError : Form.UserPicker.Model
    }


initModel : Model
initModel =
    { single = Form.UserPicker.initSingle { id = "single-picker" } |> Form.UserPicker.fromSinglePicker
    , multiple = Form.UserPicker.initMultiple { id = "multiple-picker" } |> Form.UserPicker.fromMultiplePicker
    , disabledWithError = Form.UserPicker.initMultiple { id = "disabled-picker" } |> Form.UserPicker.fromMultiplePicker
    }



-- UPDATE


type Msg
    = NoOp
    | GotSinglePickerMsg (Form.UserPicker.Options Msg) (Form.UserPicker.ViewConfig Msg) Form.UserPicker.Msg
    | GotMultiplePickerMsg (Form.UserPicker.Options Msg) (Form.UserPicker.ViewConfig Msg) Form.UserPicker.Msg
    | GotDisabledWithErrorPickerMsg (Form.UserPicker.Options Msg) (Form.UserPicker.ViewConfig Msg) Form.UserPicker.Msg


type alias SharedState x =
    { x | userpickerModel : Model }


updateSharedState : Msg -> SharedState x -> ( SharedState x, Cmd Msg )
updateSharedState msg sharedState =
    let
        model =
            sharedState.userpickerModel
    in
    case msg of
        NoOp ->
            ( sharedState, Cmd.none )

        GotSinglePickerMsg options viewConfig subMsg ->
            let
                ( newModel, subCmd, _ ) =
                    Form.UserPicker.update options
                        viewConfig
                        subMsg
                        model.single
            in
            ( { sharedState | userpickerModel = { model | single = newModel } }
            , Cmd.map (GotSinglePickerMsg options viewConfig) subCmd
            )

        GotMultiplePickerMsg options viewConfig subMsg ->
            let
                ( newModel, subCmd, _ ) =
                    Form.UserPicker.update options
                        viewConfig
                        subMsg
                        model.multiple
            in
            ( { sharedState | userpickerModel = { model | multiple = newModel } }
            , Cmd.map (GotMultiplePickerMsg options viewConfig) subCmd
            )

        GotDisabledWithErrorPickerMsg options viewConfig subMsg ->
            let
                ( newModel, subCmd, _ ) =
                    Form.UserPicker.update options
                        viewConfig
                        subMsg
                        model.disabledWithError
            in
            ( { sharedState | userpickerModel = { model | disabledWithError = newModel } }
            , Cmd.map (GotMultiplePickerMsg options viewConfig) subCmd
            )



-- CHAPTER


chapter : CustomChapter (SharedState x) Msg
chapter =
    Chapter.chapter "User picker"
        |> Chapter.withStatefulComponentList
            [ ( "Single user picker"
              , \sharedState ->
                    let
                        viewConfig =
                            { onBlur = \_ -> NoOp
                            , value = sharedState.userpickerModel.single
                            , error = Html.text ""
                            , hasError = False
                            , translators = Book.Helpers.mockTranslators
                            }
                    in
                    Form.UserPicker.init
                        { label = "Pick a user"
                        , currentUser = Eos.Account.stringToName ""
                        , profiles = Book.Helpers.mockUsers
                        }
                        |> (\options ->
                                Form.UserPicker.view options
                                    viewConfig
                                    (GotSinglePickerMsg options viewConfig)
                           )
              )
            , ( "Multiple user picker"
              , \sharedState ->
                    let
                        viewConfig =
                            { onBlur = \_ -> NoOp
                            , value = sharedState.userpickerModel.multiple
                            , error = Html.text ""
                            , hasError = False
                            , translators = Book.Helpers.mockTranslators
                            }
                    in
                    Form.UserPicker.init
                        { label = "Pick a few users"
                        , currentUser = Eos.Account.stringToName ""
                        , profiles = Book.Helpers.mockUsers
                        }
                        |> (\options ->
                                Form.UserPicker.view options
                                    viewConfig
                                    (GotMultiplePickerMsg options viewConfig)
                           )
              )
            , ( "With error and disabled"
              , \sharedState ->
                    let
                        viewConfig =
                            { onBlur = \_ -> NoOp
                            , value = sharedState.userpickerModel.disabledWithError
                            , error = Book.Helpers.viewError [] True (Just "Errors show up between the input and the selected users")
                            , hasError = True
                            , translators = Book.Helpers.mockTranslators
                            }
                    in
                    Form.UserPicker.init
                        { label = "Pick a few users"
                        , currentUser = Eos.Account.stringToName ""
                        , profiles = Book.Helpers.mockUsers
                        }
                        |> Form.UserPicker.withDisabled True
                        |> (\options ->
                                Form.UserPicker.view options
                                    viewConfig
                                    (GotDisabledWithErrorPickerMsg options viewConfig)
                           )
              )
            ]
        |> Chapter.render """
Sometimes we need to select users, such as when transferring some tokens to
someone else. This component helps by showing a full list of users, searchable
by name. Once you start searching, you can navigate with the arrow keys and
select with `Enter`. We don't use our app's real data here. Instead, we use some
mock users. You can search for `henriquebuss`, `henriquebus4` or `lucca`. The
search works by account name (the 12-letter name) and by the user's real name.

For different purposes, you might want to select a single user:

<component with-label="Single user picker" />

Or maybe you need to select multiple users at a time:

<component with-label="Multiple user picker" />

In either case, you might want to display an error, or maybe disable it. Here's
an example with those options:

<component with-label="With error and disabled" />
"""
