module Book.Form.UserPicker exposing (Model, Msg, chapter, initModel)

import Book.Helpers
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Eos.Account
import Form.UserPicker
import Html
import Task



-- MODEL


type alias Model =
    { single : Form.UserPicker.Model
    }


initModel : Model
initModel =
    { single = Form.UserPicker.initSingle { id = "single-picker" } |> Form.UserPicker.fromSinglePicker
    }



-- UPDATE


type Msg
    = NoOp
    | GotPickerMsg (Form.UserPicker.Options Msg) (Form.UserPicker.ViewConfig Msg) Form.UserPicker.Msg


type alias SharedState x =
    { x | userpickerModel : Model }


update : Msg -> Form.UserPicker.Model -> ( Form.UserPicker.Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPickerMsg options viewConfig subMsg ->
            let
                ( newModel, cmd, maybeNewMsg ) =
                    Form.UserPicker.update options viewConfig subMsg model
            in
            ( newModel
            , Cmd.batch
                [ Cmd.map (GotPickerMsg options viewConfig) cmd
                , case maybeNewMsg of
                    Nothing ->
                        Cmd.none

                    Just newMsg ->
                        Task.succeed ()
                            |> Task.perform (always newMsg)
                ]
            )



-- CHAPTER


chapter : Chapter (SharedState x)
chapter =
    Chapter.chapter "User picker"
        |> Chapter.withStatefulComponentList
            [ ( "Single user picker"
              , \sharedState ->
                    let
                        viewConfig =
                            { onBlur = NoOp
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
                                    (GotPickerMsg options viewConfig)
                                    |> Html.map
                                        (Actions.mapUpdateWithCmd
                                            { fromState = .userpickerModel >> .single
                                            , toState =
                                                \shared model ->
                                                    let
                                                        userpicker =
                                                            shared.userpickerModel
                                                    in
                                                    { shared | userpickerModel = { userpicker | single = model } }
                                            , update = update
                                            }
                                        )
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

Select a single user:

<component with-label="Single user picker" />
"""
