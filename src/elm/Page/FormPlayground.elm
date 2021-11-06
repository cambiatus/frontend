module Page.FormPlayground exposing (Model, Msg, init, msgToString, update, view)

-- TODO - Remove this before merging PR

import Form exposing (Form)
import Form.Text
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Session.LoggedIn as LoggedIn
import UpdateResult as UR



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( { userFormModel = Form.initViewModel { name = "", age = "" }
      }
    , Cmd.none
    )


type alias Model =
    { userFormModel : Form.ViewModel DirtyUser
    }



-- UPDATE


type Msg
    = GotUserFormMsg (Form.Msg DirtyUser User)
    | SubmittedUser User


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        GotUserFormMsg subMsg ->
            Form.update subMsg model.userFormModel
                |> UR.fromChild
                    (\userFormModel -> { model | userFormModel = userFormModel })
                    GotUserFormMsg
                    SubmittedUser
                    model

        SubmittedUser _ ->
            UR.init model



-- USER FORM


type alias DirtyUser =
    { name : String, age : String }


type alias User =
    { name : String, age : Int }


userForm : Form DirtyUser User
userForm =
    Form.succeed User
        |> Form.with
            (Form.Text.init
                { label = "Name"
                , id = "name-input"
                , disabled = False
                }
                |> Form.textField
                    { parser = Ok
                    , value = .name
                    , update = \name user -> { user | name = name }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                { label = "Age"
                , id = "age-input"
                , disabled = False
                }
                |> Form.Text.asNumeric
                |> Form.Text.withType Form.Text.Number
                |> Form.textField
                    { parser = String.toInt >> Result.fromMaybe "Age must be an int"
                    , value = .age
                    , update = \age user -> { user | age = age }
                    , externalError = always Nothing
                    }
            )



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    { title = "Form playground"
    , content =
        div [ class "container mx-auto px-4 my-10" ]
            [ viewUserForm loggedIn model
            ]
    }


viewUserForm : LoggedIn.Model -> Model -> Html Msg
viewUserForm loggedIn model =
    Form.view []
        { buttonAttrs = []
        , buttonLabel = [ text "Submit user" ]
        , translators = loggedIn.shared.translators
        }
        userForm
        model.userFormModel
        |> Html.map GotUserFormMsg



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    []
