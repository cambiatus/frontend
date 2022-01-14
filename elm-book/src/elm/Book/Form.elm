module Book.Form exposing (chapters)

import Book.Form.Checkbox
import Book.Form.DatePicker
import Book.Form.File
import Book.Form.Radio
import Book.Form.RichText
import Book.Form.Select
import Book.Form.Text
import Book.Form.Toggle
import Book.Form.UserPicker
import Book.Form.Validation
import ElmBook.Chapter as Chapter exposing (Chapter)


type alias SharedState x =
    { x
        | toggleModel : Book.Form.Toggle.Model
        , textModel : Book.Form.Text.Model
        , richTextModel : Book.Form.RichText.Model
        , datepickerModel : Book.Form.DatePicker.Model
        , userpickerModel : Book.Form.UserPicker.Model
        , selectModel : Book.Form.Select.Model
        , checkboxModel : Book.Form.Checkbox.Model
        , radioModel : Book.Form.Radio.Model
        , fileModel : Book.Form.File.Model
    }


chapters : List (Chapter (SharedState x))
chapters =
    [ introduction
    , Book.Form.Validation.chapter
    , Book.Form.Text.chapter
    , Book.Form.RichText.chapter
    , Book.Form.UserPicker.chapter
    , Book.Form.DatePicker.chapter
    , Book.Form.Select.chapter
    , Book.Form.Checkbox.chapter
    , Book.Form.Toggle.chapter
    , Book.Form.Radio.chapter
    , Book.Form.File.chapter
    ]


introduction : Chapter x
introduction =
    Chapter.chapter "Introduction"
        |> Chapter.render """
In Cambiatus, we have a very powerful abstraction over forms - they're just a
function that takes in a "dirty" model (made up of user input) and returns a
valid model (by parsing the dirty model, following [Parse don't validate](https://sporto.github.io/elm-patterns/basic/parse-dont-validate.html)).

This abstraction comes primarily from [hecrj/composable-form](https://package.elm-lang.org/packages/hecrj/composable-form/latest/),
but we have our own `Form` module.

Along with the abstraction of how a form should work in a general sense
(receive input, and then parse that input), we also have a bunch of components
that define how the form fields should work in a specific sense, and also how
they should look.

Another powerful feature of our `Form` module and all of it's fields is the use
of [The builder pattern](https://sporto.github.io/elm-patterns/basic/builder-pattern.html),
for forms *and* for form fields (in our abstraction, each form field is considered
as a mini-form itself). This ensures we can have a very composable and flexible
API to define forms. Here's roughly how we could have a login form:

    type alias DirtyCredentials = { email : String, password : String }

    type alias Credentials = { email : Email, password : String }

    loginForm : Bool -> Form DirtyCredentials Credentials
    loginForm areCredentialsIncorrect =
        Form.succeed Credentials
            -- We can have fields defined on their own functions
            |> Form.with emailField
            -- And we can have them inline
            |> Form.with
                (Form.Text.init { label = "Password", id = "password-input" }
                    -- We can add some attributes to the text field
                    |> Form.Text.withType Form.Text.Password
                    |> Form.Text.withElements
                        [ clearInputButton
                        , toggleInputVisibilityButton
                        ]
                    |> Form.textField
                        { parser =
                            \\password ->
                                if String.length password > 3 then
                                    Ok password
                                else
                                    Err "Password must be at least 4 characters long"
                        , value = .password
                        , update = \\password dirtyCredentials -> { dirtyCredentials | password = password }
                        , externalError =
                            \\_ ->
                                if areCredentialsIncorrect then
                                    Just "Your credentials don't match!"
                                else
                                    Nothing
                        }
                )
    
    emailField : Form String Email
    emailField =
        Form.Text.init { label = "Email", id = "email-input" }
            |> Form.textField
                -- Imagine that `Email.fromString` is of type `String -> Result String Email`
                { parser = Email.fromString
                , value = .email
                , update = \\email dirtyCredentials -> { dirtyCredentials | email = email }
                , externalError = always Nothing
                }
            
With these functions, we can build generic forms - notice we didn't give them
any data!

In order to actually render them and get input from them, we need a
`Form.Model`, which has an `init`, `update` and `view` triplet:

    type alias Model =
        { dirtyCredentials : Form.Model DirtyCredentials }

    init : (Model, Cmd Msg)
    init =
        ( { dirtyCredentials = Form.init { email = "", password = "" } }
        , Cmd.none
        )

    type Msg
        = GotLoginFormMsg (Form.Msg DirtyCredentials)
        | AttemptedLogin Credentials
    
    update : Msg -> Model -> LoggedIn.Model -> UpdateResult
    update msg model loggedIn =
        case msg of
            GotLoginFormMsg subMsg ->
                Form.update loggedIn.shared subMsg model.dirtyCredentials
                    |> UR.fromChild
                        (\\dirtyCredentials -> { model | dirtyCredentials = dirtyCredentials })
                        GotLoginFormMsg
                        LoggedIn.executeFeedback
                        model
            
            AttemptedLogin credentials ->
                UR.init model
                    |> UR.addCmd (Api.login credentials)
    
    view : LoggedIn.Model -> Model -> Html Msg
    view loggedIn model =
        Form.view []
            { buttonAttrs = []
            , buttonLabel = [ text "Login" ]
            , translators = loggedIn.shared.translators
            }
            loginForm
            model.dirtyCredentials
            GotLoginFormMsg
            AttemptedLogin

In the following chapters, we will take an in-depth look at all of our
form components. If you're testing these, please make sure they're
keyboard-accessible (you can navigate and use them only with your keyboard) and
also screen-reader-friendly!
"""
