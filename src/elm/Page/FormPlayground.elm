module Page.FormPlayground exposing (Model, Msg, init, msgToString, update, view)

-- TODO - Remove this before merging PR

import Date exposing (Date)
import Eos
import Form exposing (Form)
import Form.Checkbox
import Form.DatePicker
import Form.File
import Form.Radio
import Form.RichText
import Form.Select
import Form.Text
import Form.Toggle
import Html exposing (Html, div, img, p, strong, text)
import Html.Attributes exposing (autocomplete, class, rows, src)
import Http
import Icons
import Markdown exposing (Markdown)
import Mask
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared
import UpdateResult as UR
import View.Components
import View.Form



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init { shared } =
    ( { userFormModel =
            Form.init
                { name = ""
                , middleName = ""
                , phoneNumber = ""
                , age = ""
                , bio = "This is an example bio!"
                , balance = "0"
                , disabledField = "This field is disabled"
                , agrees = False
                , accountType = Personal
                , avatar = RemoteData.NotAsked
                , resume = RemoteData.NotAsked
                , interest = Programming
                , toggleTest = False
                , richtextTest = Form.RichText.initModel "richtext-input" Nothing
                , dateTest = Form.DatePicker.initModel (Date.fromPosix shared.timezone shared.now)
                }
      , user = Nothing
      }
    , Cmd.none
    )


type alias Model =
    { userFormModel : Form.Model DirtyUser
    , user : Maybe User
    }



-- UPDATE


type Msg
    = GotUserFormMsg (Form.Msg DirtyUser)
    | SubmittedUser User


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        GotUserFormMsg subMsg ->
            Form.update loggedIn.shared subMsg model.userFormModel
                |> UR.fromChild
                    (\userFormModel -> { model | userFormModel = userFormModel })
                    GotUserFormMsg
                    LoggedIn.executeFeedback
                    model

        SubmittedUser user ->
            { model | user = Just user }
                |> UR.init



-- USER FORM


type alias DirtyUser =
    { name : String
    , middleName : String
    , phoneNumber : String
    , age : String
    , bio : String
    , balance : String
    , disabledField : String
    , agrees : Bool
    , accountType : AccountType
    , avatar : RemoteData Http.Error String
    , resume : RemoteData Http.Error String
    , interest : Interest
    , toggleTest : Bool
    , richtextTest : Form.RichText.Model
    , dateTest : Form.DatePicker.Model
    }


type alias User =
    { name : String
    , middleName : Maybe String
    , phoneNumber : String
    , age : Int
    , bio : String
    , balance : Eos.Asset
    , disabledField : String
    , agrees : Bool
    , accountType : AccountType
    , avatarUrl : String
    , resumeUrl : Maybe String
    , interest : Interest
    , toggleTest : Bool
    , richtextTest : Markdown
    , dateTest : Date
    }


type AccountType
    = Juridical
    | Personal


type Interest
    = Programming
    | Sports
    | Gaming


userForm : Shared.Translators -> Form DirtyUser User
userForm translators =
    let
        phoneMask =
            { mask = "## ##### ####", replace = '#' }

        communitySymbol =
            Eos.symbolFromString "0,BUSS"
                |> Maybe.withDefault Eos.cambiatusSymbol
    in
    Form.succeed User
        |> Form.with
            (Form.Text.init { label = "Name", id = "name-input" }
                |> Form.Text.withContainerAttrs [ class "bg-white p-2 rounded" ]
                |> Form.Text.withCounter (Form.Text.CountWords 1)
                |> Form.textField
                    { parser = Ok
                    , value = .name
                    , update = \name user -> { user | name = name }
                    , externalError = always Nothing
                    }
            )
        |> Form.withOptional
            (Form.Text.init { label = "Middle name", id = "middle-name-input" }
                |> Form.Text.withInputContainerAttrs [ class "bg-white p-2 rounded" ]
                |> Form.textField
                    { parser =
                        \middleName ->
                            if String.length middleName > 3 then
                                Ok middleName

                            else
                                Err "Middle name must be > 3 characters"
                    , value = .middleName
                    , update = \middleName user -> { user | middleName = middleName }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = "Phone", id = "phone-input" }
                |> Form.Text.withLabelAttrs [ class "bg-white p-2 rounded" ]
                |> Form.Text.withPlaceholder "## ##### ####"
                |> Form.Text.withMask phoneMask
                |> Form.Text.withType Form.Text.Telephone
                |> Form.Text.withElements
                    [ Icons.phone "absolute right-4 top-1/2 transform -translate-y-1/2"
                    ]
                |> Form.textField
                    { parser = Mask.remove phoneMask >> Ok
                    , value = .phoneNumber
                    , update = \phone user -> { user | phoneNumber = phone }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = "Age", id = "age-input" }
                |> Form.Text.asNumeric
                |> Form.Text.withType Form.Text.Number
                |> Form.Text.withErrorAttrs [ class "bg-white p-2 rounded" ]
                |> Form.textField
                    { parser = String.toInt >> Result.fromMaybe "Age must be an int"
                    , value = .age
                    , update = \age user -> { user | age = age }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = "Bio", id = "bio-input" }
                |> Form.Text.withInputElement Form.Text.TextareaInput
                |> Form.Text.withExtraAttrs
                    [ class "p-4"
                    , rows 2
                    , View.Form.noGrammarly
                    , autocomplete False
                    ]
                |> Form.Text.withCounter (Form.Text.CountLetters 255)
                |> Form.Text.withCounterAttrs [ class "bg-white p-2 rounded" ]
                |> Form.textField
                    { parser =
                        \bio ->
                            if String.length bio >= 10 then
                                Ok bio

                            else
                                Err "Bio must be >= 10 characters long"
                    , value = .bio
                    , update = \bio user -> { user | bio = bio }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = "Balance", id = "balance-input" }
                |> Form.Text.withCurrency communitySymbol
                |> Form.textField
                    { parser =
                        \stringBalance ->
                            case
                                Shared.floatStringFromSeparatedString translators stringBalance
                                    |> String.toFloat
                            of
                                Just balance ->
                                    Ok { symbol = communitySymbol, amount = balance }

                                Nothing ->
                                    Err "Balance must be a float"
                    , value = .balance
                    , update = \balance user -> { user | balance = balance }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = "Disabled field", id = "disabled-field" }
                |> Form.Text.withDisabled True
                |> Form.textField
                    { parser = Ok
                    , value = .disabledField
                    , update = \disabled user -> { user | disabledField = disabled }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Checkbox.init
                { label = text "I have read something and I agree with it"
                , id = "agree-checkbox"
                }
                |> Form.Checkbox.withContainerAttrs [ class "mb-10" ]
                |> Form.Checkbox.withDisabled False
                |> Form.checkbox
                    { parser =
                        \agrees ->
                            if agrees then
                                Ok agrees

                            else
                                Err "You need to agree to this. What is it? I don't know, just agree with it and move on"
                    , value = .agrees
                    , update = \agrees user -> { user | agrees = agrees }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Radio.init
                { label = "Account type"
                , id = "account-type-radio"
                , optionToString = accountTypeToString
                }
                |> Form.Radio.withOption Personal (text "Personal")
                |> Form.Radio.withOptions [ ( Juridical, text "Juridical" ) ]
                |> Form.Radio.withDisabled False
                |> Form.Radio.withContainerAttrs [ class "bg-white p-4" ]
                |> Form.Radio.withGroupAttrs [ class "border p-4 rounded-sm" ]
                |> Form.Radio.withDirection Form.Radio.Vertical
                |> Form.radio
                    (accountTypeFromString >> Maybe.withDefault Personal)
                    { parser = Ok
                    , value = .accountType
                    , update = \accountType user -> { user | accountType = accountType }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.File.init { label = "Avatar", id = "avatar-input" }
                |> Form.File.withVariant Form.File.SmallCircle
                |> Form.File.withContainerAttrs [ class "my-10" ]
                |> Form.file
                    { parser = identity
                    , failureErrorMessage = \_ -> "Something went wrong when uploading avatar"
                    , loadingErrorMessage = "Please wait for the avatar to be finished uploading"
                    , notAskedErrorMessage = "Avatar is required"
                    , value = .avatar
                    , update = \avatar user -> { user | avatar = avatar }
                    , externalError = always Nothing
                    }
            )
        |> Form.withOptional
            (Form.File.init { label = "Resume", id = "resume-input" }
                |> Form.File.withVariant (Form.File.LargeRectangle Form.File.Gray)
                |> Form.File.withFileTypes [ Form.File.PDF ]
                |> Form.File.withContainerAttrs [ class "my-10" ]
                |> Form.File.withDisabled True
                |> Form.file
                    { parser = identity
                    , failureErrorMessage = \_ -> "Something went wrong when uploading resume"
                    , loadingErrorMessage = "Please wait for your resume to be finished uploading"
                    , notAskedErrorMessage = ""
                    , value = .resume
                    , update = \resume user -> { user | resume = resume }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Select.init
                { label = "Interest"
                , id = "interest-select"
                , optionToString = interestToString
                }
                |> Form.Select.withOption Programming "Programming"
                |> Form.Select.withOption Sports "Sports"
                |> Form.Select.withOption Gaming "Gaming"
                |> Form.Select.withContainerAttrs [ class "mb-10" ]
                |> Form.Select.withAttrs [ class "w-1/3" ]
                |> Form.Select.withDisabled True
                |> Form.select
                    (interestFromString >> Maybe.withDefault Programming)
                    { parser = Ok
                    , value = .interest
                    , update = \interest user -> { user | interest = interest }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Toggle.init
                { label = text "Toggle test"
                , id = "toggle-test"
                }
                |> Form.Toggle.withTooltip { message = "Test tooltip Test tooltip Test tooltip", iconClass = "" }
                |> Form.Toggle.withDisabled False
                |> Form.toggle
                    { parser = Ok
                    , value = .toggleTest
                    , update = \toggle user -> { user | toggleTest = toggle }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.RichText.init { label = "Richtext test" }
                |> Form.RichText.withDisabled False
                |> Form.RichText.withPlaceholder "Some placeholder"
                |> Form.RichText.withContainerAttrs [ class "p-4 bg-white" ]
                |> Form.richText
                    { parser = Ok
                    , value = .richtextTest
                    , update = \richtext user -> { user | richtextTest = richtext }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.DatePicker.init { label = "Date test", id = "datepicker-test" }
                |> Form.DatePicker.withDisabled False
                |> Form.DatePicker.withAbsolutePositioning False
                |> Form.datePicker
                    { parser = Result.fromMaybe "Date is mandatory"
                    , value = .dateTest
                    , update = \dateTest user -> { user | dateTest = dateTest }
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
            , case model.user of
                Nothing ->
                    text ""

                Just user ->
                    let
                        viewProperty property =
                            p [] [ strong [] [ text property ] ]
                    in
                    div [ class "bg-white rounded p-8 grid grid-cols-2 mt-8" ]
                        [ viewProperty "Name"
                        , p [] [ text user.name ]
                        , viewProperty "Middle name"
                        , p []
                            [ case user.middleName of
                                Nothing ->
                                    text "No middle name provided"

                                Just middleName ->
                                    text middleName
                            ]
                        , viewProperty "Phone number"
                        , p [] [ text user.phoneNumber ]
                        , viewProperty "Age"
                        , p [] [ text <| String.fromInt user.age ]
                        , viewProperty "Bio"
                        , p [] [ text user.bio ]
                        , viewProperty "Balance"
                        , p [] [ text <| Eos.assetToString loggedIn.shared.translators user.balance ]
                        , viewProperty "Disabled field"
                        , p [] [ text user.disabledField ]
                        , viewProperty "Agrees"
                        , p []
                            [ text
                                (if user.agrees then
                                    "Yes"

                                 else
                                    "No"
                                )
                            ]
                        , viewProperty "Account type"
                        , p [] [ text (accountTypeToString user.accountType) ]
                        , viewProperty "Avatar"
                        , img [ src user.avatarUrl ] []
                        , viewProperty "Resume"
                        , case user.resumeUrl of
                            Nothing ->
                                text "No resume provided"

                            Just resumeUrl ->
                                View.Components.pdfViewer []
                                    { url = resumeUrl
                                    , childClass = ""
                                    , maybeTranslators = Nothing
                                    }
                        , viewProperty "Interest"
                        , p [] [ text (interestToString user.interest) ]
                        , viewProperty "Toggle test"
                        , p []
                            [ text
                                (if user.toggleTest then
                                    "Enabled"

                                 else
                                    "Disabled"
                                )
                            ]
                        , viewProperty "Rich text test"
                        , Markdown.view [] user.richtextTest
                        ]
            ]
    }


viewUserForm : LoggedIn.Model -> Model -> Html Msg
viewUserForm loggedIn model =
    Form.view []
        loggedIn.shared.translators
        (\submitButton ->
            [ submitButton [ class "mx-auto" ] [ text "Submit user" ] ]
        )
        (userForm loggedIn.shared.translators)
        model.userFormModel
        { toMsg = GotUserFormMsg, onSubmit = SubmittedUser }



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        GotUserFormMsg subMsg ->
            "GotUserFormMsg" :: Form.msgToString subMsg

        SubmittedUser _ ->
            [ "SubmittedUser" ]


accountTypeToString : AccountType -> String
accountTypeToString accountType =
    case accountType of
        Personal ->
            "personal"

        Juridical ->
            "juridical"


accountTypeFromString : String -> Maybe AccountType
accountTypeFromString accountType =
    case accountType of
        "juridical" ->
            Just Juridical

        "personal" ->
            Just Personal

        _ ->
            Nothing


interestToString : Interest -> String
interestToString interest =
    case interest of
        Programming ->
            "programming"

        Sports ->
            "sports"

        Gaming ->
            "gaming"


interestFromString : String -> Maybe Interest
interestFromString interest =
    case interest of
        "programming" ->
            Just Programming

        "sports" ->
            Just Sports

        "gaming" ->
            Just Gaming

        _ ->
            Nothing
