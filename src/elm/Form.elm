module Form exposing
    ( Form
    , succeed, fail, with, withNoOutput, withDecoration, withNesting, withGroup
    , optional, introspect, mapValues, mapOutput
    , textField, richText, toggle, checkbox, radio, select, file, datePicker, userPicker, userPickerMultiple, arbitrary
    , view, viewWithoutSubmit, Model, init, Msg, update, updateValues, msgToString
    , withDisabled
    , parse
    )

{-| This is how we deal with forms. The main idea behind a form is to take user
input (a "dirty" model) and parse it into useful output (a "clean" model). Each
individual field is a form on itself, and determines how to go from the dirty
model to the clean model. This module is heavily inspired by
`hecrj/composable-form`, so it might be worth looking at that package's
documentation if you're stuck.

    type alias DirtyUser =
        { name : String, age : String }

    type alias User =
        { name : String, age : Int }

    nameField : Form DirtyUser String msg
    nameField =
        Form.Text.init { label = "Name", id = "name-input" }
            |> Form.Text.withPlaceholder "Name"
            |> Form.textField
                { parser = \dirtyName -> Ok dirtyName
                , value = \dirtyUser -> dirtyUser.name
                , update = \name dirtyUser -> { dirtyUser | name = name }
                , error = \dirtyUser -> Nothing
                }

    ageField : Form DirtyUser Int msg
    ageField =
        Form.Text.init { label = "Age", id = "age-input" }
            |> Form.textField
                { parser =
                    \dirtyAge ->
                        case String.toInt dirtyAge of
                            Just numericAge ->
                                Ok numericAge

                            Nothing ->
                                Err "Age must be a number"
                , value = .age
                , update = \age dirtyUser -> { dirtyUser | age = age }
                , error =
                    \dirtyUser ->
                        case String.toInt dirtyUser.age of
                            Just _ ->
                                Nothing

                            Nothing ->
                                Just "Age must be a number"
                }

    userForm : Form DirtyUser User msg
    userForm =
        Form.succeed User
            |> Form.with nameField
            |> Form.with ageField

@docs Form


## Composing

@docs succeed, fail, with, withNoOutput, withDecoration, withNesting, withGroup


## Modifiers

@docs optional, introspect, mapValues, mapOutput


## Fields

@docs textField, richText, toggle, checkbox, radio, select, file, datePicker, userPicker, userPickerMultiple, arbitrary


## Viewing

@docs view, viewWithoutSubmit, Model, init, Msg, update, updateValues, msgToString


### Changing attributes and state

@docs withDisabled


## Validating

@docs parse

-}

import Browser.Dom
import Date exposing (Date)
import Form.Checkbox as Checkbox
import Form.DatePicker as DatePicker
import Form.File
import Form.Radio as Radio
import Form.RichText as RichText
import Form.Select as Select
import Form.Text as Text
import Form.Toggle as Toggle
import Form.UserPicker as UserPicker
import Html exposing (Html, button)
import Html.Attributes exposing (class, novalidate, type_)
import Html.Events as Events
import Markdown exposing (Markdown)
import Maybe.Extra
import OptionalResult exposing (OptionalResult(..))
import Profile
import Session.Shared as Shared exposing (Shared)
import Set exposing (Set)
import Task
import UpdateResult as UR
import View.Feedback as Feedback



-- TYPES


{-| The main type of this module. A `Form` is a function that takes an input
(the dirty model) and returns a bunch of fields and their result. You shouldn't
have to manipulate this type much, and it will be used mostly just to add type
annotations
-}
type Form values output
    = Form (values -> FilledForm values output)


{-| A `FilledForm` represents a form with some (dirty) values in it. It also
tries to parse the form, and hols the result as either an error or the
valid/clean model
-}
type alias FilledForm values output =
    { fields : List (FilledField values)
    , result : OptionalResult ( String, List String ) output
    }


{-| We need a way to hold a single field's information, and whether or not it
has an error! This is what we use to do that
-}
type alias FilledField values =
    { state : Field values
    , error : Maybe String
    , isRequired : Bool
    }


{-| Every field must have at least a value and a way to update the dirty model
with this value. This is used to construct a `Field` given some values.

The `updateWithValues` function should only be used by inputs that are async.
For example, a file selector needs to upload the file to the server, and update
the model when it's done. In order to not override other changes the user might
have done on the form, we use the model at the time the file was uploaded to
update the model

-}
type alias BaseField value values =
    { value : value
    , getValue : values -> value
    , update : value -> values
    , updateWithValues : value -> values -> values
    }


{-| This is how we can tell the most important stuff from a field:

  - How to parse it from a dirty value to a clean one
  - How to get the field's value out of the dirty model
  - How to update the dirty model with a new value of this field
  - How to know if there are any errors on this field, based on the dirty model

-}
type alias FieldConfig input output values =
    { parser : input -> Result String output
    , value : values -> input
    , update : input -> values -> values
    , externalError : values -> Maybe String
    }



-- FIELDS


{-| All the possible field types, such as `Text`, `RichText` and `Radio`. Use
the corresponding function (i.e. `textField` for `Text`, etc) to build fields
with these types
-}
type Field values
    = Text (Text.Options (Msg values)) (BaseField String values)
    | RichText (RichText.Options (Msg values)) (BaseField RichText.Model values)
    | Toggle (Toggle.Options (Msg values)) (BaseField Bool values)
    | Checkbox (Checkbox.Options (Msg values)) (BaseField Bool values)
    | Radio (Radio.Options String (Msg values)) (BaseField String values)
    | File (Form.File.Options (Msg values)) (BaseField Form.File.Model values)
    | Select (Select.Options String (Msg values)) (BaseField String values)
    | DatePicker (DatePicker.Options (Msg values)) (BaseField DatePicker.Model values)
    | UserPicker (UserPicker.Options (Msg values)) (BaseField UserPicker.Model values)
    | Group (List (Html.Attribute Never)) (List (FilledField values))
    | Arbitrary (Html (values -> values))


{-| A generic function to build a generic `Field`. We can use this function to
define more specific field constructors, such as `textField`
-}
field :
    (BaseField input values -> Field values)
    -> FieldConfig input output values
    -> Form values output
field build config =
    let
        parse_ values =
            config.parser (config.value values)
                |> Result.andThen
                    (\output ->
                        config.externalError values
                            |> Maybe.map (\error -> Err error)
                            |> Maybe.withDefault (Ok output)
                    )
                |> OptionalResult.fromResult

        field_ values =
            build
                { value = config.value values
                , getValue = config.value
                , update = \newValue -> config.update newValue values
                , updateWithValues = config.update
                }
    in
    Form
        (\values ->
            let
                result =
                    parse_ values
            in
            { fields =
                [ { state = field_ values
                  , error =
                        case result of
                            OptErr err ->
                                Just err

                            OptOk _ ->
                                Nothing

                            OptNothing ->
                                Nothing
                  , isRequired = True
                  }
                ]
            , result = OptionalResult.mapError (\_ -> ( getId (field_ values), [] )) result
            }
        )


{-| An input that receives text. Checkout `Form.Text` for more information on
what you can do with this field.
-}
textField :
    FieldConfig String output values
    -> Text.Options (Msg values)
    -> Form values output
textField config options =
    field (Text options) config


{-| An input that receives rich text. Allows formatting like bold, italic,
underline and links. Checkout `Form.RichText` for more information on what you
can do with this field.
-}
richText :
    { parser : Markdown -> Result String output
    , value : values -> RichText.Model
    , update : RichText.Model -> values -> values
    , externalError : values -> Maybe String
    }
    -> RichText.Options (Msg values)
    -> Form values output
richText config options =
    field (RichText options)
        { parser = RichText.getMarkdownContent >> config.parser
        , value = config.value
        , update = config.update
        , externalError = config.externalError
        }


{-| An input that represents either `True` or `False`. Checkout `Form.Toggle`
for more infromation on what you can do with this field.
-}
toggle :
    FieldConfig Bool output values
    -> Toggle.Options (Msg values)
    -> Form values output
toggle config options =
    field (Toggle options) config


{-| An input that represents either `True` or `False`. Checkout `Form.Checkbox`
for more information on what you can do with this field.
-}
checkbox :
    FieldConfig Bool output values
    -> Checkbox.Options (Msg values)
    -> Form values output
checkbox config options =
    field (Checkbox options) config


{-| An input that lets you select one out of a list of options. Checkout
`Form.Radio` for more information on what you can do with this field.
-}
radio :
    (String -> input)
    -> FieldConfig input output values
    -> Radio.Options input (Msg values)
    -> Form values output
radio optionFromString config options =
    let
        optionToString =
            Radio.getOptionToString options
    in
    field
        (Radio (Radio.map optionToString optionFromString options))
        (mapFieldConfig optionToString optionFromString config)


{-| An input that receives files. Checkout `Form.File` for more information on
what you can do with this field. Whenever a user selects a file, it is
automatically uploaded to our servers.
-}
file :
    { translators : Shared.Translators
    , value : values -> Form.File.Model
    , update : Form.File.Model -> values -> values
    , externalError : values -> Maybe String
    }
    -> Form.File.Options (Msg values)
    -> Form values String
file config options =
    field (File options)
        { parser = Form.File.parser config.translators
        , value = config.value
        , update = config.update
        , externalError = config.externalError
        }


{-| An input that lets you select one out of a list of options. Checkout
`Form.Select` for more information on what you can do with this field.
-}
select :
    (String -> input)
    -> FieldConfig input output values
    -> Select.Options input (Msg values)
    -> Form values output
select optionFromString config options =
    let
        optionToString =
            Select.getOptionToString options
    in
    field (Select (Select.map optionToString optionFromString options))
        (mapFieldConfig optionToString optionFromString config)


{-| An input that lets you select a date. Checkout `Form.DatePicker` for more
information on what you can do with this field.
-}
datePicker :
    { parser : Maybe Date -> Result String output
    , value : values -> DatePicker.Model
    , update : DatePicker.Model -> values -> values
    , externalError : values -> Maybe String
    }
    -> DatePicker.Options (Msg values)
    -> Form values output
datePicker config options =
    field (DatePicker options)
        { parser = DatePicker.getDate >> config.parser
        , value = config.value
        , update = config.update
        , externalError = config.externalError
        }


{-| An input that lets you select a single user. Checkout `Form.UserPicker` for
more information on what you can do with this field.
-}
userPicker :
    { parser : Maybe Profile.Minimal -> Result String output
    , value : values -> UserPicker.SinglePickerModel
    , update : UserPicker.SinglePickerModel -> values -> values
    , externalError : values -> Maybe String
    }
    -> UserPicker.Options (Msg values)
    -> Form values output
userPicker config options =
    field
        (mapBaseField UserPicker.fromSinglePicker UserPicker.toSinglePicker
            >> UserPicker options
        )
        { parser = UserPicker.getSingleProfile >> config.parser
        , value = config.value
        , update = config.update
        , externalError = config.externalError
        }


{-| An input that lets you select multiple users. Checkout `Form.UserPicker` for
more information on what you can do with this field.
-}
userPickerMultiple :
    { parser : List Profile.Minimal -> Result String output
    , value : values -> UserPicker.MultiplePickerModel
    , update : UserPicker.MultiplePickerModel -> values -> values
    , externalError : values -> Maybe String
    }
    -> UserPicker.Options (Msg values)
    -> Form values output
userPickerMultiple config options =
    field
        (mapBaseField UserPicker.fromMultiplePicker UserPicker.toMultiplePicker
            >> UserPicker options
        )
        { parser = UserPicker.getMultipleProfiles >> config.parser
        , value = config.value
        , update = config.update
        , externalError = config.externalError
        }



-- COMPOSING


{-| Builds a form that always succeeds. Similar to Json.Decode.Pipeline or
Graphql.SelectionSet, this is useful to start a pipeline chain. Give it a
function that transforms a dirty model into a clean one, and build the form
[`with`](#with) other fields.
-}
succeed : output -> Form values output
succeed output =
    Form (\_ -> { fields = [], result = OptOk output })


{-| Builds a form that always fails. This is similar to Json.Decode.fail, and it
can be used in conjunction with `introspect`: maybe you only want to show some
fields if anothe field is filled out, otherwise, the form should fail.
-}
fail : Form values output
fail =
    Form (\_ -> { fields = [], result = OptNothing })


{-| Appends a form into another form. Since every field is a form on itself, we
can use this to add the fields we need on a form. This is supposed to be used in
a pipeline, together with `succeed`.
-}
with :
    Form values a
    -> Form values (a -> b)
    -> Form values b
with new current =
    Form
        (\values ->
            let
                filledNew =
                    fill new values

                filledCurrent =
                    fill current values
            in
            { fields = filledNew.fields ++ filledCurrent.fields
            , result =
                case filledCurrent.result of
                    OptOk fn ->
                        OptionalResult.map fn filledNew.result

                    OptNothing ->
                        case filledNew.result of
                            OptErr errors ->
                                OptErr errors

                            OptOk _ ->
                                OptNothing

                            OptNothing ->
                                OptNothing

                    OptErr ( firstError, otherErrors ) ->
                        case filledNew.result of
                            OptOk _ ->
                                OptErr ( firstError, otherErrors )

                            OptNothing ->
                                OptErr ( firstError, otherErrors )

                            OptErr ( secondFirstError, secondOtherErrors ) ->
                                OptErr
                                    ( firstError
                                    , otherErrors ++ (secondFirstError :: secondOtherErrors)
                                    )
            }
        )


{-| Use this if you want to nest forms. If you need to break forms apart for any
reason (reusability, making it easier to follow, etc.), you can use this function
to nest them!

    type alias Input =
        { name : String, address : AddressInput }

    type alias Output =
        { name : String, address : Address }

    addressForm : Form AddressInput Address

    userForm : Form Input Output
    userForm =
        Form.succeed Output
            |> Form.with nameField
            |> Form.withNesting
                { value = .address
                , update = \address user -> { user | address = address }
                }
                addressForm

-}
withNesting :
    { value : parent -> child
    , update : child -> parent -> parent
    }
    -> Form child a
    -> Form parent (a -> b)
    -> Form parent b
withNesting mappings child parent =
    with (mapValues mappings child) parent


{-| Display two fields as a group. You can pass in arbitrary HTML attributes, as
long as they don't emit Msgs.
-}
withGroup :
    List (Html.Attribute Never)
    -> Form values a
    -> Form values b
    -> Form values (a -> b -> c)
    -> Form values c
withGroup attributes leftSide rightSide current =
    Form
        (\values ->
            let
                filledLeft =
                    fill leftSide values

                filledRight =
                    fill rightSide values

                filledCurrent =
                    fill current values
            in
            { fields =
                { state = Group attributes (filledLeft.fields ++ filledRight.fields)
                , error = Nothing
                , isRequired = False
                }
                    :: filledCurrent.fields
            , result =
                let
                    leftErrors =
                        case filledLeft.result of
                            OptErr ( first, others ) ->
                                first :: others

                            _ ->
                                []

                    rightErrors =
                        case filledRight.result of
                            OptErr ( first, others ) ->
                                first :: others

                            _ ->
                                []

                    leftAndRightErrors =
                        leftErrors ++ rightErrors
                in
                case filledCurrent.result of
                    OptOk fn ->
                        OptionalResult.map2 fn filledLeft.result filledRight.result

                    OptErr ( currentError, currentErrors ) ->
                        OptErr ( currentError, currentErrors ++ leftAndRightErrors )

                    OptNothing ->
                        List.head leftAndRightErrors
                            |> Maybe.map (\head -> OptErr ( head, List.drop 1 leftAndRightErrors ))
                            |> Maybe.withDefault OptNothing
            }
        )


{-| Arbitrary HTML that can change the values of the form. You should
usually not need this, but it can be useful for forms that need buttons to
manipulate the input, or something similar. This is usually used with
`withNoOutput`

    Form.withNoOutput
        (button
            [ onClick
                (\values ->
                    { values
                        | interests = form.interest :: form.interests
                        , interest = ""
                    }
                )
            ]
            [ text "Add" ]
            |> Form.arbitrary
        )

-}
arbitrary : Html (values -> values) -> Form values output
arbitrary html =
    Form
        (\_ ->
            { fields =
                [ { state = Arbitrary html
                  , error = Nothing
                  , isRequired = False
                  }
                ]
            , result = OptNothing
            }
        )


{-| Look at the values of the form before building the next form. Useful when
some form depends on other fields
-}
introspect : (values -> Form values output) -> Form values output
introspect buildForm =
    Form (\values -> fill (buildForm values) values)


{-| Makes a form optional. Turns the result of the form into `Maybe output`. If
the form is empty, we don't care if the parsed value is ok, and the result is
`Nothing`. If the form has some content, the result is `Just` the parsed value.

    type alias User =
        { middleName : Maybe String }

    type alias DirtyUser =
        { middleName : String }

    userForm : Form DirtyUser User
    userForm =
        Form.succeed User
            |> Form.with
                (Form.Text.init {}
                    |> Form.textField
                        { parser =
                            \middleName ->
                                if String.length middleName > 3 then
                                    Ok middleName

                                else
                                    Err "Middle name must be > 3 characters"
                        , value = .middleName
                        , update = \middleName dirtyUser -> { dirtyUser | middleName = middleName }
                        , externalError = always Nothing
                        }
                    |> Form.optional
                )

The above example pictures a form where the user can enter their middle name. If
they don't have a middle name, they can just skip the form, and we will get back
a user with `middleName` as `Nothing`. If they do have a middle name, for some
reason we demand that their middle name is longer than 3 characters. If their
actual middle name is less than 3 characters long, they're out of luck 🤷

-}
optional : Form values output -> Form values (Maybe output)
optional form =
    Form
        (\values ->
            let
                filledForm =
                    fill form values

                isFilledFormEmpty =
                    List.all (.state >> isEmpty) filledForm.fields
            in
            { fields =
                List.map
                    (\field_ ->
                        { field_
                            | isRequired = False
                            , error =
                                if isFilledFormEmpty then
                                    Nothing

                                else
                                    field_.error
                        }
                    )
                    filledForm.fields
            , result =
                if isFilledFormEmpty then
                    OptOk Nothing

                else
                    case filledForm.result of
                        OptOk ok ->
                            OptOk (Just ok)

                        OptErr _ ->
                            OptNothing

                        OptNothing ->
                            OptNothing
            }
        )


{-| Just like `with`, but don't consider the result in the final output.
-}
withNoOutput : Form values x -> Form values output -> Form values output
withNoOutput new current =
    Form
        (\values ->
            let
                filledNew =
                    fill new values

                filledCurrent =
                    fill current values
            in
            { fields = filledNew.fields ++ filledCurrent.fields
            , result = filledCurrent.result
            }
        )


{-| Add some decoration to a form
-}
withDecoration : Html Never -> Form values output -> Form values output
withDecoration decoration =
    withNoOutput (arbitrary (Html.map (\_ -> identity) decoration))


{-| Given some values (a dirty model), fill a form with them.
-}
fill :
    Form values output
    -> values
    -> FilledForm values output
fill (Form fill_) =
    fill_



-- MODEL


{-| This is what you should keep in your model. It basically keeps the value to
feed to a `Form`, and which errors to show
-}
type Model values
    = Model
        { values : values
        , errorTracking : ErrorTracking
        , disabled : Bool
        }


{-| Use this to initialize a `Model`, and store the result into your particular
model. You give the initial values of the form, and can then use this `Model` to
display a `Form` (see `update` and `view` below)
-}
init : values -> Model values
init values =
    Model
        { values = values
        , errorTracking =
            ErrorTracking
                { showAllErrors = False
                , showFieldError = Set.empty
                }
        , disabled = False
        }


{-| Manually update values from a form model. Useful if you get some remote data
after initializing the form and need to change the state of the form.
-}
updateValues : (values -> values) -> Model values -> Model values
updateValues updateFn (Model model) =
    Model { model | values = updateFn model.values }


{-| Set the entire form as disabled or enabled at once (if specific fields are
set as disabled, they'll stay disabled even if you set this to `True`). Useful
when you want to disable the form after the user submits it.
-}
withDisabled : Bool -> Model values -> Model values
withDisabled disabled (Model model) =
    Model { model | disabled = disabled }


{-| Determines which errors we should show. This is opaque so it can't be
modified on the outside
-}
type ErrorTracking
    = ErrorTracking { showAllErrors : Bool, showFieldError : Set String }



-- UPDATE


{-| The result of calling `update`. Use with `UR.fromChild` or `UR.addChild`.
The external message is the output, meaning you can just give one of those `UR`
functions a msg that will be fired whenever the user submits a valid form
-}
type alias UpdateResult values =
    UR.UpdateResult (Model values) (Msg values) Feedback.Model


type Msg values
    = NoOp
    | ChangedValues values
    | GotRichTextMsg (values -> RichText.Model) (RichText.Model -> values -> values) RichText.Msg
    | GotFileMsg (values -> Form.File.Model) (Form.File.Model -> values -> values) Form.File.Msg
    | GotDatePickerMsg (DatePicker.Options (Msg values)) (DatePicker.ViewConfig (Msg values)) (values -> DatePicker.Model) (DatePicker.Model -> values -> values) DatePicker.Msg
    | GotUserPickerMsg (UserPicker.Options (Msg values)) (UserPicker.ViewConfig (Msg values)) (values -> UserPicker.Model) (UserPicker.Model -> values -> values) UserPicker.Msg
    | BlurredField String
    | ClickedSubmitWithErrors ( String, List String )


{-| Call this inside your update function. Use with `UR.fromChild` or
`UR.addChild`:

    case msg of
        GotFormMsg subMsg ->
            Form.update subMsg model.form
                |> UR.fromChild
                    (\form -> { model | form = form })
                    GotFormMsg
                    SubmittedForm
                    model

Where `SubmittedForm` is the message that will be fired whenever the form is
submitted when valid:

    type Msg
        = GotFormMsg (Form.Msg DirtyForm ValidForm)
        | SubmittedForm ValidForm

-}
update : Shared -> Msg values -> Model values -> UpdateResult values
update shared msg (Model model) =
    let
        (ErrorTracking errorTracking) =
            model.errorTracking
    in
    case msg of
        NoOp ->
            UR.init (Model model)

        ChangedValues newValues ->
            Model { model | values = newValues }
                |> UR.init

        GotRichTextMsg getModel updateFn subMsg ->
            let
                ( newModel, cmd ) =
                    RichText.update subMsg (getModel model.values)
            in
            Model { model | values = updateFn newModel model.values }
                |> UR.init
                |> UR.addCmd (Cmd.map (GotRichTextMsg getModel updateFn) cmd)

        GotFileMsg getModel updateFn subMsg ->
            Form.File.update shared subMsg (getModel model.values)
                |> UR.fromChild (\newFile -> Model { model | values = updateFn newFile model.values })
                    (GotFileMsg getModel updateFn)
                    identity
                    (Model model)

        GotDatePickerMsg options viewConfig getModel updateFn subMsg ->
            let
                ( newModel, cmd ) =
                    DatePicker.update options viewConfig subMsg (getModel model.values)
            in
            Model { model | values = updateFn newModel model.values }
                |> UR.init
                |> UR.addCmd (Cmd.map (GotDatePickerMsg options viewConfig getModel updateFn) cmd)

        GotUserPickerMsg options viewConfig getModel updateFn subMsg ->
            let
                ( newModel, cmd, maybeExternalMsg ) =
                    UserPicker.update options viewConfig subMsg (getModel model.values)

                maybeAddMsg maybeMsg =
                    case maybeMsg of
                        Nothing ->
                            identity

                        Just externalMsg ->
                            UR.addMsg externalMsg
            in
            Model { model | values = updateFn newModel model.values }
                |> UR.init
                |> UR.addCmd (Cmd.map (GotUserPickerMsg options viewConfig getModel updateFn) cmd)
                |> maybeAddMsg maybeExternalMsg

        BlurredField fieldId ->
            Model
                { model
                    | errorTracking =
                        ErrorTracking
                            { errorTracking
                                | showFieldError =
                                    Set.insert fieldId errorTracking.showFieldError
                            }
                }
                |> UR.init

        ClickedSubmitWithErrors ( firstError, _ ) ->
            { model | errorTracking = ErrorTracking { errorTracking | showAllErrors = True } }
                |> Model
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.focus firstError
                        |> Task.attempt (\_ -> NoOp)
                    )


msgToString : Msg values -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        GotRichTextMsg _ _ subMsg ->
            "GotRichTextMsg" :: RichText.msgToString subMsg

        GotFileMsg _ _ subMsg ->
            "GotFileMsg" :: Form.File.msgToString subMsg

        GotDatePickerMsg _ _ _ _ subMsg ->
            "GotDatePickerMsg" :: DatePicker.msgToString subMsg

        GotUserPickerMsg _ _ _ _ subMsg ->
            "GotUserPickerMsg" :: UserPicker.msgToString subMsg

        ChangedValues _ ->
            [ "ChangedValues" ]

        BlurredField _ ->
            [ "BlurredField" ]

        ClickedSubmitWithErrors _ ->
            [ "ClickedSubmitWithErrors" ]



-- VIEW


{-| Provide a form and a dirty model, and get back some HTML
-}
view :
    List (Html.Attribute msg)
    -> Shared.Translators
    -> ((List (Html.Attribute Never) -> (List (Html msg) -> Html msg)) -> List (Html msg))
    -> Form values output
    -> Model values
    ->
        { toMsg : Msg values -> msg
        , onSubmit : output -> msg
        }
    -> Html msg
view formAttrs translators footer form model { toMsg, onSubmit } =
    Html.form
        (novalidate True
            :: Events.onSubmit
                (parse form
                    model
                    { onError = toMsg
                    , onSuccess = onSubmit
                    }
                )
            :: formAttrs
        )
        (List.map (Html.map toMsg)
            (viewFields translators form model)
            ++ footer
                (\attrs ->
                    button
                        (type_ "submit"
                            :: List.map (Html.Attributes.map (\_ -> toMsg NoOp)) attrs
                        )
                )
        )


{-| Provide a form and a dirty model, and get back some HTML. You should usually
use `view`, but you can use this function on the rare case where you need a form
without a submit button. You can provide a custom footer, just like in `view`,
but it won't include a submit button (and the form won't have a `onSubmit` event
listener)
-}
viewWithoutSubmit :
    List (Html.Attribute msg)
    -> Shared.Translators
    -> (values -> List (Html msg))
    -> Form values output
    -> Model values
    -> { toMsg : Msg values -> msg }
    -> Html msg
viewWithoutSubmit formAttrs translators footer form (Model model) { toMsg } =
    Html.form (novalidate True :: formAttrs)
        (List.map (Html.map toMsg) (viewFields translators form (Model model))
            ++ footer model.values
        )


viewFields : Shared.Translators -> Form values output -> Model values -> List (Html (Msg values))
viewFields translators form (Model model) =
    let
        filledForm =
            fill form model.values

        (ErrorTracking errorTracking) =
            model.errorTracking
    in
    filledForm.fields
        |> List.reverse
        |> List.map
            (\field_ ->
                let
                    shouldShowFieldError =
                        Set.member (getId field_.state) errorTracking.showFieldError
                in
                viewField
                    { showError = shouldShowFieldError || errorTracking.showAllErrors
                    , translators = translators
                    , disabled = model.disabled
                    , values = model.values
                    }
                    field_
            )


{-| Parse a form with some values. If there are errors, they are highlighted in
the form. Otherwise, fire a msg containing the output of the form.

You shouldn't need this function often, as it's already called when submitting a
form, but it could be useful if you need more than one button to validate the
form, or if you want to do it programatically.

-}
parse :
    Form values output
    -> Model values
    ->
        { onError : Msg values -> msg
        , onSuccess : output -> msg
        }
    -> msg
parse form (Model model) { onError, onSuccess } =
    let
        filledForm =
            fill form model.values
    in
    case filledForm.result of
        OptErr errors ->
            onError (ClickedSubmitWithErrors errors)

        OptOk validForm ->
            onSuccess validForm

        OptNothing ->
            onError NoOp


viewField :
    { showError : Bool
    , translators : Shared.Translators
    , disabled : Bool
    , values : values
    }
    -> FilledField values
    -> Html (Msg values)
viewField { showError, translators, disabled, values } { state, error, isRequired } =
    let
        hasError =
            showError && Maybe.Extra.isJust error

        disableIfNotAlreadyDisabled : options -> (Bool -> options -> options) -> options
        disableIfNotAlreadyDisabled options setDisabled =
            if disabled then
                setDisabled True options

            else
                options
    in
    case state of
        Text options baseField ->
            Text.view (disableIfNotAlreadyDisabled options Text.withDisabled)
                { onChange = baseField.update >> ChangedValues
                , onBlur = BlurredField
                , value = baseField.value
                , error = viewError (Text.getErrorAttrs options) showError error
                , hasError = hasError
                , isRequired = isRequired
                , translators = translators
                }

        RichText options baseField ->
            RichText.view (disableIfNotAlreadyDisabled options RichText.withDisabled)
                { onBlur = BlurredField
                , value = baseField.value
                , error = viewError [] showError error
                , hasError = hasError
                , isRequired = isRequired
                , translators = translators
                }
                (GotRichTextMsg baseField.getValue baseField.updateWithValues)

        Toggle options baseField ->
            Toggle.view (disableIfNotAlreadyDisabled options Toggle.withDisabled)
                { onToggle = baseField.update >> ChangedValues
                , onBlur = BlurredField
                , value = baseField.value
                , error = viewError [] showError error
                , hasError = hasError
                , isRequired = isRequired
                , translators = translators
                }

        Checkbox options baseField ->
            Checkbox.view (disableIfNotAlreadyDisabled options Checkbox.withDisabled)
                { value = baseField.value
                , onCheck = baseField.update >> ChangedValues
                , onBlur = BlurredField
                , error = viewError [] showError error
                , hasError = hasError
                , isRequired = isRequired
                }

        Radio options baseField ->
            Radio.view (disableIfNotAlreadyDisabled options Radio.withDisabled)
                { onSelect = baseField.update >> ChangedValues
                , onBlur = BlurredField
                , value = baseField.value
                , error = viewError [] showError error
                , hasError = hasError
                }

        File options baseField ->
            Form.File.view (disableIfNotAlreadyDisabled options Form.File.withDisabled)
                { value = baseField.value
                , error = viewError [] showError error
                , hasError = hasError
                , isRequired = isRequired
                , translators = translators
                }
                (GotFileMsg baseField.getValue baseField.updateWithValues)

        Select options baseField ->
            Select.view (disableIfNotAlreadyDisabled options Select.withDisabled)
                { onSelect = baseField.update >> ChangedValues
                , onBlur = BlurredField
                , value = baseField.value
                , error = viewError [] showError error
                , hasError = hasError
                , isRequired = isRequired
                }

        DatePicker options baseField ->
            let
                viewConfig =
                    { value = baseField.value
                    , error = viewError [] showError error
                    , hasError = hasError
                    , isRequired = isRequired
                    , translators = translators
                    }
            in
            DatePicker.view (disableIfNotAlreadyDisabled options DatePicker.withDisabled)
                viewConfig
                (GotDatePickerMsg options viewConfig baseField.getValue baseField.updateWithValues)

        UserPicker options baseField ->
            let
                viewConfig =
                    { onBlur = BlurredField
                    , value = baseField.value
                    , error = viewError [] showError error
                    , hasError = hasError
                    , translators = translators
                    }
            in
            UserPicker.view (disableIfNotAlreadyDisabled options UserPicker.withDisabled)
                viewConfig
                (GotUserPickerMsg options viewConfig baseField.getValue baseField.updateWithValues)

        Group attrs fields ->
            Html.div
                (List.map (Html.Attributes.map (\_ -> NoOp)) attrs)
                (List.map
                    (viewField
                        { showError = showError
                        , translators = translators
                        , disabled = disabled
                        , values = values
                        }
                    )
                    fields
                )

        Arbitrary html ->
            Html.map (\fn -> ChangedValues (fn values)) html


viewError : List (Html.Attribute msg) -> Bool -> Maybe String -> Html msg
viewError attributes showError maybeError =
    case maybeError of
        Nothing ->
            Html.text ""

        Just error ->
            if showError then
                Html.p (class "form-error" :: attributes)
                    [ Html.text error ]

            else
                Html.text ""



-- INTERNAL HELPERS


getId : Field values -> String
getId state =
    case state of
        Text options _ ->
            Text.getId options

        RichText _ baseField ->
            RichText.getId baseField.value

        Toggle options _ ->
            Toggle.getId options

        Checkbox options _ ->
            Checkbox.getId options

        Radio options _ ->
            Radio.getId options

        File options _ ->
            Form.File.getId options

        Select options _ ->
            Select.getId options

        DatePicker options _ ->
            DatePicker.getId options

        UserPicker _ baseField ->
            UserPicker.getId baseField.value

        Group _ fields ->
            fields
                |> List.head
                |> Maybe.map (.state >> getId)
                |> Maybe.withDefault ""

        Arbitrary _ ->
            ""


isEmpty : Field values -> Bool
isEmpty field_ =
    case field_ of
        Text _ { value } ->
            String.isEmpty value

        RichText _ { value } ->
            value
                |> RichText.getMarkdownContent
                |> Markdown.toUnformattedString
                |> String.isEmpty

        Toggle _ _ ->
            False

        Checkbox _ _ ->
            -- There's no way a checkbox can be empty - we don't use the
            -- indeterminate state, so it's value is either `True` or `False`
            False

        Radio _ _ ->
            False

        File _ { value } ->
            Form.File.isEmpty value

        Select _ _ ->
            False

        DatePicker _ { value } ->
            Maybe.Extra.isNothing (DatePicker.getDate value)

        UserPicker _ { value } ->
            UserPicker.isEmpty value

        Group _ fields ->
            fields
                |> List.map .state
                |> List.any isEmpty

        Arbitrary _ ->
            False


mapFieldConfig :
    (input -> mappedInput)
    -> (mappedInput -> input)
    -> FieldConfig input output values
    -> FieldConfig mappedInput output values
mapFieldConfig fn reverseFn config =
    { parser = reverseFn >> config.parser
    , value = config.value >> fn
    , update = \mappedInput -> config.update (reverseFn mappedInput)
    , externalError = config.externalError
    }


mapBaseField :
    (value -> mappedValue)
    -> (mappedValue -> value)
    -> BaseField value values
    -> BaseField mappedValue values
mapBaseField fn reverseFn baseField =
    { value = fn baseField.value
    , getValue = baseField.getValue >> fn
    , update = reverseFn >> baseField.update
    , updateWithValues = reverseFn >> baseField.updateWithValues
    }


mapBaseFieldValues :
    (values -> mappedValues)
    -> (mappedValues -> values)
    -> BaseField value values
    -> BaseField value mappedValues
mapBaseFieldValues fn reverseFn baseField =
    { value = baseField.value
    , getValue = reverseFn >> baseField.getValue
    , update = baseField.update >> fn
    , updateWithValues =
        \value values ->
            reverseFn values
                |> baseField.updateWithValues value
                |> fn
    }


{-| You can use this function to nest forms inside one another! Just give a way
to get the child form from the parent form, and a way to update the child on the
parent. If you only want to use nesting, you can use the more pipeline-friendly
`withNesting`.
-}
mapValues :
    { value : parent -> child
    , update : child -> parent -> parent
    }
    -> Form child output
    -> Form parent output
mapValues mappings child =
    Form
        (\values ->
            let
                filledChild =
                    fill child (mappings.value values)
            in
            { fields =
                List.map
                    (mapFilledField (\child_ -> mappings.update child_ values) mappings.value)
                    filledChild.fields
            , result = filledChild.result
            }
        )


{-| Change the output of a form.
-}
mapOutput : (output -> mappedOutput) -> Form values output -> Form values mappedOutput
mapOutput fn form =
    Form
        (\values ->
            let
                filledForm =
                    fill form values
            in
            { fields = filledForm.fields
            , result = OptionalResult.map fn filledForm.result
            }
        )


mapFilledField : (values -> mappedValues) -> (mappedValues -> values) -> FilledField values -> FilledField mappedValues
mapFilledField fn reverseFn filledField =
    { state = mapField fn reverseFn filledField.state
    , error = filledField.error
    , isRequired = filledField.isRequired
    }


mapField : (values -> mappedValues) -> (mappedValues -> values) -> Field values -> Field mappedValues
mapField fn reverseFn field_ =
    let
        baseMap fieldType specificMap options baseField =
            fieldType (specificMap (mapMsg fn reverseFn) options)
                (mapBaseFieldValues fn reverseFn baseField)
    in
    case field_ of
        Text options baseField ->
            baseMap Text Text.map options baseField

        RichText options baseField ->
            baseMap RichText RichText.map options baseField

        Toggle options baseField ->
            baseMap Toggle Toggle.map options baseField

        Checkbox options baseField ->
            baseMap Checkbox Checkbox.map options baseField

        Radio options baseField ->
            baseMap Radio Radio.mapMsg options baseField

        File options baseField ->
            baseMap File Form.File.map options baseField

        Select options baseField ->
            baseMap Select Select.mapMsg options baseField

        DatePicker options baseField ->
            baseMap DatePicker DatePicker.map options baseField

        UserPicker options baseField ->
            baseMap UserPicker UserPicker.map options baseField

        Group attributes fields ->
            Group attributes (List.map (mapFilledField fn reverseFn) fields)

        Arbitrary html ->
            Arbitrary (Html.map (\htmlFn -> reverseFn >> htmlFn >> fn) html)


mapMsg : (values -> mappedValues) -> (mappedValues -> values) -> Msg values -> Msg mappedValues
mapMsg fn reverseFn msg =
    case msg of
        NoOp ->
            NoOp

        ChangedValues values ->
            ChangedValues (fn values)

        GotRichTextMsg getModel updateFn subMsg ->
            GotRichTextMsg
                (reverseFn >> getModel)
                (\value values -> reverseFn values |> updateFn value |> fn)
                subMsg

        GotFileMsg getModel updateFn subMsg ->
            GotFileMsg (reverseFn >> getModel)
                (\value values -> reverseFn values |> updateFn value |> fn)
                subMsg

        GotDatePickerMsg options viewConfig getModel updateFn subMsg ->
            GotDatePickerMsg (DatePicker.map (mapMsg fn reverseFn) options)
                (DatePicker.mapViewConfig (mapMsg fn reverseFn) viewConfig)
                (reverseFn >> getModel)
                (\value values -> reverseFn values |> updateFn value |> fn)
                subMsg

        GotUserPickerMsg options viewConfig getModel updateFn subMsg ->
            GotUserPickerMsg (UserPicker.map (mapMsg fn reverseFn) options)
                (UserPicker.mapViewConfig (mapMsg fn reverseFn) viewConfig)
                (reverseFn >> getModel)
                (\value values -> reverseFn values |> updateFn value |> fn)
                subMsg

        BlurredField fieldId ->
            BlurredField fieldId

        ClickedSubmitWithErrors errors ->
            ClickedSubmitWithErrors errors
