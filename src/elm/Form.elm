module Form exposing
    ( Form
    , succeed, with, withOptional
    , textField, checkbox, radio, select, file
    , view, Model, init, Msg, update, msgToString
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

@docs succeed, with, withOptional


## Fields

@docs textField, checkbox, radio, select, file


## Viewing

@docs view, Model, init, Msg, update, msgToString

-}

import Api
import Browser.Dom
import File
import Form.Checkbox as Checkbox
import Form.File
import Form.Radio as Radio
import Form.Select as Select
import Form.Text as Text
import Html exposing (Html, button)
import Html.Attributes exposing (class, novalidate, type_)
import Html.Events as Events
import Http
import Maybe.Extra
import RemoteData exposing (RemoteData)
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
    , result : Result ( String, List String ) output
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
    | Checkbox (Checkbox.Options (Msg values)) (BaseField Bool values)
    | Radio (Radio.Options String (Msg values)) (BaseField String values)
    | File (Form.File.Options (Msg values)) (BaseField (RemoteData Http.Error String) values)
    | Select (Select.Options String (Msg values)) (BaseField String values)


{-| A generic function to build a generic `Field`. We can use this function to
define more specific field constructors, such as `textField`
-}
field :
    (BaseField input values -> Field values)
    -> FieldConfig input output values
    -> Form values output
field build config =
    let
        parse values =
            config.parser (config.value values)
                |> Result.andThen
                    (\output ->
                        config.externalError values
                            |> Maybe.map (\error -> Err error)
                            |> Maybe.withDefault (Ok output)
                    )

        field_ values =
            build
                { value = config.value values
                , update = \newValue -> config.update newValue values
                , updateWithValues = config.update
                }
    in
    Form
        (\values ->
            let
                result =
                    parse values
            in
            { fields =
                [ { state = field_ values
                  , error =
                        case result of
                            Err err ->
                                Just err

                            Ok _ ->
                                Nothing
                  , isRequired = True
                  }
                ]
            , result = Result.mapError (\_ -> ( getId (field_ values), [] )) result
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
    { parser : String -> output
    , failureErrorMessage : Http.Error -> String
    , loadingErrorMessage : String
    , notAskedErrorMessage : String
    , value : values -> RemoteData Http.Error String
    , update : RemoteData Http.Error String -> values -> values
    , externalError : values -> Maybe String
    }
    -> Form.File.Options (Msg values)
    -> Form values output
file config options =
    field (File options)
        { parser =
            \remoteData ->
                case remoteData of
                    RemoteData.Success a ->
                        Ok (config.parser a)

                    RemoteData.Failure err ->
                        Err (config.failureErrorMessage err)

                    RemoteData.Loading ->
                        Err config.loadingErrorMessage

                    RemoteData.NotAsked ->
                        Err config.notAskedErrorMessage
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



-- COMPOSING


{-| Builds a form that always succeeds. Similar to Json.Decode.Pipeline or
Graphql.SelectionSet, this is useful to start a pipeline chain. Give it a
function that transforms a dirty model into a clean one, and build the form
[`with`](#with) other fields.
-}
succeed : output -> Form values output
succeed output =
    Form (\_ -> { fields = [], result = Ok output })


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
                    Ok fn ->
                        Result.map fn filledNew.result

                    Err ( firstError, otherErrors ) ->
                        case filledNew.result of
                            Ok _ ->
                                Err ( firstError, otherErrors )

                            Err ( secondFirstError, secondOtherErrors ) ->
                                Err
                                    ( firstError
                                    , otherErrors ++ (secondFirstError :: secondOtherErrors)
                                    )
            }
        )


{-| Appends an optional form into another form. Similar to `with`, but the
result of the form is a `Maybe output`. If the form to be appended is empty, we
don't care if the parsed value is ok, and the result is `Nothing`. If the form
to be appended has some content, the result is `Just` the parsed value.

    type alias User =
        { middleName : Maybe String }

    type alias DirtyUser =
        { middleName : String }

    userForm : Form DirtyUser User
    userForm =
        Form.succeed User
            |> Form.withOptional
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
                )

The above example pictures a form where the user can enter their middle name. If
they don't have a middle name, they can just skip the form, and we will get back
a user with `middleName` as `Nothing`. If they do have a middle name, for some
reason we demand that their middle name is longer than 3 characters. If their
actual middle name is less than 3 characters long, they're out of luck 🤷

-}
withOptional :
    Form values a
    -> Form values (Maybe a -> b)
    -> Form values b
withOptional new current =
    Form
        (\values ->
            let
                filledNew =
                    fill new values

                filledCurrent =
                    fill current values

                isFilledNewEmpty =
                    List.all (.state >> isEmpty) filledNew.fields

                filledNewFields =
                    if isFilledNewEmpty then
                        List.map (\field_ -> { field_ | error = Nothing })
                            filledNew.fields

                    else
                        filledNew.fields
            in
            { fields =
                List.map (\field_ -> { field_ | isRequired = False }) filledNewFields
                    ++ filledCurrent.fields
            , result =
                case ( filledCurrent.result, filledNew.result ) of
                    ( Ok fn, Ok result ) ->
                        if isFilledNewEmpty then
                            Ok (fn Nothing)

                        else
                            Ok (fn (Just result))

                    ( Err err, Ok _ ) ->
                        Err err

                    ( Ok fn, Err err ) ->
                        if isFilledNewEmpty then
                            Ok (fn Nothing)

                        else
                            Err err

                    ( Err ( firstError, otherErrors ), Err ( secondFirstError, secondOtherErrors ) ) ->
                        let
                            listOfErrors =
                                if isFilledNewEmpty then
                                    otherErrors

                                else
                                    otherErrors ++ (secondFirstError :: secondOtherErrors)
                        in
                        Err
                            ( firstError
                            , listOfErrors
                            )
            }
        )


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
        }


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
    | RequestedUploadFile (RemoteData Http.Error String -> values -> values) File.File
    | CompletedUploadingFile (RemoteData Http.Error String -> values -> values) (Result Http.Error String)
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

        RequestedUploadFile updateFn fileToUpload ->
            { model | values = updateFn RemoteData.Loading model.values }
                |> Model
                |> UR.init
                |> UR.addCmd
                    (Api.uploadImage shared
                        fileToUpload
                        (CompletedUploadingFile updateFn)
                    )

        CompletedUploadingFile updateFn (Err error) ->
            { model | values = updateFn (RemoteData.Failure error) model.values }
                |> Model
                |> UR.init
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure
                        (shared.translators.t "error.file_upload")
                    )
                |> UR.logHttpError msg
                    Nothing
                    "Error uploading file"
                    { moduleName = "Form", function = "update" }
                    []
                    error

        CompletedUploadingFile updateFn fileResult ->
            { model | values = updateFn (RemoteData.fromResult fileResult) model.values }
                |> Model
                |> UR.init

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

        ChangedValues _ ->
            [ "ChangedValues" ]

        RequestedUploadFile _ _ ->
            [ "RequestedUploadFile" ]

        CompletedUploadingFile _ r ->
            [ "CompletedUploadingFile", UR.resultToString r ]

        BlurredField _ ->
            [ "BlurredField" ]

        ClickedSubmitWithErrors _ ->
            [ "ClickedSubmitWithErrors" ]



-- VIEW


{-| Provide a form and a dirty model, and get back some HTML
-}
view :
    List (Html.Attribute msg)
    ->
        { buttonAttrs : List (Html.Attribute (Msg values))
        , buttonLabel : List (Html (Msg values))
        , translators : Shared.Translators
        }
    -> Form values output
    -> Model values
    -> (Msg values -> msg)
    -> (output -> msg)
    -> Html msg
view formAttrs { buttonAttrs, buttonLabel, translators } form (Model model) toMsg onSubmit =
    let
        filledForm =
            fill form model.values

        (ErrorTracking errorTracking) =
            model.errorTracking

        fields =
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
                            }
                            field_
                    )
    in
    Html.form
        (novalidate True
            :: Events.onSubmit
                (case filledForm.result of
                    Ok validForm ->
                        onSubmit validForm

                    Err errors ->
                        toMsg (ClickedSubmitWithErrors errors)
                )
            :: formAttrs
        )
        (List.map (Html.map toMsg)
            (fields
                ++ [ button
                        (type_ "submit"
                            :: class "button button-primary"
                            :: buttonAttrs
                        )
                        buttonLabel
                   ]
            )
        )


viewField :
    { showError : Bool, translators : Shared.Translators }
    -> FilledField values
    -> Html (Msg values)
viewField { showError, translators } { state, error, isRequired } =
    let
        hasError =
            showError && Maybe.Extra.isJust error
    in
    case state of
        Text options baseField ->
            Text.view options
                { onChange = baseField.update >> ChangedValues
                , onBlur = BlurredField
                , value = baseField.value
                , error = viewError (Text.getErrorAttrs options) showError error
                , hasError = hasError
                , isRequired = isRequired
                , translators = translators
                }

        Checkbox options baseField ->
            Checkbox.view options
                { value = baseField.value
                , onCheck = baseField.update >> ChangedValues
                , onBlur = BlurredField
                , error = viewError [] showError error
                , hasError = hasError
                , isRequired = isRequired
                }

        Radio options baseField ->
            Radio.view options
                { onSelect = baseField.update >> ChangedValues
                , onBlur = BlurredField
                , value = baseField.value
                , error = viewError [] showError error
                , hasError = hasError
                }

        File options baseField ->
            Form.File.view options
                { onInput = RequestedUploadFile baseField.updateWithValues
                , value = baseField.value
                , error = viewError [] showError error
                , hasError = hasError
                , isRequired = isRequired
                , translators = translators
                }

        Select options baseField ->
            Select.view options
                { onSelect = baseField.update >> ChangedValues
                , onBlur = BlurredField
                , value = baseField.value
                , error = viewError [] showError error
                , hasError = hasError
                , isRequired = isRequired
                }


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

        Checkbox options _ ->
            Checkbox.getId options

        Radio options _ ->
            Radio.getId options

        File options _ ->
            Form.File.getId options

        Select options _ ->
            Select.getId options


isEmpty : Field values -> Bool
isEmpty field_ =
    case field_ of
        Text _ { value } ->
            String.isEmpty value

        Checkbox _ _ ->
            -- There's no way a checkbox can be empty - we don't use the
            -- indeterminate state, so it's value is either `True` or `False`
            False

        Radio _ _ ->
            False

        File _ { value } ->
            not (RemoteData.isSuccess value)

        Select _ _ ->
            -- TODO
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
