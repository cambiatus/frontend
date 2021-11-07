module Form exposing
    ( Form
    , succeed, with, withOptional
    , textField, checkbox
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

@docs textField, checkbox


## Viewing

@docs view, Model, init, Msg, update, msgToString

-}

import Browser.Dom
import Form.Checkbox as Checkbox
import Form.Text as Text
import Html exposing (Html, button)
import Html.Attributes exposing (class, novalidate, type_)
import Html.Events as Events
import Maybe.Extra
import Session.Shared as Shared
import Set exposing (Set)
import Task
import UpdateResult as UR



-- TYPES


{-| The main type of this module. A `Form` is a function that takes an input
(the dirty model) and returns a bunch of fields and their result. You shouldn't
have to manipulate this type much, and it will be used mostly just to add type
annotations
-}
type alias Form values output =
    GenericForm values output (Msg values output)


{-| Since `Form`s have their own `Msg`, we expose `Form` as the main type, but
this is what does all the work, and can receive any generic msg
-}
type GenericForm values output msg
    = Form (values -> FilledForm values output msg)


{-| A `FilledForm` represents a form with some (dirty) values in it. It also
tries to parse the form, and hols the result as either an error or the
valid/clean model
-}
type alias FilledForm values output msg =
    { fields : List (FilledField values msg)
    , result : Result ( String, List String ) output
    }


{-| We need a way to hold a single field's information, and whether or not it
has an error! This is what we use to do that
-}
type alias FilledField values msg =
    { state : Field values msg
    , error : Maybe String
    , isRequired : Bool
    }


{-| Every field must have at least a value and a way to update the dirty model
with this value. This is used to construct a `Field` given some values.
-}
type alias BaseField value values =
    { value : value
    , update : value -> values
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
type Field values msg
    = Text (Text.Options msg) (BaseField String values)
    | Checkbox (Checkbox.Options msg) (BaseField Bool values)


{-| A generic function to build a generic `Field`. We can use this function to
define more specific field constructors, such as `textField`
-}
field :
    (BaseField input values -> Field values msg)
    -> FieldConfig input output values
    -> GenericForm values output msg
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
    -> Text.Options msg
    -> GenericForm values output msg
textField config options =
    field (Text options) config


{-| An input that represents either `True` or `False`. Checkout `Form.Checkbox`
for more information on what you can do with this field.
-}
checkbox :
    FieldConfig Bool output values
    -> Checkbox.Options msg
    -> GenericForm values output msg
checkbox config options =
    field (Checkbox options) config



-- COMPOSING


{-| Builds a form that always succeeds. Similar to Json.Decode.Pipeline or
Graphql.SelectionSet, this is useful to start a pipeline chain. Give it a
function that transforms a dirty model into a clean one, and build the form
[`with`](#with) other fields.
-}
succeed : output -> GenericForm values output msg
succeed output =
    Form (\_ -> { fields = [], result = Ok output })


{-| Appends a form into another form. Since every field is a form on itself, we
can use this to add the fields we need on a form. This is supposed to be used in
a pipeline, together with `succeed`.
-}
with :
    GenericForm values a msg
    -> GenericForm values (a -> b) msg
    -> GenericForm values b msg
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
    GenericForm values a msg
    -> GenericForm values (Maybe a -> b) msg
    -> GenericForm values b msg
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
    GenericForm values output msg
    -> values
    -> FilledForm values output msg
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
type alias UpdateResult values output =
    UR.UpdateResult (Model values) (Msg values output) output


type Msg values output
    = NoOp
    | ChangedValues values
    | BlurredField String
    | ClickedSubmit (Result ( String, List String ) output)


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
update : Msg values output -> Model values -> UpdateResult values output
update msg (Model model) =
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

        ClickedSubmit (Err ( firstError, _ )) ->
            Model
                { model
                    | errorTracking =
                        ErrorTracking
                            { errorTracking | showAllErrors = True }
                }
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.focus firstError
                        |> Task.attempt (\_ -> NoOp)
                    )

        ClickedSubmit (Ok validForm) ->
            Model model
                |> UR.init
                |> UR.addExt validForm


msgToString : Msg values output -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ChangedValues _ ->
            [ "ChangedValues" ]

        BlurredField _ ->
            [ "BlurredField" ]

        ClickedSubmit r ->
            [ "ClickedSubmit", UR.resultToString r ]



-- VIEW


{-| Provide a form and a dirty model, and get back some HTML
-}
view :
    List (Html.Attribute (Msg values output))
    ->
        { buttonAttrs : List (Html.Attribute (Msg values output))
        , buttonLabel : List (Html (Msg values output))
        , translators : Shared.Translators
        }
    -> GenericForm values output (Msg values output)
    -> Model values
    -> Html (Msg values output)
view formAttrs { buttonAttrs, buttonLabel, translators } form (Model model) =
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
        (Events.onSubmit (ClickedSubmit filledForm.result)
            :: novalidate True
            :: formAttrs
        )
        (fields
            ++ [ button
                    (type_ "submit"
                        :: class "button button-primary"
                        :: buttonAttrs
                    )
                    buttonLabel
               ]
        )


viewField :
    { showError : Bool, translators : Shared.Translators }
    -> FilledField values (Msg values output)
    -> Html (Msg values output)
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


getId : Field values msg -> String
getId state =
    case state of
        Text options _ ->
            Text.getId options

        Checkbox options _ ->
            Checkbox.getId options


isEmpty : Field values msg -> Bool
isEmpty field_ =
    case field_ of
        Text _ { value } ->
            String.isEmpty value

        Checkbox _ _ ->
            -- There's no way a checkbox can be empty - we don't use the
            -- indeterminate state, so it's value is either `True` or `False`
            False
