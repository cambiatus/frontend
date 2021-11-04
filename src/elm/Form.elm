module Form exposing
    ( Form
    , succeed, with
    , textField
    , view
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
        Form.Text.init
            { label = "Name"
            , id = "name-input"
            , disabled = False
            }
            |> Form.Text.withPlaceholder "Name"
            |> Form.textField
                { parser = \dirtyName -> Ok dirtyName
                , value = \dirtyUser -> dirtyUser.name
                , update = \name dirtyUser -> { dirtyUser | name = name }
                , error = \dirtyUser -> Nothing
                }

    ageField : Form DirtyUser Int msg
    ageField =
        Form.Text.init
            { label = "Age"
            , id = "age-input"
            , disabled = False
            }
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

@docs succeed, with


## Fields

@docs textField


## Viewing

@docs view

-}

import Form.Text as Text
import Html exposing (Html)



-- TYPES


{-| The main type of this module. A `Form` is a function that takes an input
(the dirty model) and returns a bunch of fields and their result. You shouldn't
have to manipulate this type much, and it will be used mostly just to add type
annotations
-}
type Form values output msg
    = Form (values -> FilledForm values output msg)


{-| A `FilledForm` represents a form with some (dirty) values in it. It also
tries to parse the form, and hols the result as either an error or the
valid/clean model
-}
type alias FilledForm values output msg =
    { fields : List (FilledField values msg)
    , result : Result String output
    }


{-| We need a way to hold a single field's information, and whether or not it
has an error! This is what we use to do that
-}
type alias FilledField values msg =
    { state : Field values msg
    , error : Maybe String
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
    , error : values -> Maybe String
    }



-- FIELDS


{-| All the possible field types, such as `Text`, `RichText` and `Radio`. Use
the corresponding function (i.e. `textField` for `Text`, etc) to build fields
with these types
-}
type Field values msg
    = Text (Text.Options msg) (BaseField String values)


{-| A generic function to build a generic `Field`. We can use this function to
define more specific field constructors, such as `textField`
-}
field :
    (BaseField input values -> Field values msg)
    -> FieldConfig input output values
    -> Form values output msg
field build config =
    let
        parse values =
            config.parser (config.value values)
                |> Result.andThen
                    (\output ->
                        config.error values
                            |> Maybe.map (\error -> Err error)
                            |> Maybe.withDefault (Ok output)
                    )

        field_ values =
            let
                value =
                    config.value values

                update newValue =
                    config.update newValue values
            in
            build
                { value = value
                , update = update
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
                  }
                ]
            , result = result
            }
        )


{-| An input that receives text. Checkout `Form.Text` for more information on
what you can do with this field.
-}
textField :
    FieldConfig String output values
    -> Text.Options msg
    -> Form values output msg
textField config options =
    field (Text options) config



-- COMPOSING


{-| Builds a form that always succeeds. Similar to Json.Decode.Pipeline or
Graphql.SelectionSet, this is useful to start a pipeline chain. Give it a
function that transforms a dirty model into a clean one, and build the form
[`with`](#with) other fields.
-}
succeed : output -> Form values output msg
succeed output =
    Form (\_ -> { fields = [], result = Ok output })


{-| Appends a form into another form. Since every field is a form on itself, we
can use this to add the fields we need on a form. This is supposed to be used in
a pipeline, together with `succeed`.
-}
with :
    Form values a msg
    -> Form values (a -> b) msg
    -> Form values b msg
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

                    Err currErr ->
                        case filledNew.result of
                            Ok _ ->
                                Err currErr

                            Err newErr ->
                                Err newErr
            }
        )


{-| Given some values (a dirty model), fill a form with them.
-}
fill :
    Form values output msg
    -> values
    -> FilledForm values output msg
fill (Form fill_) =
    fill_



-- VIEW


{-| Provide a form and a dirty model, and get back some HTML
-}
view : Form values output msg -> values -> (values -> msg) -> Html msg
view form values inputMsg =
    fill form values
        |> .fields
        |> List.map (viewField inputMsg)
        |> Html.form []


viewField : (values -> msg) -> FilledField values msg -> Html msg
viewField inputMsg { state, error } =
    case state of
        Text options { value, update } ->
            Text.view options
                { onChange = update >> inputMsg
                , value = value
                , error = error
                }
