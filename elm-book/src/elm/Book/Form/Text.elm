module Book.Form.Text exposing (Model, Msg, chapter, initModel)

import Book.Helpers
import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Eos
import Form.Text
import Html exposing (Html)
import Html.Attributes



-- MODEL


type alias Model =
    { basicExample : String
    , textareaExample : String
    , currencyExample : String
    , maskExample : String
    }


initModel : Model
initModel =
    { basicExample = ""
    , textareaExample = ""
    , currencyExample = ""
    , maskExample = ""
    }



-- UPDATE


type Msg
    = NoOp
    | ChangedBasicExample String
    | ChangedTextareaExample String
    | ChangedCurrencyExample String
    | ChangedMaskExample String
    | BlurredField String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ChangedBasicExample basicExample ->
            { model | basicExample = basicExample }

        ChangedTextareaExample textareaExample ->
            { model | textareaExample = textareaExample }

        ChangedCurrencyExample currencyExample ->
            { model | currencyExample = currencyExample }

        ChangedMaskExample maskExample ->
            { model | maskExample = maskExample }

        BlurredField _ ->
            model


updateSharedState : Msg -> { x | textModel : Model } -> { x | textModel : Model }
updateSharedState msg sharedState =
    { sharedState | textModel = update msg sharedState.textModel }



-- VIEW


viewBasic : Model -> Html Msg
viewBasic model =
    let
        options =
            Form.Text.init
                { id = "basic-example"
                , label = "Name"
                }
                |> Form.Text.withPlaceholder "Some placeholder text"
    in
    Form.Text.view options
        { onChange = ChangedBasicExample
        , onBlur = BlurredField
        , value = model.basicExample
        , error = Html.text ""
        , hasError = False
        , translators = Book.Helpers.mockTranslators
        , isRequired = True
        }


viewTextarea : Model -> Html Msg
viewTextarea model =
    let
        options =
            Form.Text.init
                { id = "textarea-example"
                , label = "Bio"
                }
                |> Form.Text.withInputElement (Form.Text.TextareaInput { submitOnEnter = False })
                |> Form.Text.withPlaceholder "Enter up to 200 characters"
                |> Form.Text.withCounter (Form.Text.CountLetters 200)
    in
    Form.Text.view options
        { onChange = ChangedBasicExample
        , onBlur = BlurredField
        , value = model.basicExample
        , error = Html.text ""
        , hasError = False
        , translators = Book.Helpers.mockTranslators
        , isRequired = True
        }


viewWithCurrency : Model -> Html Msg
viewWithCurrency model =
    let
        options =
            Form.Text.init
                { id = "currency-example"
                , label = "Enter an amount"
                }
                |> Form.Text.withCurrency Eos.cambiatusSymbol
    in
    Form.Text.view options
        { onChange = ChangedCurrencyExample
        , onBlur = BlurredField
        , value = model.currencyExample
        , error = Html.text ""
        , hasError = False
        , translators = Book.Helpers.mockTranslators
        , isRequired = True
        }


viewWithMask : Model -> Html Msg
viewWithMask model =
    let
        options =
            Form.Text.init
                { id = "currency-example"
                , label = "Enter a brazilian phone number"
                }
                |> Form.Text.withMask { mask = "(##) ##### ####", replace = '#' }
                |> Form.Text.withPlaceholder "(##) ##### ####"
    in
    Form.Text.view options
        { onChange = ChangedMaskExample
        , onBlur = BlurredField
        , value = model.maskExample
        , error = Html.text ""
        , hasError = False
        , translators = Book.Helpers.mockTranslators
        , isRequired = True
        }


viewSimple : Form.Text.Options (ElmBook.Msg state) -> Html (ElmBook.Msg state)
viewSimple options =
    Form.Text.view options
        { onChange = Actions.logActionWithString "Input"
        , onBlur = Actions.logActionWithString "Blurred field"
        , value = ""
        , error = Html.text ""
        , hasError = False
        , translators = Book.Helpers.mockTranslators
        , isRequired = True
        }


viewSimpleWithError : String -> Form.Text.Options (ElmBook.Msg state) -> Html (ElmBook.Msg state)
viewSimpleWithError error options =
    Form.Text.view options
        { onChange = Actions.logActionWithString "Input"
        , onBlur = Actions.logActionWithString "Blurred field"
        , value = ""
        , error =
            Html.p [ Html.Attributes.class "form-error" ]
                [ Html.text error ]
        , hasError = True
        , translators = Book.Helpers.mockTranslators
        , isRequired = True
        }



-- CHAPTER


chapter : Chapter { x | textModel : Model }
chapter =
    Chapter.chapter "Text"
        |> Chapter.withComponentList
            [ ( "Disabled"
              , Form.Text.init
                    { id = "disabled-example"
                    , label = "Disabled input"
                    }
                    |> Form.Text.withDisabled True
                    |> viewSimple
              )
            , ( "With error"
              , Form.Text.init
                    { id = "with-error-example"
                    , label = "Input with error"
                    }
                    |> viewSimpleWithError "Something wrong happened"
              )
            ]
        |> Chapter.withStatefulComponentList
            [ ( "Basic"
              , \{ textModel } ->
                    viewBasic textModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            , ( "Text area"
              , \{ textModel } ->
                    viewTextarea textModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            , ( "With currency"
              , \{ textModel } ->
                    viewWithCurrency textModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            , ( "With mask"
              , \{ textModel } ->
                    viewWithMask textModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            ]
        |> Chapter.render """
This is by far the most used and most configurable input of them all! It can be
used to receive simple String inputs, numeric values, time values, and more. This
component is especially useful with the [Validation API](/forms/validation) to turn
`String` values into whatever you want.

This is the most basic form of this component:

<component with-label="Basic" />

Notice it always has a label (`NAME` in this case), enforcing good practices. It
can also optionally have a placeholder, an input mask, extra elements, word or letter
counters, and more.

This same component can be rendered as a `textarea` for longer texts:

<component with-label="Text area" />

But always think if you shouldn't use the [RichText editor](/forms/rich-text) instead.

Some important states are the disabled state and an errored state:

<component with-label="Disabled" />
<component with-label="With error" />

Try typing into the two following inputs, and see how the input mask works! The
currency input also shows an example of an extra element displayed on top of the
input.

<component with-label="With currency" />
<component with-label="With mask" />
"""
