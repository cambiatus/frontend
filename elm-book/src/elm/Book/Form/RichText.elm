module Book.Form.RichText exposing (Model, Msg, chapter, initModel, updateSharedState)

import Book.Helpers
import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (CustomChapter)
import Form.RichText
import Html exposing (div, node)
import Html.Attributes exposing (class)
import Markdown



-- UPDATE


type alias Model =
    Form.RichText.Model


initModel : Model
initModel =
    Form.RichText.initModel "rich-text-example" Nothing


type Msg
    = ChangedModel ( Form.RichText.Model, Cmd Form.RichText.Msg )
    | GotRichTextMsg Form.RichText.Msg
    | NoOp


type alias SharedState x =
    { x | richTextModel : Model }


updateSharedState : Msg -> SharedState x -> ( SharedState x, Cmd Msg )
updateSharedState msg sharedState =
    case msg of
        ChangedModel ( model, subCmd ) ->
            ( { sharedState | richTextModel = model }
            , Cmd.map GotRichTextMsg subCmd
            )

        GotRichTextMsg subMsg ->
            let
                ( newModel, subCmd ) =
                    Form.RichText.update subMsg sharedState.richTextModel
            in
            ( { sharedState | richTextModel = newModel }
            , Cmd.map GotRichTextMsg subCmd
            )

        NoOp ->
            ( sharedState, Cmd.none )



-- CHAPTER


chapter : CustomChapter (SharedState x) Msg
chapter =
    Chapter.chapter "Rich Text"
        |> Chapter.withStatefulComponentList
            [ ( "Live example"
              , \sharedState ->
                    div []
                        [ Form.RichText.init { label = "Rich text" }
                            |> (\options ->
                                    Form.RichText.view options
                                        { onChange = ChangedModel
                                        , onBlur = \_ -> NoOp
                                        , value = sharedState.richTextModel
                                        , error = Html.text ""
                                        , hasError = False
                                        , isRequired = True
                                        , translators = Book.Helpers.mockTranslators
                                        }
                                        GotRichTextMsg
                               )
                        , Markdown.view [ class "mt-10" ]
                            (Form.RichText.getMarkdownContent sharedState.richTextModel)
                        ]
              )
            , ( "Disabled example"
              , \_ ->
                    Form.RichText.init { label = "Disabled rich text input" }
                        |> Form.RichText.withDisabled True
                        |> (\options ->
                                Form.RichText.view options
                                    { onChange = \_ -> NoOp
                                    , onBlur = \_ -> NoOp
                                    , value = Form.RichText.initModel "disabled-rich-text" Nothing
                                    , error = Html.text ""
                                    , hasError = False
                                    , isRequired = True
                                    , translators = Book.Helpers.mockTranslators
                                    }
                                    (\_ -> NoOp)
                           )
              )
            , ( "Placeholder and error"
              , \_ ->
                    Form.RichText.init { label = "Input with placeholder and error" }
                        |> Form.RichText.withPlaceholder "Lorem ipsum dolor sit amet"
                        |> (\options ->
                                Form.RichText.view options
                                    { onChange = \_ -> NoOp
                                    , onBlur = \_ -> NoOp
                                    , value = Form.RichText.initModel "disabled-rich-text" Nothing
                                    , error = Book.Helpers.viewError [] True (Just "Errors are displayed below the input")
                                    , hasError = True
                                    , isRequired = True
                                    , translators = Book.Helpers.mockTranslators
                                    }
                                    (\_ -> NoOp)
                           )
              )
            ]
        |> Chapter.render """
We have a special kind of text input, which accepts rich text (text with
formatting, such as bold, italics, etc.). Under the covers, we use a
[custom element](https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_custom_elements)
that wraps the [Quill](https://quilljs.com/) rich text editor. They look very
similar to the regular [text fields](/forms/text). Down below there's an
interactive example you can play with. Below the input, you can see how the
final text will look like for other users.

<component with-label="Live example" />

Unlike the `Text` input, we don't have as many configuration options for this
input (yet!). For now, we can only have it disabled. We can also add arbitrary
attributes/styling to it, which isn't showcased here.

<component with-label="Disabled example" />

In the next example, we show an input that has a placeholder, and also an error:

<component with-label="Placeholder and error" />
"""
