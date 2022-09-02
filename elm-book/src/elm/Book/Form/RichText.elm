module Book.Form.RichText exposing (Model, Msg, chapter, initModel)

import Book.Helpers
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Form.RichText
import Html exposing (div)
import Html.Attributes exposing (class)
import Markdown



-- MODEL


type alias Model =
    Form.RichText.Model


initModel : Model
initModel =
    Form.RichText.initModel "rich-text-example" Nothing



-- UPDATE


type Msg
    = GotRichTextMsg Form.RichText.Msg
    | NoOp


type alias SharedState x =
    { x | richTextModel : Model }


update : Msg -> Form.RichText.Model -> ( Form.RichText.Model, Cmd Msg )
update msg richtextModel =
    case msg of
        GotRichTextMsg subMsg ->
            Form.RichText.update subMsg richtextModel
                |> Tuple.mapSecond (Cmd.map GotRichTextMsg)

        NoOp ->
            ( richtextModel, Cmd.none )



-- CHAPTER


chapter : Chapter (SharedState x)
chapter =
    let
        mapIntoNoOp =
            Html.map
                (Actions.mapUpdate
                    { fromState = .richTextModel
                    , toState = \shared model -> { shared | richTextModel = model }
                    , update = \_ model -> model
                    }
                )
    in
    Chapter.chapter "Rich Text"
        |> Chapter.withStatefulComponentList
            [ ( "Live example"
              , \sharedState ->
                    div []
                        [ Form.RichText.init { label = "Rich text" }
                            |> (\options ->
                                    Form.RichText.view options
                                        { onBlur = NoOp
                                        , value = sharedState.richTextModel
                                        , error = Html.text ""
                                        , hasError = False
                                        , isRequired = True
                                        , translators = Book.Helpers.mockTranslators
                                        }
                                        GotRichTextMsg
                                        |> Html.map
                                            (Actions.mapUpdateWithCmd
                                                { fromState = .richTextModel
                                                , toState =
                                                    \shared model ->
                                                        { shared | richTextModel = model }
                                                , update = update
                                                }
                                            )
                               )
                        , Markdown.view [ class "mt-10" ]
                            (Form.RichText.getMarkdownContent sharedState.richTextModel)
                        ]
              )
            ]
        |> Chapter.withComponentList
            [ ( "Disabled example"
              , Form.RichText.init
                    { label = "Disabled rich text input" }
                    |> Form.RichText.withDisabled True
                    |> (\options ->
                            Form.RichText.view options
                                { onBlur = NoOp
                                , value = Form.RichText.initModel "disabled-rich-text" Nothing
                                , error = Html.text ""
                                , hasError = False
                                , isRequired = True
                                , translators = Book.Helpers.mockTranslators
                                }
                                (\_ -> NoOp)
                       )
                    |> mapIntoNoOp
              )
            , ( "Placeholder and error"
              , Form.RichText.init
                    { label = "Input with placeholder and error" }
                    |> Form.RichText.withPlaceholder "Lorem ipsum dolor sit amet"
                    |> (\options ->
                            Form.RichText.view options
                                { onBlur = NoOp
                                , value = Form.RichText.initModel "error-rich-text" Nothing
                                , error = Book.Helpers.viewError [] True (Just "Errors are displayed below the input")
                                , hasError = True
                                , isRequired = True
                                , translators = Book.Helpers.mockTranslators
                                }
                                (\_ -> NoOp)
                       )
                    |> mapIntoNoOp
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
