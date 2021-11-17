module Book.Form.RichText exposing (chapter)

import ElmBook.Chapter as Chapter exposing (Chapter)



-- CHAPTER


chapter : Chapter x
chapter =
    Chapter.chapter "Rich Text"
        -- TODO - Include component here
        -- TODO - Add custom elements to index.js
        |> Chapter.render """
We have a special kind of text input, which accepts rich text (text with
formatting, such as bold, italics, etc.). Under the covers, we use a
[custom element](https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_custom_elements)
that wraps the [Quill](https://quilljs.com/) rich text editor. They look very
similar to the regular [text fields](/forms/toggle):
"""
