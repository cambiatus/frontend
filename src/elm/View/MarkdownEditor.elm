module View.MarkdownEditor exposing (Msg, msgToString, view)

import Html exposing (Html, div, h1, text, textarea)
import Html.Attributes exposing (class, contenteditable)



-- TYPES


type Msg
    = ClickedBold



-- VIEW


view : Html Msg
view =
    div [ contenteditable True ]
        []



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    Debug.todo "TODO - Add msgs"
