module CambiatusBook exposing (main)

import Book.Form
import Book.Form.Checkbox
import Book.Form.DatePicker
import Book.Form.File
import Book.Form.Radio
import Book.Form.RichText
import Book.Form.Select
import Book.Form.Text
import Book.Form.Toggle
import Book.Form.UserPicker
import ElmBook as Book exposing (Book)
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import ElmBook.ChapterOptions as ChapterOptions
import ElmBook.StatefulOptions as StatefulOptions
import ElmBook.ThemeOptions as ThemeOptions
import Html
import Html.Attributes
import Html.Events


main : Book SharedState
main =
    Book.book "Cambiatus"
        |> Book.withThemeOptions
            [ ThemeOptions.logo <| Html.img [ Html.Attributes.src "/images/cambiatus-logo-mobile.svg" ] []
            , ThemeOptions.background "#5859a1"
            , ThemeOptions.navAccent "white"
            ]
        |> Book.withStatefulOptions
            [ StatefulOptions.initialState initialState
            ]
        |> Book.withChapterGroups
            [ ( "", [ introduction ] )
            , ( "Forms"
              , Book.Form.chapters
              )
            ]


type alias SharedState =
    { toggleModel : Book.Form.Toggle.Model
    , textModel : Book.Form.Text.Model
    , richTextModel : Book.Form.RichText.Model
    , datepickerModel : Book.Form.DatePicker.Model
    , userpickerModel : Book.Form.UserPicker.Model
    , selectModel : Book.Form.Select.Model
    , checkboxModel : Book.Form.Checkbox.Model
    , radioModel : Book.Form.Radio.Model
    , fileModel : Book.Form.File.Model
    }


initialState : SharedState
initialState =
    { toggleModel = Book.Form.Toggle.initModel
    , textModel = Book.Form.Text.initModel
    , richTextModel = Book.Form.RichText.initModel
    , datepickerModel = Book.Form.DatePicker.initModel
    , userpickerModel = Book.Form.UserPicker.initModel
    , selectModel = Book.Form.Select.initModel
    , checkboxModel = Book.Form.Checkbox.initModel
    , radioModel = Book.Form.Radio.initModel
    , fileModel = Book.Form.File.initModel
    }


introduction : Chapter x
introduction =
    Chapter.chapter "Overview"
        |> Chapter.withChapterOptions [ ChapterOptions.hiddenTitle True ]
        |> Chapter.withComponentList
            [ ( "Test component"
              , Html.button
                    [ Html.Attributes.class "button button-primary"
                    , Html.Events.onClick (Actions.logAction "Nice, you clicked the button!")
                    ]
                    [ Html.text "Click me!" ]
              )
            ]
        |> Chapter.render """
# Hello, and welcome to the Cambiatus Elm-book! 👋

Here you will find documentation about our components, and you will be able to
test them in an isolated environment. If you somehow got here without knowing
who we are, you can explore more about what we do on [our website](https://cambiatus.com),
see our source code on [our GitHub organization](https://github.com/cambiatus),
or [see our app live](https://www.cambiatus.com/welcome). You can also check out
[our wiki](https://cambiatus.github.io/) for more advanced information on how our
app works, and some more information about us.

This book contains several chapters (like this one!), and each chapter is made
up of some text and components. You know what text looks like, so here's how a
component looks like:

<component with-label="Test component" />

Notice it has a label (`Test component` in this case) above it, and it's inside
a padded white box. When you interact with a component, it might tell you what
is going on on the box at the very bottom of the page (the one that says
"Your logged actions will appear here."). Go ahead and click the button to see
how it works!

If you click on that box at the bottom of the page, you can see everything that
happened while you've been browsing the chapter you're in.

Notice that not all components log their actions - some of them actually do
stuff, like changing their own internal state. In that case, it should be
visually apparent that something happened.

**Note for testers:** Please make sure every component is keyboard-accessible
and screen-reader-friendly. We want to be accessible to as many people as
possible!
"""
