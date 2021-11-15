module CambiatusBook exposing (main)

import Book.Form
import Book.Form.Toggle
import ElmBook as Book exposing (Book)
import ElmBook.Chapter as Chapter exposing (Chapter)
import ElmBook.StatefulOptions as StatefulOptions
import ElmBook.ThemeOptions as ThemeOptions
import Html
import Html.Attributes


main : Book SharedState
main =
    Book.book "Cambiatus"
        |> Book.withThemeOptions
            [ ThemeOptions.logo <| Html.img [ Html.Attributes.src "/images/cambiatus-logo-mobile.svg" ] []
            , ThemeOptions.backgroundGradient "#5859a1" "#45469B"
            , ThemeOptions.navAccent "white"
            ]
        |> Book.withStatefulOptions
            [ StatefulOptions.initialState initialState
            ]
        |> Book.withChapterGroups
            [ ( "Forms"
              , Book.Form.chapters
              )
            ]


type alias SharedState =
    { toggleModel : Book.Form.Toggle.Model
    }


initialState : SharedState
initialState =
    { toggleModel = Book.Form.Toggle.initModel
    }
