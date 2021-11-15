module CambiatusBook exposing (main)

import Book.Form
import ElmBook as Book exposing (Book)
import ElmBook.Chapter as Chapter exposing (Chapter)
import ElmBook.ThemeOptions as ThemeOptions
import Html
import Html.Attributes


main : Book ()
main =
    Book.book "Cambiatus"
        |> Book.withThemeOptions
            [ ThemeOptions.logo <| Html.img [ Html.Attributes.src "/images/cambiatus-logo-mobile.svg" ] []
            , ThemeOptions.backgroundGradient "#5859a1" "#45469B"
            , ThemeOptions.navAccent "white"
            ]
        |> Book.withChapterGroups
            [ ( "Forms"
              , [ Book.Form.introduction
                ]
              )
            ]
