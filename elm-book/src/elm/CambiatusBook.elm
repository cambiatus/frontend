module CambiatusBook exposing (main)

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
            [ ( "Form elements"
              , [ testChapter
                ]
              )
            ]


testChapter : Chapter x
testChapter =
    Chapter.chapter "Test component"
        |> Chapter.withComponentList
            [ ( "Normal", Html.button [] [ Html.text "Normal" ] )
            , ( "Disabled", Html.button [ Html.Attributes.disabled True ] [ Html.text "Disabled" ] )
            ]
        |> Chapter.render """
This is a test. Try clicking these buttons:

<component with-label="Normal" />

Also, a disabled one:

<component with-label="Disabled" />
"""
