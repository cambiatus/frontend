module Page.ComingSoon exposing (view)

import Html exposing (Html, div, text)
import Page


view : Page.Session -> { title : String, content : Html msg }
view session =
    let
        shared =
            Page.toShared session
    in
    { title =
        shared.translators.t "menu.coming_soon"
    , content =
        div []
            [ Page.viewTitle (shared.translators.t "menu.coming_soon")
            , text (shared.translators.t "menu.coming_soon")
            ]
    }
