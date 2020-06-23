module Page.ComingSoon exposing (view)

import Html exposing (Html, div, text)
import I18Next exposing (t)
import Page


view : Page.Session -> { title : String, content : Html msg }
view session =
    let
        shared =
            Page.toShared session
    in
    { title =
        t shared.translations "menu.coming_soon"
    , content =
        div []
            [ Page.viewTitle (t shared.translations "menu.coming_soon")
            , text (t shared.translations "menu.coming_soon")
            ]
    }
