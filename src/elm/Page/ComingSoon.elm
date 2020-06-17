module Page.ComingSoon exposing (view)

import Browser exposing (Document)
import Html exposing (text)
import I18Next exposing (t)
import Page


view : Page.Session -> Document msg
view session =
    let
        shared =
            Page.toShared session
    in
    Document
        (t shared.translations "menu.coming_soon")
        [ Page.viewTitle (t shared.translations "menu.coming_soon")
        , text (t shared.translations "menu.coming_soon")
        ]
