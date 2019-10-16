module Page.ComingSoon exposing (view)

import Html exposing (..)
import I18Next exposing (Delims(..), t, tr)
import Page


view : Page.Session -> Html msg
view session =
    let
        shared =
            Page.toShared session
    in
    div []
        [ Page.viewTitle (t shared.translations "menu.coming_soon")
        , text (t shared.translations "menu.coming_soon")
        ]
