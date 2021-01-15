module Page.NotFound exposing (view)

import Html exposing (Html)
import Page


view : Page.Session -> { title : String, content : Html msg }
view session =
    let
        shared =
            Page.toShared session
    in
    { title =
        shared.translators.t "error.pageNotFound"
    , content =
        Page.fullPageNotFound (shared.translators.t "error.pageNotFound") ""
    }
