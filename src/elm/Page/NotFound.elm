module Page.NotFound exposing (view)

import Html exposing (Html)
import I18Next exposing (t)
import Page


view : Page.Session -> { title : String, content : Html msg }
view session =
    let
        shared =
            Page.toShared session

        text_ =
            t shared.translations
    in
    { title =
        t shared.translations "error.pageNotFound"
    , content =
        Page.fullPageNotFound (text_ "error.pageNotFound") ""
    }
