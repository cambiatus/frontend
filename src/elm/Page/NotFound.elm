module Page.NotFound exposing (view)

import Browser exposing (Document)
import Html exposing (Html)
import I18Next exposing (t)
import Page


view : Page.Session -> Document msg
view session =
    let
        shared =
            Page.toShared session

        text_ =
            t shared.translations
    in
    Document
        (t shared.translations "error.pageNotFound")
        [ Page.fullPageNotFound (text_ "error.pageNotFound") "" ]
