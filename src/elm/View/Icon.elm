module View.Icon exposing (largeView, mediumView, smallView)

import Asset.Icon exposing (accountCircle)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)


view : String -> Maybe String -> String -> Html msg
view baseUrl maybeHash classes =
    case maybeHash of
        Nothing ->
            accountCircle classes

        Just hash ->
            if String.isEmpty hash then
                accountCircle classes

            else
                div
                    [ class classes
                    , style "background-image" ("url(" ++ baseUrl ++ "/" ++ hash ++ ")")
                    ]
                    []


largeView : String -> Maybe String -> String -> Html msg
largeView url maybeHash classes =
    view url maybeHash ("h-24 w-24 rounded-full " ++ classes)


mediumView : String -> Maybe String -> String -> Html msg
mediumView url maybeHash classes =
    view url maybeHash ("h-14 w-14 rounded-full " ++ classes)


smallView : String -> Maybe String -> String -> Html msg
smallView url maybeHash classes =
    view url maybeHash ("h-8 w-8 rounded-full " ++ classes)
