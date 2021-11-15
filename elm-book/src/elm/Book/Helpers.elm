module Book.Helpers exposing (mockTranslators, viewError)

import Html exposing (Html)
import Html.Attributes
import Session.Shared as Shared


mockTranslators : Shared.Translators
mockTranslators =
    { t = identity, tr = \x _ -> x }


viewError : List (Html.Attribute msg) -> Bool -> Maybe String -> Html msg
viewError attributes showError maybeError =
    case maybeError of
        Nothing ->
            Html.text ""

        Just error ->
            if showError then
                Html.p (Html.Attributes.class "form-error" :: attributes)
                    [ Html.text error ]

            else
                Html.text ""
