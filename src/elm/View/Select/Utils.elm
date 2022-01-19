module View.Select.Utils exposing
    ( difference
    , menuItemId
    , stylesToAttrs
    )

import Html
import Html.Attributes exposing (style)
import View.Select.Config exposing (Config)


menuItemId : Config msg item -> Int -> String
menuItemId config index =
    config.inputId ++ "-menu-item-" ++ String.fromInt index


difference : List item -> List item -> List item
difference listA listB =
    List.filter (\x -> not <| List.any (\y -> x == y) listB) listA


stylesToAttrs : List ( String, String ) -> List (Html.Attribute msg)
stylesToAttrs styles =
    List.map (\( k, v ) -> style k v) styles
