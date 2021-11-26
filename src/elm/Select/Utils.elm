module Select.Utils exposing
    ( difference
    , menuItemId
    , stylesToAttrs
    )

import Html
import Html.Attributes exposing (id, style)
import Select.Config exposing (Config)
import Select.Messages exposing (Msg)


menuItemId : Config msg item -> Int -> Html.Attribute (Msg item)
menuItemId config index =
    id (config.inputId ++ "-menu-item-" ++ String.fromInt index)


difference : List item -> List item -> List item
difference listA listB =
    List.filter (\x -> not <| List.any (\y -> x == y) listB) listA


stylesToAttrs : List ( String, String ) -> List (Html.Attribute msg)
stylesToAttrs styles =
    List.map (\( k, v ) -> style k v) styles
