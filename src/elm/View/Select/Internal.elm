module View.Select.Internal exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, id, style)
import Html.Attributes.Aria exposing (ariaExpanded)
import View.Select.Config exposing (Config)
import View.Select.Internal.Input
import View.Select.Internal.Menu
import View.Select.Messages exposing (Msg)
import View.Select.Models exposing (State)


view : Config msg item -> State -> List item -> List item -> Html (Msg item)
view config model availableItems selectedItems =
    let
        classes =
            "elm-select"
    in
    div
        [ id model.id
        , class classes
        , style "position" "relative"
        , style "width" "100%"
        , ariaExpanded
            (if model.showMenu then
                "true"

             else
                "false"
            )
        ]
        [ View.Select.Internal.Input.view config model availableItems selectedItems
        , View.Select.Internal.Menu.view config model availableItems selectedItems
        ]
