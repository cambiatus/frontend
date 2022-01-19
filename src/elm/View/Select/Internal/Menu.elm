module View.Select.Internal.Menu exposing (view)

import Html exposing (Html, text, ul)
import Html.Attributes exposing (class, classList, id, style)
import View.Components
import View.Select.Config exposing (Config)
import View.Select.Events
import View.Select.Internal.Item as Item
import View.Select.Messages as Msg exposing (Msg)
import View.Select.Models exposing (State)
import View.Select.Search as Search
import View.Select.Styles as Styles


view : Config msg item -> State -> List item -> List item -> Html (Msg item)
view config model availableItems selectedItems =
    let
        searchResult =
            Search.matchedItemsWithCutoff
                config
                model.query
                availableItems
                selectedItems
    in
    case searchResult of
        Nothing ->
            text ""

        Just matchedItems ->
            menu config model matchedItems


menu : Config msg item -> State -> List item -> Html (Msg item)
menu config model matchedItems =
    let
        hideWhenNotFound =
            config.notFoundShown == False && matchedItems == []

        menuStyle =
            if hideWhenNotFound then
                Styles.hiddenMenuStyles |> List.map (\( f, s ) -> style f s)

            else
                viewStyles config |> List.map (\( f, s ) -> style f s)

        noResultElement =
            if matchedItems == [] then
                Item.viewNotFound config

            else
                text ""

        itemCount =
            List.length matchedItems

        elements =
            matchedItems
                |> List.indexedMap (Item.view config model itemCount)
    in
    if model.showMenu then
        ul
            (class config.menuClass
                :: classList [ ( config.emptyMenuClass, itemCount == 0 ) ]
                :: View.Select.Events.onBlurAttribute config model
                :: id (model.id ++ "-items-list")
                :: menuStyle
            )
            (noResultElement :: keyListener model itemCount :: elements)

    else
        text ""


keyListener : State -> Int -> Html (Msg item)
keyListener model itemCount =
    View.Components.keyListener
        { acceptedKeys =
            [ View.Components.Escape
            , View.Components.ArrowUp
            , View.Components.ArrowDown
            ]
        , toMsg =
            \key ->
                case key of
                    View.Components.Escape ->
                        Msg.OnEsc

                    View.Components.ArrowUp ->
                        Msg.OnUpArrow

                    View.Components.ArrowDown ->
                        case model.highlightedItem of
                            Nothing ->
                                Msg.OnDownArrow

                            Just highlightedItem ->
                                if highlightedItem >= itemCount - 1 then
                                    Msg.OnResetFocusToFirstItem

                                else
                                    Msg.OnDownArrow

                    _ ->
                        Msg.OnDownArrow
        , stopPropagation = False
        , preventDefault = True
        }


viewStyles : Config msg item -> List ( String, String )
viewStyles config =
    List.append Styles.visibleMenuStyles config.menuStyles
