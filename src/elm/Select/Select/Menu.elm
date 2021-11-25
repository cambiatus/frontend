module Select.Select.Menu exposing (menu, view, viewStyles)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode
import Select.Config exposing (Config)
import Select.Messages as Msg exposing (Msg(..))
import Select.Models exposing (State)
import Select.Search as Search
import Select.Select.Item as Item
import Select.Styles as Styles
import View.Components


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

        focusoutDecoder =
            Json.Decode.at [ "target", "id" ] Json.Decode.string
                |> Json.Decode.andThen
                    (\targetId ->
                        if String.startsWith (config.inputId ++ "-menu-item-") targetId then
                            case model.highlightedItem of
                                Nothing ->
                                    Json.Decode.fail "Focus is not leaving menu container"

                                Just highlightedItem ->
                                    if String.endsWith (String.fromInt highlightedItem) targetId then
                                        Json.Decode.succeed Msg.OnBlur

                                    else
                                        Json.Decode.fail "Focus is not leaving menu container"

                        else
                            Json.Decode.succeed Msg.OnBlur
                    )

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
        div
            (class config.menuClass
                :: Html.Events.on "focusout" focusoutDecoder
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
