module View.Select.Internal.Item exposing (view, viewNotFound)

import Html exposing (Html, button, div, li, text)
import Html.Attributes exposing (class, id, style, tabindex, type_)
import Html.Attributes.Aria as Aria
import Html.Events exposing (onClick)
import View.Select.Config exposing (Config)
import View.Select.Messages exposing (Msg(..))
import View.Select.Models exposing (State)
import View.Select.Styles as Styles
import View.Select.Utils exposing (menuItemId)


view : Config msg item -> State -> Int -> Int -> item -> Html (Msg item)
view config state itemCount index item =
    let
        ( highlightedItemClass, highlightedItemStyles ) =
            case state.highlightedItem of
                Nothing ->
                    ( "", [] )

                Just highlighted ->
                    -- take remainder as item numbers wrap around
                    if remainderBy itemCount highlighted == index then
                        ( config.highlightedItemClass, config.highlightedItemStyles )

                    else
                        ( "", [] )

        classes =
            String.join " "
                [ config.itemClass
                , "w-full"
                , highlightedItemClass
                ]

        styles =
            List.concat
                [ Styles.menuItemStyles
                , baseItemStyles config
                , highlightedItemStyles
                ]

        itemHtml =
            case config.itemHtml of
                Nothing ->
                    text (config.toLabel item)

                Just fn ->
                    Html.map (\_ -> NoOp) (fn item)
    in
    li
        [ class "block w-full"
        , Aria.role "option"
        ]
        [ button
            (class classes
                :: onClick (OnSelect item)
                :: tabindex -1
                :: type_ "button"
                :: id (menuItemId config index)
                :: (styles |> List.map (\( f, s ) -> style f s))
            )
            [ itemHtml
            ]
        ]


viewNotFound : Config msg item -> Html (Msg item)
viewNotFound config =
    let
        classes =
            String.join " "
                [ config.itemClass
                , config.notFoundClass
                ]

        styles =
            List.append (baseItemStyles config) config.notFoundStyles
    in
    if config.notFound == "" then
        text ""

    else
        div
            (class classes
                :: (styles |> List.map (\( f, s ) -> style f s))
            )
            [ text config.notFound
            ]


baseItemStyles : Config msg item -> List ( String, String )
baseItemStyles config =
    config.itemStyles
