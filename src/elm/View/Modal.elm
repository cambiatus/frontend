module View.Modal exposing
    ( ModalSize(..)
    , initWith
    , toHtml
    , withAttrs
    , withBody
    , withFooter
    , withHeader
    , withHeaderElement
    , withPreventScrolling
    , withSize
    )

{-| Modal dialog (popup)

To create one you need to init the popup with required options along with optional parameters via `with*` functions
and call `toHtml` at the end of the pipeline:

    Modal.initWith
        -- required options to create a barebone (empty) popup
        { closeMsg = <Msg from the module, where the popup is used>
        , isVisible = True|False
        }

        -- optional params to fill the popup with the actual data
        |> Modal.withHeader "Some header"
        |> Modal.withBody [ div [] [...] ]
        |> Modal.withFooter [ button [] [...] ]

        -- view function to create the actual `Html msg`
        |> Modal.toHtml

-}

import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (class, tabindex)
import Html.Attributes.Aria exposing (ariaHidden)
import Icons
import Utils exposing (onClickNoBubble, onClickPreventAll)
import View.Components



-- OPTIONS


{-| All possible options for the modal dialog.
-}
type alias Options msg =
    { header : Maybe (Html msg)
    , body : Maybe (List (Html msg))
    , footer : Maybe (List (Html msg))
    , isVisible : Bool
    , preventScrolling : View.Components.PreventScroll
    , closeMsg : msg
    , size : ModalSize
    , attrs : List (Html.Attribute msg)
    }


{-| Minimal options required to create a modal dialog.
-}
type alias RequiredOptions msg =
    { closeMsg : msg
    , isVisible : Bool
    }


{-| Sizing type
-}
type ModalSize
    = Large
    | FullScreen
    | Default



-- MODAL


type Modal msg
    = Modal (Options msg)


initWith : RequiredOptions msg -> Modal msg
initWith reqOpts =
    Modal
        { closeMsg = reqOpts.closeMsg
        , isVisible = reqOpts.isVisible
        , preventScrolling = View.Components.PreventScrollAlways
        , header = Nothing
        , body = Nothing
        , footer = Nothing
        , size = Default
        , attrs = []
        }



-- WITH*


withHeader : String -> Modal msg -> Modal msg
withHeader header (Modal options) =
    Modal { options | header = Just (h3 [] [ text header ]) }


withHeaderElement : Html msg -> Modal msg -> Modal msg
withHeaderElement header (Modal options) =
    Modal { options | header = Just header }


withBody : List (Html msg) -> Modal msg -> Modal msg
withBody body (Modal options) =
    Modal { options | body = Just body }


withFooter : List (Html msg) -> Modal msg -> Modal msg
withFooter footer (Modal options) =
    Modal { options | footer = Just footer }


withPreventScrolling : View.Components.PreventScroll -> Modal msg -> Modal msg
withPreventScrolling preventScrolling (Modal options) =
    Modal { options | preventScrolling = preventScrolling }


withSize : ModalSize -> Modal msg -> Modal msg
withSize size (Modal options) =
    Modal { options | size = size }


withAttrs : List (Html.Attribute msg) -> Modal msg -> Modal msg
withAttrs attrs (Modal options) =
    Modal { options | attrs = attrs ++ options.attrs }



-- VIEW


toHtml : Modal msg -> Html msg
toHtml (Modal options) =
    if options.isVisible then
        viewModalDetails options

    else
        text ""


viewModalDetails : Options msg -> Html msg
viewModalDetails options =
    let
        header =
            div [ class "modal-header" ]
                [ case options.header of
                    Just headerElement ->
                        headerElement

                    Nothing ->
                        text ""
                , button
                    [ class "ml-auto text-gray-400 hover:text-red focus:text-red focus:outline-none"
                    , onClickNoBubble options.closeMsg
                    ]
                    [ Icons.close "fill-current"
                    ]
                ]

        body =
            case options.body of
                Just b ->
                    div [ class "modal-body", tabindex -1 ] b

                Nothing ->
                    text ""

        footer =
            case options.footer of
                Just f ->
                    div [ class "modal-footer" ]
                        f

                Nothing ->
                    text ""

        content =
            case options.size of
                Default ->
                    "modal-content"

                Large ->
                    "modal-content modal-content-lg"

                FullScreen ->
                    "modal-content modal-content-full"
    in
    div [ class "modal fade-in" ]
        [ View.Components.bgNoScroll
            [ class "modal-bg"
            , onClickPreventAll options.closeMsg
            ]
            options.preventScrolling
        , View.Components.focusTrap { initialFocusId = Just "modal-first-focus" }
            (class content :: options.attrs)
            [ header
            , button
                [ Html.Attributes.id "modal-first-focus"
                , tabindex -1
                , class "sr-only"
                , ariaHidden True
                ]
                []
            , body
            , footer
            ]
        , View.Components.keyListener
            { acceptedKeys = [ View.Components.Escape ]
            , toMsg = \_ -> options.closeMsg
            , stopPropagation = True
            , preventDefault = False
            }
        ]
