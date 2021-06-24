module View.Modal exposing
    ( ModalSize(..)
    , initWith
    , toHtml
    , withBody
    , withFooter
    , withHeader
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
import Html.Attributes exposing (class)
import Icons
import Utils exposing (onClickNoBubble)
import View.Components



-- OPTIONS


{-| All possible options for the modal dialog.
-}
type alias Options msg =
    { header : Maybe String
    , body : Maybe (List (Html msg))
    , footer : Maybe (List (Html msg))
    , isVisible : Bool
    , preventScrolling : View.Components.PreventScroll
    , closeMsg : msg
    , size : ModalSize
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
        }



-- WITH*


withHeader : String -> Modal msg -> Modal msg
withHeader header (Modal options) =
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
            h3 [ class "modal-header" ]
                [ text (Maybe.withDefault "" options.header) ]

        body =
            case options.body of
                Just b ->
                    case options.size of
                        Default ->
                            div
                                [ class "modal-body" ]
                                b

                        Large ->
                            div
                                [ class "modal-body-lg" ]
                                b

                        FullScreen ->
                            div
                                [ class "modal-body modal-body-full" ]
                                b

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
                    "modal-content"

                FullScreen ->
                    "modal-content modal-content-full"
    in
    div
        [ class "modal fade-in" ]
        [ View.Components.bgNoScroll
            [ class "modal-bg"
            , onClickNoBubble options.closeMsg
            ]
            options.preventScrolling
        , div
            [ class content
            ]
            [ button
                [ class "absolute top-0 right-0 mx-4 my-4"
                , onClickNoBubble options.closeMsg
                ]
                [ Icons.close "text-gray-400 fill-current"
                ]
            , header
            , body
            , footer
            ]
        ]
