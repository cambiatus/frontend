module View.Modal exposing
    ( initWith
    , toHtml
    , withBody
    , withFooter
    , withHeader
    )

import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Icons



-- OPTIONS


{-| All possible options for the modal dialog.
-}
type alias Options msg =
    { header : Maybe String
    , body : Maybe (Html msg)
    , footer : Maybe (Html msg)
    , isVisible : Bool
    , closeMsg : msg
    }


{-| Minimal options, required to create a modal dialog.
-}
type alias RequiredOptions msg =
    { closeMsg : msg
    , isVisible : Bool
    }



-- MODAL


type Modal msg
    = Modal (Options msg)


{-| Returns full config with all required and optional options.
-}
initWith : RequiredOptions msg -> Modal msg
initWith reqOpts =
    Modal
        { header = Nothing
        , body = Nothing
        , footer = Nothing
        , closeMsg = reqOpts.closeMsg
        , isVisible = reqOpts.isVisible
        }



-- WITH*


withHeader : String -> Modal msg -> Modal msg
withHeader header (Modal options) =
    Modal { options | header = Just header }


withBody : Html msg -> Modal msg -> Modal msg
withBody body (Modal options) =
    Modal { options | body = Just body }


withFooter : Html msg -> Modal msg -> Modal msg
withFooter footer (Modal options) =
    Modal { options | footer = Just footer }



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
            case options.header of
                Just h ->
                    h3 [ class "w-full font-medium text-heading text-2xl mb-2" ]
                        [ text h ]

                Nothing ->
                    text ""

        body =
            case options.body of
                Just b ->
                    b

                Nothing ->
                    text ""

        footer =
            case options.footer of
                Just f ->
                    div [ class "modal-footer" ]
                        [ f ]

                Nothing ->
                    text ""
    in
    div
        [ class "modal container fade-in" ]
        [ div
            [ class "modal-bg"
            , onClick options.closeMsg
            ]
            []
        , div
            [ class "modal-content overflow-auto" ]
            [ button
                [ class "absolute top-0 right-0 mx-4 my-4"
                , onClick options.closeMsg
                ]
                [ Icons.close "text-gray-400 fill-current"
                ]
            , div [ class "display flex flex-col justify-around h-full" ]
                [ header
                , body
                , footer
                ]
            ]
        ]
