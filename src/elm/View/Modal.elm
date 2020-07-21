module View.Modal exposing
    ( Visibility
    , hidden
    , newConfig
    , shown
    , view
    , withBody
    , withFooter
    , withHeader
    )

import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, stopPropagationOn)
import Icons
import Json.Decode as Decode


{-| Represents minimal (empty) modal dialog.
-}
type alias RequiredConfig msg =
    { closeMsg : msg
    , noOpMsg : msg
    , visibility : Visibility
    }


{-| Represents all possible options for the modal dialog.
-}
type alias Config msg =
    { header : Maybe String
    , body : Maybe (Html msg)
    , footer : Maybe (Html msg)
    , visibility : Visibility
    , closeMsg : msg
    , ignoreMsg : msg
    }


{-| Returns full config with all required and optional fields
-}
newConfig : RequiredConfig msg -> Config msg
newConfig required =
    { header = Nothing
    , body = Nothing
    , footer = Nothing
    , closeMsg = required.closeMsg
    , ignoreMsg = required.noOpMsg
    , visibility = required.visibility
    }


type Visibility
    = Hidden
    | Shown


hidden : Visibility
hidden =
    Hidden


shown : Visibility
shown =
    Shown


withHeader : String -> Config msg -> Config msg
withHeader header cfg =
    { cfg | header = Just header }


withBody : Html msg -> Config msg -> Config msg
withBody body cfg =
    { cfg | body = Just body }


withFooter : Html msg -> Config msg -> Config msg
withFooter footer cfg =
    { cfg | footer = Just footer }


view : Config msg -> Html msg
view cfg =
    case cfg.visibility of
        Shown ->
            let
                header =
                    case cfg.header of
                        Just h ->
                            h3 [ class "w-full font-medium text-heading text-2xl mb-2" ]
                                [ text h ]

                        Nothing ->
                            text ""

                body =
                    case cfg.body of
                        Just b ->
                            div []
                                [ b ]

                        Nothing ->
                            text ""

                footer =
                    case cfg.footer of
                        Just f ->
                            div [ class "modal-footer" ]
                                [ f ]

                        Nothing ->
                            text ""
            in
            div
                [ class "modal container fade-in"
                , stopPropagationOn "click" (Decode.succeed ( cfg.ignoreMsg, True ))
                ]
                [ div
                    [ class "modal-bg"
                    , onClick cfg.closeMsg
                    ]
                    []
                , div
                    [ class "modal-content overflow-auto" ]
                    [ button
                        [ class "absolute top-0 right-0 mx-4 my-4"
                        , onClick cfg.closeMsg
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

        Hidden ->
            text ""
