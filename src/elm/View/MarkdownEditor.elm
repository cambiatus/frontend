module View.MarkdownEditor exposing (Model, Msg, init, msgToString, update, view)

import Html exposing (Html, button, div, node, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (on, onClick)
import Json.Decode
import Ports
import Session.Shared exposing (Translators)
import View.Form.Input as Input
import View.Modal as Modal



-- MODEL


type alias Model =
    { linkModalState : LinkModalState
    }


init : Model
init =
    { linkModalState = NotShowing }



-- TYPES


type alias Link =
    { label : String, url : String }


type LinkModalState
    = NotShowing
    | Editing Link


type Msg
    = ClickedIncludeLink Link
    | ClosedLinkModal
    | EnteredLinkLabel String
    | EnteredLinkUrl String
    | ClickedAcceptLink



-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ClickedIncludeLink link ->
            ( { model | linkModalState = Editing link }, Cmd.none )

        ClosedLinkModal ->
            ( { model | linkModalState = NotShowing }, Cmd.none )

        EnteredLinkLabel label ->
            case model.linkModalState of
                Editing editingState ->
                    ( { model | linkModalState = Editing { editingState | label = label } }
                    , Cmd.none
                    )

                NotShowing ->
                    ( model, Cmd.none )

        EnteredLinkUrl url ->
            case model.linkModalState of
                Editing editingState ->
                    ( { model | linkModalState = Editing { editingState | url = url } }
                    , Cmd.none
                    )

                NotShowing ->
                    ( model, Cmd.none )

        ClickedAcceptLink ->
            case model.linkModalState of
                Editing editingState ->
                    ( { model | linkModalState = NotShowing }
                    , Ports.sendMarkdownLink editingState
                    )

                NotShowing ->
                    ( model, Cmd.none )



-- VIEW


view : Translators -> Model -> Html Msg
view translators model =
    -- TODO - I18N
    -- TODO - Pass in options
    div []
        [ node "markdown-editor"
            [ attribute "elm-placeholder" "Placeholder placeholder"
            , on "clicked-include-link" (Json.Decode.map ClickedIncludeLink linkDecoder)
            ]
            []
        , case model.linkModalState of
            Editing linkModal ->
                Modal.initWith
                    { closeMsg = ClosedLinkModal
                    , isVisible = True
                    }
                    |> Modal.withHeader "Add link"
                    |> Modal.withBody
                        [ Input.init
                            { label = "Enter a label"
                            , id = "link-modal-label"
                            , onInput = EnteredLinkLabel
                            , disabled = False
                            , value = linkModal.label
                            , placeholder = Nothing
                            , problems = Nothing
                            , translators = translators
                            }
                            |> Input.toHtml
                        , Input.init
                            { label = "Enter a URL"
                            , id = "link-modal-url"
                            , onInput = EnteredLinkUrl
                            , disabled = False
                            , value = linkModal.url
                            , placeholder = Nothing
                            , problems = Nothing
                            , translators = translators
                            }
                            |> Input.toHtml
                        ]
                    |> Modal.withFooter
                        [ button [ class "modal-cancel", onClick ClosedLinkModal ]
                            [ text "Cancel" ]
                        , button [ class "modal-accept", onClick ClickedAcceptLink ]
                            [ text "Save" ]
                        ]
                    |> Modal.toHtml

            NotShowing ->
                text ""
        ]



-- UTILS


linkDecoder : Json.Decode.Decoder Link
linkDecoder =
    Json.Decode.field "detail"
        (Json.Decode.map2 Link
            (Json.Decode.field "label" Json.Decode.string)
            (Json.Decode.field "url" Json.Decode.string)
        )


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ClickedIncludeLink _ ->
            [ "ClickedIncludeLink" ]

        ClosedLinkModal ->
            [ "ClosedLinkModal" ]

        EnteredLinkLabel _ ->
            [ "EnteredLinkLabel" ]

        EnteredLinkUrl _ ->
            [ "EnteredLinkUrl" ]

        ClickedAcceptLink ->
            [ "ClickedAcceptLink" ]
