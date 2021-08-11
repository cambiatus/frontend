module View.MarkdownEditor exposing (Model, Msg, init, msgToString, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Html exposing (Html, button, div, node, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (on, onClick)
import Json.Decode
import Ports
import Session.Shared exposing (Translators)
import Task
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
    = Ignored
    | KeyDown String
    | ClickedIncludeLink Link
    | ClosedLinkModal
    | EnteredLinkLabel String
    | EnteredLinkUrl String
    | ClickedAcceptLink



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignored ->
            ( model, Cmd.none )

        KeyDown key ->
            if key == "Enter" then
                update ClickedAcceptLink model

            else
                ( model, Cmd.none )

        ClickedIncludeLink link ->
            ( { model | linkModalState = Editing link }
            , Browser.Dom.focus "link-modal-label"
                |> Task.attempt (\_ -> Ignored)
            )

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


view : Translators -> Maybe String -> Model -> Html Msg
view ({ t } as translators) placeholder model =
    div []
        [ node "markdown-editor"
            [ attribute "elm-placeholder" (Maybe.withDefault "" placeholder)
            , attribute "elm-edit-text" (t "markdown.link_tooltip.edit")
            , attribute "elm-remove-text" (t "markdown.link_tooltip.remove")
            , on "clicked-include-link" (Json.Decode.map ClickedIncludeLink linkDecoder)
            ]
            []
        , case model.linkModalState of
            Editing linkModal ->
                Modal.initWith
                    { closeMsg = ClosedLinkModal
                    , isVisible = True
                    }
                    |> Modal.withHeader (t "markdown.link_form.header")
                    |> Modal.withBody
                        [ Input.init
                            { label = t "markdown.link_form.label"
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
                            { label = t "markdown.link_form.url"
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
                            [ text <| t "menu.cancel" ]
                        , button [ class "modal-accept", onClick ClickedAcceptLink ]
                            [ text <| t "menu.save" ]
                        ]
                    |> Modal.toHtml

            NotShowing ->
                text ""
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.linkModalState of
        Editing _ ->
            Sub.map KeyDown (Browser.Events.onKeyDown (Json.Decode.field "key" Json.Decode.string))

        NotShowing ->
            Sub.none



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
        Ignored ->
            [ "Ignored" ]

        KeyDown _ ->
            [ "KeyDown" ]

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
