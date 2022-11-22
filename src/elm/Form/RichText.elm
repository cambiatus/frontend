module Form.RichText exposing
    ( init, Options
    , withDisabled, withPlaceholder, withContainerAttrs, withEditorContainerAttrs
    , getId, getMarkdownContent
    , view
    , Model, initModel, update, Msg, msgToString
    )

{-| Creates a Cambiatus-style RichText. Use it within a `Form.Form`:

    Form.RichText.init { label = "Memo (optional)" }


# Initializing

@docs init, Options


# Helpers


## Adding attributes

@docs withDisabled, withPlaceholder, withContainerAttrs, withEditorContainerAttrs


# Getters

@docs getId, getMarkdownContent


# View

@docs view


# The elm architecture

This is how you actually use this component!

@docs Model, initModel, update, Msg, msgToString

-}

import Form.Text
import Html exposing (Html, button, div)
import Html.Attributes exposing (class)
import Html.Events
import Json.Decode
import Json.Decode.Pipeline as Decode
import Json.Encode
import Markdown exposing (Markdown, QuillOp)
import Ports
import Session.Shared as Shared
import Utils
import View.Components
import View.Modal as Modal



-- OPTIONS


type Options msg
    = Options
        { label : String
        , disabled : Bool
        , placeholder : Maybe String
        , containerAttrs : List (Html.Attribute msg)
        , editorContainerAttrs : List (Html.Attribute msg)
        }


{-| Initializes a RichText
-}
init : { label : String } -> Options msg
init { label } =
    Options
        { label = label
        , disabled = False
        , placeholder = Nothing
        , containerAttrs = []
        , editorContainerAttrs = []
        }



-- ADDING ATTRIBUTES


{-| Determines if the RichText should be disabled
-}
withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


{-| Adds a placeholder
-}
withPlaceholder : String -> Options msg -> Options msg
withPlaceholder placeholder (Options options) =
    Options { options | placeholder = Just placeholder }


{-| Adds attributes to the element that contains the editor and the link modal
-}
withContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }


{-| Adds attributes to the element that contains the label, the editor itself,
and the error message
-}
withEditorContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withEditorContainerAttrs attrs (Options options) =
    Options { options | editorContainerAttrs = options.editorContainerAttrs ++ attrs }



-- VIEW


type alias ViewConfig msg =
    { onBlur : msg
    , value : Model
    , error : Html msg
    , hasError : Bool
    , isRequired : Bool
    , translators : Shared.Translators
    }


view : Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
view (Options options) viewConfig toMsg =
    let
        model =
            case viewConfig.value of
                Model model_ ->
                    model_

        boolToString x =
            if x then
                "true"

            else
                "false"

        { t } =
            viewConfig.translators
    in
    div options.containerAttrs
        [ div options.editorContainerAttrs
            [ if String.isEmpty options.label then
                Html.text ""

              else
                View.Components.label []
                    { targetId = model.id, labelText = options.label }
            , Html.node "richtext-editor"
                [ Html.Attributes.attribute "elm-placeholder" (Maybe.withDefault "" options.placeholder)
                , Html.Attributes.attribute "elm-id" model.id
                , Html.Attributes.attribute "elm-has-error" (boolToString viewConfig.hasError)
                , Html.Attributes.attribute "elm-edit-text" (t "markdown.link_tooltip.edit")
                , Html.Attributes.attribute "elm-remove-text" (t "markdown.link_tooltip.remove")
                , Html.Attributes.attribute "elm-disabled" (boolToString options.disabled)
                , Html.Events.on "clicked-include-link" (Json.Decode.map ClickedIncludeLink linkDecoder)
                , Html.Events.on "text-change" (Json.Decode.map ChangedText textChangeDecoder)
                , Html.Events.on "component-loaded" (Json.Decode.succeed ComponentLoaded)
                ]
                []
                |> Html.map toMsg
            , viewConfig.error
            ]
        , case model.linkModalState of
            NotShowing ->
                Html.text ""

            Showing linkModal ->
                Modal.initWith
                    { closeMsg = ClosedLinkModal
                    , isVisible = True
                    }
                    |> Modal.withHeader (t "markdown.link_form.header")
                    |> Modal.withBody
                        [ Html.form
                            [ Utils.onSubmitPreventAll ClickedAcceptLink
                            , Html.Attributes.novalidate True
                            ]
                            [ Form.Text.init
                                { label = t "markdown.link_form.label"
                                , id = model.id ++ "-link-modal-label"
                                }
                                |> (\config ->
                                        Form.Text.view config
                                            { onChange = EnteredLinkLabel
                                            , onBlur = NoOp
                                            , value = linkModal.label
                                            , error = Html.text ""
                                            , hasError = False
                                            , translators = viewConfig.translators
                                            , isRequired = True
                                            }
                                   )
                            , Form.Text.init
                                { label = t "markdown.link_form.url"
                                , id = model.id ++ "-link-modal-url"
                                }
                                |> Form.Text.withType Form.Text.Url
                                |> (\config ->
                                        Form.Text.view config
                                            { onChange = EnteredLinkUrl
                                            , onBlur = NoOp
                                            , value = linkModal.url
                                            , error = Html.text ""
                                            , hasError = False
                                            , translators = viewConfig.translators
                                            , isRequired = True
                                            }
                                   )
                            , div [ class "flex justify-center items-center" ]
                                [ button
                                    [ class "modal-cancel"
                                    , Html.Events.onClick ClosedLinkModal
                                    , Html.Attributes.type_ "button"
                                    ]
                                    [ Html.text <| t "menu.cancel" ]
                                , button
                                    [ class "modal-accept"
                                    , Html.Attributes.type_ "submit"
                                    ]
                                    [ Html.text <| t "menu.save" ]
                                ]
                            ]
                        ]
                    |> Modal.toHtml
                    |> Html.map toMsg
        ]



-- GETTERS


getId : Model -> String
getId (Model model) =
    model.id


getMarkdownContent : Model -> Markdown
getMarkdownContent (Model model) =
    Markdown.fromQuillOps model.contents



-- THE ELM ARCHITECTURE
-- MODEL


type Model
    = Model
        { linkModalState : LinkModalState
        , id : String
        , contents : List QuillOp
        , isFocused : Bool
        }


initModel : String -> Maybe Markdown -> Model
initModel id maybeMarkdown =
    Model
        { linkModalState = NotShowing
        , id = id
        , contents =
            maybeMarkdown
                |> Maybe.map Markdown.toQuillOps
                |> Maybe.withDefault []
        , isFocused = False
        }



-- TYPES


type alias Link =
    { label : String, url : String }


type LinkModalState
    = NotShowing
    | Showing Link


type Msg
    = NoOp
    | ClickedIncludeLink Link
    | ClosedLinkModal
    | EnteredLinkLabel String
    | EnteredLinkUrl String
    | ClickedAcceptLink
    | ChangedText (List QuillOp)
    | ComponentLoaded



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        NoOp ->
            ( Model model, Cmd.none )

        ClickedIncludeLink link ->
            ( Model { model | linkModalState = Showing link }, Cmd.none )

        ClosedLinkModal ->
            ( Model { model | linkModalState = NotShowing }, Cmd.none )

        EnteredLinkLabel label ->
            case model.linkModalState of
                Showing link ->
                    ( Model { model | linkModalState = Showing { link | label = label } }
                    , Cmd.none
                    )

                NotShowing ->
                    ( Model model, Cmd.none )

        EnteredLinkUrl url ->
            case model.linkModalState of
                Showing link ->
                    ( Model { model | linkModalState = Showing { link | url = url } }
                    , Cmd.none
                    )

                NotShowing ->
                    ( Model model, Cmd.none )

        ClickedAcceptLink ->
            case model.linkModalState of
                Showing { label, url } ->
                    ( Model { model | linkModalState = NotShowing }
                    , Ports.sendMarkdownLink { id = model.id, label = label, url = url }
                    )

                NotShowing ->
                    ( Model model, Cmd.none )

        ChangedText quillOps ->
            ( Model { model | contents = quillOps }, Cmd.none )

        ComponentLoaded ->
            ( Model model
            , Ports.setMarkdownContent
                { id = model.id
                , content = Json.Encode.list Markdown.encodeQuillOp model.contents
                }
            )



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

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

        ChangedText _ ->
            [ "ChangedText" ]

        ComponentLoaded ->
            [ "ComponentLoaded" ]



-- INTERNAL HELPERS


linkDecoder : Json.Decode.Decoder Link
linkDecoder =
    Json.Decode.field "detail"
        (Json.Decode.succeed Link
            |> Decode.required "label" Json.Decode.string
            |> Decode.required "url" Json.Decode.string
        )


textChangeDecoder : Json.Decode.Decoder (List QuillOp)
textChangeDecoder =
    Json.Decode.at [ "detail", "ops" ] (Json.Decode.list Markdown.quillOpDecoder)
