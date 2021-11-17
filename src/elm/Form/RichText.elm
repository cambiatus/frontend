module Form.RichText exposing
    ( init, Options
    , withDisabled, withPlaceholder, withContainerAttrs
    , getId
    , view
    , Model, Msg, initModel, msgToString, update
    )

{-| Creates a Cambiatus-style RichText. Use it within a `Form.Form`:

    Form.RichText.init
        { label = text "Memo (optional)"
        , id = "memo-input"
        }


# Initializing

@docs init, Options


# Helpers


## Adding attributes

@docs withDisabled, withPlaceholder, withContainerAttrs


# Getters

@docs getId


# View

@docs view

-}

import Form.Text
import Html exposing (Html, button, div, input, label)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onBlur)
import Json.Decode
import Json.Decode.Pipeline as Decode
import Ports
import Session.Shared as Shared
import Utils
import View.Form
import View.Modal as Modal



-- OPTIONS


type Options msg
    = Options
        { label : String
        , id : String
        , disabled : Bool
        , placeholder : Maybe String
        , containerAttrs : List (Html.Attribute msg)
        }


{-| Initializes a RichText
-}
init : { label : String, id : String } -> Options msg
init { label, id } =
    Options
        { label = label
        , id = id
        , disabled = False
        , placeholder = Nothing
        , containerAttrs = []
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


{-| Adds attributes to the element that container the label, the editor itself,
and the error message
-}
withContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }



-- VIEW


type alias ViewConfig msg =
    { onChange : ( Model, Cmd Msg ) -> msg
    , onBlur : String -> msg
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
    div []
        [ div options.containerAttrs
            [ if String.isEmpty options.label then
                Html.text ""

              else
                View.Form.label [] options.id options.label
            , Html.node "markdown-editor"
                [ Html.Attributes.attribute "elm-placeholder" (Maybe.withDefault "" options.placeholder)
                , Html.Attributes.attribute "elm-has-error" (boolToString viewConfig.hasError)
                , Html.Attributes.attribute "elm-edit-text" (t "markdown.link_tooltip.edit")
                , Html.Attributes.attribute "elm-remove-text" (t "markdown.link_tooltip.remove")
                , Html.Attributes.attribute "elm-disabled" (boolToString options.disabled)
                , Html.Events.on "clicked-include-link" (Json.Decode.map ClickedIncludeLink linkDecoder)
                , Html.Events.on "text-change" (Json.Decode.map ChangedText textChangeDecoder)
                , Html.Events.on "component-loaded" (Json.Decode.succeed RequestedSetContents)
                , Html.Attributes.id options.id
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
                                , id = options.id ++ "-link-modal-label"
                                }
                                |> (\config ->
                                        Form.Text.view config
                                            { onChange = EnteredLinkLabel
                                            , onBlur = \_ -> NoOp
                                            , value = linkModal.label
                                            , error = Html.text ""
                                            , hasError = False
                                            , translators = viewConfig.translators
                                            , isRequired = True
                                            }
                                   )
                            , Form.Text.init
                                { label = t "markdown.link_form.url"
                                , id = options.id ++ "-link-modal-url"
                                }
                                |> Form.Text.withType Form.Text.Url
                                |> (\config ->
                                        Form.Text.view config
                                            { onChange = EnteredLinkUrl
                                            , onBlur = \_ -> NoOp
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


getId : Options msg -> String
getId (Options options) =
    options.id



-- THE ELM ARCHITECTURE
-- MODEL


type Model
    = Model
        { linkModalState : LinkModalState

        -- TODO - We might not need id on options if we have it here
        , id : String
        , contents : String
        , isFocused : Bool
        }


initModel : String -> Model
initModel id =
    Model
        { linkModalState = NotShowing
        , id = id
        , contents = ""
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
    | RequestedSetContents


type alias QuillOp =
    {}



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        NoOp ->
            ( Model model, Cmd.none )

        ClickedIncludeLink link ->
            -- TODO - Add RichText custom type and selectionset
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

        ChangedText _ ->
            -- TODO
            ( Model model, Cmd.none )

        RequestedSetContents ->
            -- TODO
            ( Model model, Cmd.none )



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    -- TODO
    []



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
    -- TODO
    Json.Decode.succeed []
