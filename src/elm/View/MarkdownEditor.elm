module View.MarkdownEditor exposing
    ( Formatting(..)
    , Model
    , Msg
    , QuillOp
    , init
    , msgToString
    , quillOpFromMarkdown
    , quillOpToMarkdown
    , subscriptions
    , update
    , view
    )

import Browser.Dom
import Browser.Events
import Html exposing (Html, button, div, node, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (on, onClick)
import Json.Decode
import Json.Decode.Pipeline as Decode
import Markdown.Block
import Markdown.Parser
import Parser
import Parser.Advanced
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
    | ChangedText (List QuillOp)



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

        ChangedText result ->
            let
                _ =
                    result
                        |> List.map quillOpToMarkdown
                        |> String.concat
            in
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
            , on "text-change" (Json.Decode.map ChangedText textChangeDecoder)
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



-- PARSING QUILL FORMATTING


type Formatting
    = Bold
    | Italic
    | Strike
    | LinkFormatting String
    | OrderedList
    | UnorderedList


type alias QuillOp =
    { insert : String
    , attributes : List Formatting
    }


textChangeDecoder : Json.Decode.Decoder (List QuillOp)
textChangeDecoder =
    Json.Decode.value
        |> Json.Decode.andThen
            (\event ->
                case Json.Decode.decodeValue (Json.Decode.list quillOpDecoder |> Json.Decode.at [ "detail", "ops" ]) event of
                    Ok val ->
                        Json.Decode.succeed val

                    Err err ->
                        err
                            |> Json.Decode.errorToString
                            |> Json.Decode.fail
            )


quillOpDecoder : Json.Decode.Decoder QuillOp
quillOpDecoder =
    Json.Decode.succeed QuillOp
        |> Decode.required "insert" Json.Decode.string
        |> Decode.optional "attributes" formattingDecoder []


formattingDecoder : Json.Decode.Decoder (List Formatting)
formattingDecoder =
    let
        optionalFormatting key formatting =
            Decode.optional key (Json.Decode.succeed (Just formatting)) Nothing
    in
    Json.Decode.succeed
        (\bold italic strike link list ->
            [ bold, italic, strike, link, list ]
                |> List.filterMap identity
        )
        |> optionalFormatting "bold" Bold
        |> optionalFormatting "italic" Italic
        |> optionalFormatting "strike" Strike
        |> Decode.optional "link"
            (Json.Decode.string
                |> Json.Decode.map (LinkFormatting >> Just)
            )
            Nothing
        |> Decode.optional "list"
            (Json.Decode.string
                |> Json.Decode.andThen
                    (\decodedString ->
                        case decodedString of
                            "bullet" ->
                                Json.Decode.succeed (Just UnorderedList)

                            "ordered" ->
                                Json.Decode.succeed (Just OrderedList)

                            _ ->
                                Json.Decode.fail "Expected either `bullet` or `ordered` as a list type"
                    )
            )
            Nothing


formatStrings : Formatting -> ( String, String )
formatStrings formatting =
    case formatting of
        Bold ->
            ( "**", "**" )

        Italic ->
            ( "*", "*" )

        Strike ->
            ( "~~", "~~" )

        LinkFormatting link ->
            ( "[", "](" ++ link ++ ")" )

        OrderedList ->
            ( "1. ", "" )

        UnorderedList ->
            ( "- ", "" )


quillOpToMarkdown : QuillOp -> String
quillOpToMarkdown quillOp =
    let
        addFormatting : Formatting -> String -> String
        addFormatting formatting unformattedText =
            let
                ( formatBefore, formatAfter ) =
                    formatStrings formatting
            in
            formatBefore ++ unformattedText ++ formatAfter
    in
    List.foldr addFormatting quillOp.insert quillOp.attributes


quillOpFromMarkdown : String -> Result (List (Parser.Advanced.DeadEnd String Parser.Problem)) (List QuillOp)
quillOpFromMarkdown markdown =
    Markdown.Parser.parse markdown
        |> Result.map
            (List.map quillOpFromMarkdownBlock
                >> List.concat
            )


quillOpFromMarkdownBlock : Markdown.Block.Block -> List QuillOp
quillOpFromMarkdownBlock block =
    let
        addEverySecond : a -> List a -> List a
        addEverySecond sep items =
            List.foldr
                (\currItem separatedList -> currItem :: sep :: separatedList)
                []
                items
    in
    case block of
        Markdown.Block.UnorderedList children ->
            let
                listItemToList : Markdown.Block.ListItem children -> List children
                listItemToList (Markdown.Block.ListItem _ children_) =
                    children_
            in
            children
                |> List.map (listItemToList >> List.map quillOpFromMarkdownInline)
                |> List.concat
                |> List.concat
                |> addEverySecond { insert = "\n", attributes = [ UnorderedList ] }

        Markdown.Block.OrderedList _ children ->
            children
                |> List.map (List.map quillOpFromMarkdownInline)
                |> List.concat
                |> List.concat
                |> addEverySecond { insert = "\n", attributes = [ OrderedList ] }

        Markdown.Block.Paragraph children ->
            children
                |> List.map quillOpFromMarkdownInline
                |> List.concat

        _ ->
            []


quillOpFromMarkdownInline : Markdown.Block.Inline -> List QuillOp
quillOpFromMarkdownInline inline =
    let
        addFormatting children formatting =
            children
                |> List.map quillOpFromMarkdownInline
                |> List.concat
                |> List.map (\quillOp -> { quillOp | attributes = formatting :: quillOp.attributes })
    in
    case inline of
        Markdown.Block.Link link _ children ->
            LinkFormatting link
                |> addFormatting children

        Markdown.Block.Emphasis children ->
            addFormatting children Italic

        Markdown.Block.Strong children ->
            addFormatting children Bold

        Markdown.Block.Strikethrough children ->
            addFormatting children Strike

        Markdown.Block.Text content ->
            [ { insert = content, attributes = [] } ]

        _ ->
            []



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
    Json.Decode.succeed Link
        |> Decode.required "label" Json.Decode.string
        |> Decode.required "url" Json.Decode.string
        |> Json.Decode.field "detail"


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

        ChangedText _ ->
            [ "ChangedText" ]
