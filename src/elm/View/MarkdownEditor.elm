module View.MarkdownEditor exposing
    ( Formatting(..)
    , LinkModalState(..)
    , Model
    , Msg
    , QuillOp
    , forceSetContents
    , init
    , msgToString
    , quillOpFromMarkdown
    , quillOpToMarkdown
    , removeFormatting
    , setContents
    , update
    , view
    , viewReadOnly
    )

import Dict
import Html exposing (Html, a, button, div, node, p, text, u)
import Html.Attributes exposing (attribute, class, href, id, novalidate, target, type_)
import Html.Events exposing (on, onClick)
import Json.Decode
import Json.Decode.Pipeline as Decode
import Json.Encode
import List.Extra as List
import Log
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Maybe.Extra
import Parser
import Parser.Advanced
import Ports
import Session.Shared exposing (Translators)
import Utils
import View.Form
import View.Form.Input as Input
import View.Modal as Modal



-- MODEL


type alias Model =
    { linkModalState : LinkModalState
    , id : String
    , contents : String
    }


init : String -> Model
init id =
    { linkModalState = NotShowing
    , id = id
    , contents = ""
    }



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
    | ChangedText (List QuillOp)
    | RequestedSetContents



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
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
                Editing { label, url } ->
                    ( { model | linkModalState = NotShowing }
                    , Ports.sendMarkdownLink { id = model.id, label = label, url = url }
                    )

                NotShowing ->
                    ( model, Cmd.none )

        ChangedText result ->
            ( { model | contents = quillOpsToMarkdown result }, Cmd.none )

        RequestedSetContents ->
            ( model
            , case quillOpFromMarkdown model.contents of
                Ok validQuillOps ->
                    Ports.setMarkdownContent
                        { id = model.id
                        , content = Json.Encode.list encodeQuillOp validQuillOps
                        }

                Err deadEnds ->
                    { username = Nothing
                    , message = "Got an error when parsing markdown"
                    , tags = [ Log.TypeTag Log.MarkdownError ]
                    , location = { moduleName = "View.MarkdownEditor", function = "update" }
                    , contexts =
                        [ { name = "Markdown info"
                          , extras =
                                (( "Received", Json.Encode.string model.contents )
                                    :: (deadEnds
                                            |> List.indexedMap
                                                (\index deadEnd ->
                                                    ( "Problem " ++ String.fromInt index
                                                    , Markdown.Parser.deadEndToString deadEnd
                                                        |> Json.Encode.string
                                                    )
                                                )
                                       )
                                )
                                    |> Dict.fromList
                          }
                        , { name = "Editor info"
                          , extras =
                                [ ( "id", Json.Encode.string model.id ) ]
                                    |> Dict.fromList
                          }
                        ]
                    , transaction = msg
                    , level = Log.Error
                    }
                        |> Log.send msgToString
            )


setContents : String -> Model -> Model
setContents contents model =
    { model | contents = contents }


forceSetContents : String -> Model -> ( Model, Cmd Msg )
forceSetContents contents model =
    model
        |> setContents contents
        |> update RequestedSetContents



-- VIEW


viewReadOnly : List (Html.Attribute msg) -> String -> Html msg
viewReadOnly attributes content =
    case Markdown.Parser.parse content of
        Ok blocks ->
            let
                defaultRenderer =
                    Markdown.Renderer.defaultHtmlRenderer

                renderer =
                    { defaultRenderer
                        | link =
                            \link linkContent ->
                                case link.title of
                                    Just title ->
                                        a
                                            [ href link.destination
                                            , Html.Attributes.title title
                                            , target "_blank"
                                            ]
                                            linkContent

                                    Nothing ->
                                        a [ href link.destination, target "_blank" ]
                                            linkContent
                        , html =
                            Markdown.Html.oneOf
                                [ Markdown.Html.tag "u"
                                    (\children -> u [ class "inline-children" ] children)
                                ]
                    }
            in
            case Markdown.Renderer.render renderer blocks of
                Ok asHtml ->
                    div (class "markdown-viewer" :: attributes) asHtml

                Err _ ->
                    text ""

        Err _ ->
            text ""


view :
    { translators : Translators
    , placeholder : Maybe String
    , label : String
    , problem : Maybe String
    , disabled : Bool
    }
    -> List (Html.Attribute Msg)
    -> Model
    -> Html Msg
view { translators, placeholder, label, problem, disabled } attributes model =
    let
        { t } =
            translators
    in
    div []
        [ div (class "mb-10" :: attributes)
            [ if String.isEmpty label then
                text ""

              else
                View.Form.label [] model.id label
            , node "richtext-editor"
                [ attribute "elm-placeholder" (Maybe.withDefault "" placeholder)
                , attribute "elm-has-error"
                    (case problem of
                        Nothing ->
                            "false"

                        Just _ ->
                            "true"
                    )
                , attribute "elm-edit-text" (t "markdown.link_tooltip.edit")
                , attribute "elm-remove-text" (t "markdown.link_tooltip.remove")
                , attribute "elm-disabled"
                    (if disabled then
                        "true"

                     else
                        "false"
                    )
                , on "clicked-include-link" (Json.Decode.map ClickedIncludeLink linkDecoder)
                , on "text-change" (Json.Decode.map ChangedText textChangeDecoder)
                , on "component-loaded" (Json.Decode.succeed RequestedSetContents)
                , id model.id
                ]
                []
            , case problem of
                Nothing ->
                    text ""

                Just problem_ ->
                    p [ class "form-error" ] [ text problem_ ]
            ]
        , case model.linkModalState of
            Editing linkModal ->
                Modal.initWith
                    { closeMsg = ClosedLinkModal
                    , isVisible = True
                    }
                    |> Modal.withHeader (t "markdown.link_form.header")
                    |> Modal.withBody
                        [ Html.form
                            [ Utils.onSubmitPreventAll ClickedAcceptLink
                            , novalidate True
                            ]
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
                                |> Input.withType Input.Url
                                |> Input.toHtml
                            , div [ class "flex justify-center items-center" ]
                                [ button
                                    [ class "modal-cancel"
                                    , onClick ClosedLinkModal
                                    , type_ "button"
                                    ]
                                    [ text <| t "menu.cancel" ]
                                , button
                                    [ class "modal-accept"
                                    , type_ "submit"
                                    ]
                                    [ text <| t "menu.save" ]
                                ]
                            ]
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
    | Underline
    | LinkFormatting String
    | Header Int
    | OrderedList
    | UnorderedList


type alias QuillOp =
    { insert : String
    , attributes : List Formatting
    }


textChangeDecoder : Json.Decode.Decoder (List QuillOp)
textChangeDecoder =
    Json.Decode.list quillOpDecoder
        |> Json.Decode.at [ "detail", "ops" ]


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
        (\bold italic strike underline link header list ->
            [ bold, italic, strike, underline, link, header, list ]
                |> List.filterMap identity
        )
        |> optionalFormatting "bold" Bold
        |> optionalFormatting "italic" Italic
        |> optionalFormatting "strike" Strike
        |> optionalFormatting "underline" Underline
        |> Decode.optional "link"
            (Json.Decode.string
                |> Json.Decode.map (LinkFormatting >> Just)
            )
            Nothing
        |> Decode.optional "header"
            (Json.Decode.int
                |> Json.Decode.map (Header >> Just)
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
            ( "_", "_" )

        Strike ->
            ( "~~", "~~" )

        Underline ->
            ( "<u>", "</u>" )

        LinkFormatting link ->
            ( "[", "](" ++ link ++ ")" )

        Header level ->
            ( String.repeat level "#" ++ " ", "" )

        OrderedList ->
            ( "\n1. ", "" )

        UnorderedList ->
            ( "\n- ", "" )


quillOpsToMarkdown : List QuillOp -> String
quillOpsToMarkdown quillOps =
    let
        isHeader : Formatting -> Bool
        isHeader formatting =
            case formatting of
                Header _ ->
                    True

                _ ->
                    False

        headerLevel : List Formatting -> Maybe Int
        headerLevel formattings =
            List.filterMap
                (\formatting ->
                    case formatting of
                        Header level ->
                            Just level

                        _ ->
                            Nothing
                )
                formattings
                |> List.head

        foldFn :
            QuillOp
            -> { previousItems : List QuillOp, currString : String }
            -> { previousItems : List QuillOp, currString : String }
        foldFn currItem { previousItems, currString } =
            if List.member OrderedList currItem.attributes then
                { previousItems = []
                , currString =
                    currString
                        ++ "1. "
                        ++ (previousItems
                                |> List.reverse
                                |> List.map quillOpToMarkdown
                                |> String.concat
                           )
                        ++ "\n"
                }

            else if List.member UnorderedList currItem.attributes then
                { previousItems = []
                , currString =
                    currString
                        ++ "- "
                        ++ (previousItems
                                |> List.reverse
                                |> List.map quillOpToMarkdown
                                |> String.concat
                           )
                        ++ "\n"
                }

            else if List.any isHeader currItem.attributes then
                { previousItems = []
                , currString =
                    currString
                        ++ (headerLevel currItem.attributes
                                |> Maybe.withDefault 0
                                |> (\level -> String.repeat level "#")
                           )
                        ++ " "
                        ++ (previousItems
                                |> List.reverse
                                |> List.map quillOpToMarkdown
                                |> String.concat
                           )
                        ++ "\n"
                }

            else if String.endsWith "\n" currItem.insert then
                { previousItems = []
                , currString =
                    currString
                        ++ ((currItem :: previousItems)
                                |> List.reverse
                                |> List.map quillOpToMarkdown
                                |> String.concat
                           )
                }

            else
                let
                    currItems : List QuillOp
                    currItems =
                        currItem.insert
                            |> String.split "\n"
                            |> List.map
                                (\insert ->
                                    { insert = insert
                                    , attributes = currItem.attributes
                                    }
                                )

                    ( firstItems, lastItem ) =
                        ( List.take (List.length currItems - 1) currItems
                            |> List.map quillOpToMarkdown
                        , List.last currItems
                        )
                in
                { previousItems =
                    case lastItem of
                        Nothing ->
                            []

                        Just lastItem_ ->
                            if List.isEmpty firstItems then
                                lastItem_ :: previousItems

                            else
                                [ lastItem_ ]
                , currString =
                    if List.isEmpty firstItems then
                        currString

                    else
                        currString
                            ++ (previousItems
                                    |> List.reverse
                                    |> List.map quillOpToMarkdown
                                    |> String.concat
                               )
                            ++ String.join "\n" firstItems
                            ++ "\n"
                }
    in
    quillOps
        |> List.foldl foldFn { previousItems = [], currString = "" }
        |> .currString


quillOpToMarkdown : QuillOp -> String
quillOpToMarkdown quillOp =
    let
        addFormatting : Formatting -> String -> String
        addFormatting formatting unformattedText =
            let
                ( formatBefore, formatAfter ) =
                    formatStrings formatting

                shouldAddSurroundingSpaces =
                    case formatting of
                        Bold ->
                            True

                        Italic ->
                            True

                        Strike ->
                            True

                        Underline ->
                            True

                        _ ->
                            False

                spacesBefore =
                    if shouldAddSurroundingSpaces then
                        String.toList unformattedText
                            |> List.takeWhile ((==) ' ')
                            |> List.length

                    else
                        0

                spacesAfter =
                    if shouldAddSurroundingSpaces then
                        String.toList unformattedText
                            |> List.reverse
                            |> List.takeWhile ((==) ' ')
                            |> List.length

                    else
                        0

                maybeTrim =
                    if shouldAddSurroundingSpaces then
                        String.trim

                    else
                        identity
            in
            String.repeat spacesBefore " "
                ++ formatBefore
                ++ maybeTrim unformattedText
                ++ formatAfter
                ++ String.repeat spacesAfter " "
    in
    if String.isEmpty (String.trim quillOp.insert) then
        ""

    else
        List.foldr addFormatting quillOp.insert quillOp.attributes
            -- We need the following replacements to make sure we can correctly
            -- parse some special sequences. In practice, these should be rare
            |> String.replace "*****" "__*__"
            |> String.replace "***" "_*_"
            |> String.replace "_____" "**_**"
            |> String.replace "___" "*_*"


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
        parseList listType children =
            children
                |> List.map
                    (List.map quillOpFromMarkdownBlock
                        >> List.intercalate [ { insert = " ", attributes = [] } ]
                        >> (\line -> line ++ [ { insert = "\n", attributes = [ listType ] } ])
                    )
                |> List.concat
                |> (\list -> { insert = "\n", attributes = [] } :: list)
    in
    case block of
        Markdown.Block.UnorderedList _ children ->
            let
                listItemToList (Markdown.Block.ListItem _ children_) =
                    children_
            in
            children
                |> List.map listItemToList
                |> parseList UnorderedList

        Markdown.Block.OrderedList _ _ children ->
            children
                |> parseList OrderedList

        Markdown.Block.Heading headingLevel children ->
            children
                |> List.map quillOpFromMarkdownInline
                |> List.concat
                |> (\l -> l ++ [ { insert = "\n", attributes = [ Header (Markdown.Block.headingLevelToInt headingLevel) ] } ])

        Markdown.Block.Paragraph children ->
            children
                |> List.map quillOpFromMarkdownInline
                |> List.concat

        Markdown.Block.HtmlBlock (Markdown.Block.HtmlElement "u" _ children) ->
            -- Parse underlined text
            children
                |> List.concatMap quillOpFromMarkdownBlock
                |> List.map (\quillOp -> { quillOp | attributes = Underline :: quillOp.attributes })

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

        Markdown.Block.HtmlInline (Markdown.Block.HtmlElement "u" _ children) ->
            -- Parse underlined text
            children
                |> List.concatMap quillOpFromMarkdownBlock
                |> List.map (\quillOp -> { quillOp | attributes = Underline :: quillOp.attributes })

        _ ->
            []



-- UTILS


removeFormatting : String -> String
removeFormatting markdownString =
    case Markdown.Parser.parse markdownString of
        Ok blocks ->
            blocks
                |> List.map removeFormattingFromBlock
                |> Maybe.Extra.combine
                |> Maybe.map (String.join "\n")
                |> Maybe.withDefault markdownString
                |> String.trim

        Err _ ->
            markdownString


removeFormattingFromBlock : Markdown.Block.Block -> Maybe String
removeFormattingFromBlock block =
    let
        removeFormattingFromList : (Int -> String) -> List (List Markdown.Block.Block) -> String
        removeFormattingFromList lineStarter blocks =
            blocks
                |> List.indexedMap
                    (\index blockChild ->
                        List.map removeFormattingFromBlock blockChild
                            |> List.filterMap identity
                            |> List.map (\line -> lineStarter index ++ line)
                            |> String.concat
                    )
                |> String.join "\n"
    in
    case block of
        Markdown.Block.UnorderedList _ children ->
            children
                |> List.map (\(Markdown.Block.ListItem _ children_) -> children_)
                |> removeFormattingFromList (\_ -> "- ")
                |> Just

        Markdown.Block.OrderedList _ _ children ->
            children
                |> removeFormattingFromList (\index -> String.fromInt (index + 1) ++ ". ")
                |> Just

        Markdown.Block.Heading _ inlines ->
            Markdown.Block.extractInlineText inlines
                |> Just

        Markdown.Block.Paragraph inlines ->
            Markdown.Block.extractInlineText inlines
                |> Just

        Markdown.Block.HtmlBlock (Markdown.Block.HtmlElement _ _ children) ->
            List.map removeFormattingFromBlock children
                |> Maybe.Extra.combine
                |> Maybe.map (String.join " ")

        _ ->
            Nothing


encodeQuillOp : QuillOp -> Json.Encode.Value
encodeQuillOp quillOp =
    Json.Encode.object
        [ ( "insert", Json.Encode.string quillOp.insert )
        , ( "attributes"
          , Json.Encode.object
                (List.map encodeFormatting quillOp.attributes)
          )
        ]


encodeFormatting : Formatting -> ( String, Json.Encode.Value )
encodeFormatting formatting =
    case formatting of
        Bold ->
            ( "bold", Json.Encode.bool True )

        Italic ->
            ( "italic", Json.Encode.bool True )

        Strike ->
            ( "strike", Json.Encode.bool True )

        Underline ->
            ( "underline", Json.Encode.bool True )

        LinkFormatting url ->
            ( "link", Json.Encode.string url )

        Header level ->
            ( "header", Json.Encode.int level )

        OrderedList ->
            ( "list", Json.Encode.string "ordered" )

        UnorderedList ->
            ( "list", Json.Encode.string "bullet" )


linkDecoder : Json.Decode.Decoder Link
linkDecoder =
    Json.Decode.succeed Link
        |> Decode.required "label" Json.Decode.string
        |> Decode.required "url" Json.Decode.string
        |> Json.Decode.field "detail"


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

        ChangedText _ ->
            [ "ChangedText" ]

        RequestedSetContents ->
            [ "RequestedSetContents" ]
