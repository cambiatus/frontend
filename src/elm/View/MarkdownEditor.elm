module View.MarkdownEditor exposing
    ( Formatting(..)
    , QuillOp
    , quillOpFromMarkdown
    , quillOpToMarkdown
    , viewReadOnly
    )

import Html exposing (Html, a, div, text, u)
import Html.Attributes exposing (class, href, target)
import List.Extra as List
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Parser
import Parser.Advanced



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
