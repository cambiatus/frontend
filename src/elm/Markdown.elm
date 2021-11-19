module Markdown exposing (Formatting(..), Markdown, QuillOp, encodeQuillOp, fromQuillOps, quillOpDecoder, toQuillOps)

import Json.Decode
import Json.Decode.Pipeline as Decode
import Json.Encode
import List.Extra
import Markdown.Block
import Markdown.Parser
import View.MarkdownEditor exposing (QuillOp)


type Markdown
    = Markdown String



-- PRODUCING MARKDOWN


type QuillOp
    = QuillOp
        { insert : String
        , attributes : List Formatting
        }


type Formatting
    = Bold
    | Italic
    | Strike
    | Underline
    | LinkFormatting String
    | Header Int
    | OrderedList
    | UnorderedList



-- TO QUILL OPS


toQuillOps : Markdown -> List QuillOp
toQuillOps (Markdown markdown) =
    Markdown.Parser.parse markdown
        |> Result.map (List.map quillOpFromMarkdownBlock >> List.concat)
        |> Result.withDefault []


quillOpFromMarkdownBlock : Markdown.Block.Block -> List QuillOp
quillOpFromMarkdownBlock block =
    let
        parseList listType children =
            children
                |> List.map
                    (List.map quillOpFromMarkdownBlock
                        >> List.Extra.intercalate [ QuillOp { insert = " ", attributes = [] } ]
                        >> (\line -> line ++ [ QuillOp { insert = "\n", attributes = [ listType ] } ])
                    )
                |> List.concat
                |> (\list -> QuillOp { insert = "\n", attributes = [] } :: list)
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
                |> (\l -> l ++ [ QuillOp { insert = "\n", attributes = [ Header (Markdown.Block.headingLevelToInt headingLevel) ] } ])

        Markdown.Block.Paragraph children ->
            children
                |> List.map quillOpFromMarkdownInline
                |> List.concat

        Markdown.Block.HtmlBlock (Markdown.Block.HtmlElement "u" _ children) ->
            -- Parse underlined text
            children
                |> List.concatMap quillOpFromMarkdownBlock
                |> List.map (\(QuillOp quillOp) -> QuillOp { quillOp | attributes = Underline :: quillOp.attributes })

        _ ->
            []


quillOpFromMarkdownInline : Markdown.Block.Inline -> List QuillOp
quillOpFromMarkdownInline inline =
    let
        addFormatting children formatting =
            children
                |> List.map quillOpFromMarkdownInline
                |> List.concat
                |> List.map (\(QuillOp quillOp) -> QuillOp { quillOp | attributes = formatting :: quillOp.attributes })
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
            [ QuillOp { insert = content, attributes = [] } ]

        Markdown.Block.HtmlInline (Markdown.Block.HtmlElement "u" _ children) ->
            -- Parse underlined text
            children
                |> List.concatMap quillOpFromMarkdownBlock
                |> List.map (\(QuillOp quillOp) -> QuillOp { quillOp | attributes = Underline :: quillOp.attributes })

        _ ->
            []



-- FROM QUILLOPS


fromQuillOps : List QuillOp -> Markdown
fromQuillOps quillOps =
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
        foldFn (QuillOp currItem) { previousItems, currString } =
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
                        ++ ((QuillOp currItem :: previousItems)
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
                                    QuillOp
                                        { insert = insert
                                        , attributes = currItem.attributes
                                        }
                                )

                    ( firstItems, lastItem ) =
                        ( List.take (List.length currItems - 1) currItems
                            |> List.map quillOpToMarkdown
                        , List.Extra.last currItems
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
        |> Markdown


quillOpToMarkdown : QuillOp -> String
quillOpToMarkdown (QuillOp quillOp) =
    let
        addFormatting : Formatting -> String -> String
        addFormatting formatting unformattedText =
            let
                ( formatBefore, formatAfter ) =
                    formattingStrings formatting

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
                            |> List.Extra.takeWhile ((==) ' ')
                            |> List.length

                    else
                        0

                spacesAfter =
                    if shouldAddSurroundingSpaces then
                        String.toList unformattedText
                            |> List.reverse
                            |> List.Extra.takeWhile ((==) ' ')
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


formattingStrings : Formatting -> ( String, String )
formattingStrings formatting =
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



-- ENCODING


encodeQuillOp : QuillOp -> Json.Encode.Value
encodeQuillOp (QuillOp quillOp) =
    Json.Encode.object
        [ ( "insert", Json.Encode.string quillOp.insert )
        , ( "attributes", Json.Encode.object (List.map encodeFormatting quillOp.attributes) )
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



-- DECODING


quillOpDecoder : Json.Decode.Decoder QuillOp
quillOpDecoder =
    Json.Decode.succeed (\insert attributes -> QuillOp { insert = insert, attributes = attributes })
        |> Decode.required "insert" Json.Decode.string
        |> Decode.optional "attributes" formattingObjectDecoder []


formattingObjectDecoder : Json.Decode.Decoder (List Formatting)
formattingObjectDecoder =
    let
        baseDecoder : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a -> b) -> Json.Decode.Decoder b
        baseDecoder key decoder =
            Decode.optional key (Json.Decode.map Just decoder) Nothing

        simpleFormatting key formatting =
            baseDecoder key (Json.Decode.succeed formatting)
    in
    Json.Decode.succeed
        (\bold italic strike underline link header list ->
            [ bold, italic, strike, underline, link, header, list ]
                |> List.filterMap identity
        )
        |> simpleFormatting "bold" Bold
        |> simpleFormatting "italic" Italic
        |> simpleFormatting "strike" Strike
        |> simpleFormatting "underline" Underline
        |> baseDecoder "link" (Json.Decode.string |> Json.Decode.map LinkFormatting)
        |> baseDecoder "header" (Json.Decode.int |> Json.Decode.map Header)
        |> baseDecoder "list"
            (Json.Decode.string
                |> Json.Decode.andThen
                    (\decodedString ->
                        case decodedString of
                            "bullet" ->
                                Json.Decode.succeed UnorderedList

                            "ordered" ->
                                Json.Decode.succeed OrderedList

                            _ ->
                                Json.Decode.fail "Expected either `bullet` or `ordered` as a list type"
                    )
            )
