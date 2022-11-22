module Markdown exposing
    ( view, toUnformattedString, toRawString
    , Markdown, empty, fromTranslation, fromTranslationWithReplacements
    , encode, decoder
    , selectionSet, maybeSelectionSet
    , QuillOp, fromQuillOps, toQuillOps, encodeQuillOp, quillOpDecoder
    , generator, shrink
    )

{-| This module helps you manipulate markdown/rich-text strings. Since we use
the QuillJS editor, we have some helper functions to translate their own data
model to regular markdown.


# View markdown

@docs view, toUnformattedString, toRawString


# Produce markdown

We want Markdown to be an opaque type, so we can't have a `fromString` method.
At the same time, we need some helper functions to work with Markdown

@docs Markdown, empty, fromTranslation, fromTranslationWithReplacements


## Interop with JSON

@docs encode, decoder


## Interop with GraphQL

@docs selectionSet, maybeSelectionSet


## Interop with QuillJS

@docs QuillOp, fromQuillOps, toQuillOps, encodeQuillOp, quillOpDecoder


## Test helpers

@docs generator, shrink

-}

import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes
import I18Next
import Json.Decode
import Json.Decode.Pipeline as Decode
import Json.Encode
import List.Extra
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Maybe.Extra
import Random
import Shrink exposing (Shrinker)
import Translation exposing (Translators)


{-| Markdown is just a string. It's an opaque type so we can ensure it's
actually markdown and not just some random string that might have other
formattings. If you want to produce some markdown, you probably need to get it
from the server, or request user input using `Form.RichText`.

The same reasoning behind using opaque types is used throughout this module, to
prevent people from generating markdown at will.

-}
type Markdown
    = Markdown String


{-| Markdown that represents the empty string
-}
empty : Markdown
empty =
    Markdown ""


{-| Convert a translation into Markdown
-}
fromTranslation : Translators -> String -> Markdown
fromTranslation { t } key =
    Markdown (t key)


{-| Convert a translation with replacements into Markdown
-}
fromTranslationWithReplacements : Translators -> String -> I18Next.Replacements -> Markdown
fromTranslationWithReplacements { tr } key replacements =
    Markdown (tr key replacements)


{-| Extract the raw Markdown string. You should use this as a last resort. You
can usually use other helpers, such as `encode` and `view`. A common use case is
on GraphQL mutations.
-}
toRawString : Markdown -> String
toRawString (Markdown markdown) =
    String.trim markdown



-- PRODUCING MARKDOWN
-- GRAPHQL


{-| Get some markdown from a Graphql endpoint.
-}
selectionSet : SelectionSet String typeLock -> SelectionSet Markdown typeLock
selectionSet =
    SelectionSet.map Markdown


{-| Get some optional markdown from a Graphql endpoint
-}
maybeSelectionSet : SelectionSet (Maybe String) typeLock -> SelectionSet (Maybe Markdown) typeLock
maybeSelectionSet =
    SelectionSet.map (Maybe.map Markdown)



-- QUILLJS


{-| This what we get when we ask for the QuillJS editor's contents: a `String`
with some `Formatting`s
-}
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


{-| It might be useful to convert markdown into something QuillJS can understand,
so we can set the content of an input programmatically.
-}
toQuillOps : Markdown -> List QuillOp
toQuillOps (Markdown markdown) =
    Markdown.Parser.parse markdown
        |> Result.map (List.concatMap quillOpFromMarkdownBlock)
        |> Result.withDefault []


quillOpFromMarkdownBlock : Markdown.Block.Block -> List QuillOp
quillOpFromMarkdownBlock block =
    let
        parseList listType children =
            children
                |> List.concatMap
                    (List.map quillOpFromMarkdownBlock
                        >> List.Extra.intercalate [ QuillOp { insert = " ", attributes = [] } ]
                        >> (\line -> line ++ [ QuillOp { insert = "\n", attributes = [ listType ] } ])
                    )
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
                |> List.concatMap quillOpFromMarkdownInline
                |> (\l -> l ++ [ QuillOp { insert = "\n", attributes = [ Header (Markdown.Block.headingLevelToInt headingLevel) ] } ])

        Markdown.Block.Paragraph children ->
            children
                |> List.concatMap quillOpFromMarkdownInline

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
                |> List.concatMap quillOpFromMarkdownInline
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


{-| This is what we use to get data from QuillJS into valid Markdown. Used in
`Form.RichText`
-}
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


encode : Markdown -> Json.Encode.Value
encode (Markdown markdown) =
    Json.Encode.string (String.trim markdown)


decoder : Json.Decode.Decoder Markdown
decoder =
    Json.Decode.map Markdown Json.Decode.string


{-| In order to send data to QuillJS (checkout `toQuillOp`), we need to send it
as a Json Value
-}
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


{-| In order to get data from QuillJs (checkout `fromQuillOp`), we need to
receive it as a Json Value
-}
quillOpDecoder : Json.Decode.Decoder QuillOp
quillOpDecoder =
    Json.Decode.succeed (\insert attributes -> QuillOp { insert = insert, attributes = attributes })
        |> Decode.required "insert" Json.Decode.string
        |> Decode.optional "attributes" formattingObjectDecoder []


formattingObjectDecoder : Json.Decode.Decoder (List Formatting)
formattingObjectDecoder =
    let
        baseDecoder : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a -> b) -> Json.Decode.Decoder b
        baseDecoder key formattingDecoder =
            Decode.optional key (Json.Decode.map Just formattingDecoder) Nothing

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



-- VIEWING


{-| Display some Markdown with some attributes
-}
view : List (Html.Attribute msg) -> Markdown -> Html msg
view attributes (Markdown content) =
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
                                        Html.a
                                            [ Html.Attributes.href link.destination
                                            , Html.Attributes.title title
                                            , Html.Attributes.target "_blank"
                                            ]
                                            linkContent

                                    Nothing ->
                                        Html.a
                                            [ Html.Attributes.href link.destination
                                            , Html.Attributes.target "_blank"
                                            ]
                                            linkContent
                        , html =
                            Markdown.Html.oneOf
                                [ Markdown.Html.tag "u"
                                    (\children ->
                                        Html.u
                                            [ Html.Attributes.class "inline-children" ]
                                            children
                                    )
                                ]
                    }
            in
            case Markdown.Renderer.render renderer blocks of
                Ok asHtml ->
                    Html.div (Html.Attributes.class "markdown-viewer break-words" :: attributes) asHtml

                Err _ ->
                    Html.text ""

        Err _ ->
            Html.text ""



-- REMOVING FORMATTING


toUnformattedString : Markdown -> String
toUnformattedString (Markdown markdown) =
    case Markdown.Parser.parse markdown of
        Ok blocks ->
            blocks
                |> List.map removeFormattingFromBlock
                |> Maybe.Extra.combine
                |> Maybe.map (String.join "\n")
                |> Maybe.withDefault markdown
                |> String.trim

        Err _ ->
            markdown


removeFormattingFromBlock : Markdown.Block.Block -> Maybe String
removeFormattingFromBlock block =
    let
        removeFormattingFromList : (Int -> String) -> List (List Markdown.Block.Block) -> String
        removeFormattingFromList lineStarter blocks =
            blocks
                |> List.indexedMap
                    (\index blockChild ->
                        List.filterMap removeFormattingFromBlock blockChild
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



-- TESTING


shrink : Shrinker Markdown
shrink =
    Shrink.convert Markdown (\(Markdown markdown) -> markdown) Shrink.string


generator : Random.Generator String -> Random.Generator Markdown
generator =
    Random.map Markdown
