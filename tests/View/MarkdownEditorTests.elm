module View.MarkdownEditorTests exposing (all)

import Expect
import Test exposing (..)
import TestHelpers.Fuzz as Fuzz
import View.MarkdownEditor as MarkdownEditor


all : Test
all =
    describe "MarkdownEditor"
        [ markdownCompatibility
        ]



-- MARKDOWN COMPATIBILITY


markdownCompatibility : Test
markdownCompatibility =
    describe "markdownCompatibility"
        [ fuzz Fuzz.markdownString "Parsing markdown should result in Ok" <|
            \fuzzMarkdownString ->
                MarkdownEditor.quillOpFromMarkdown fuzzMarkdownString
                    |> Expect.ok
        , fuzz Fuzz.markdownString "Parsing and reading markdown shouldn't change the content" <|
            \fuzzMarkdownString ->
                MarkdownEditor.quillOpFromMarkdown fuzzMarkdownString
                    |> Result.map (List.map MarkdownEditor.quillOpToMarkdown >> String.concat >> String.trim)
                    |> Expect.equal (Ok fuzzMarkdownString)
        ]
