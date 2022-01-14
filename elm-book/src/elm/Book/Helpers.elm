module Book.Helpers exposing (Fruit(..), allFruits, fruitToString, mockShared, mockTranslators, mockUsers, viewError)

import Avatar
import Eos.Account
import Html exposing (Html)
import Html.Attributes
import Markdown
import Profile
import Session.Shared as Shared


viewError : List (Html.Attribute msg) -> Bool -> Maybe String -> Html msg
viewError attributes showError maybeError =
    case maybeError of
        Nothing ->
            Html.text ""

        Just error ->
            if showError then
                Html.p (Html.Attributes.class "form-error" :: attributes)
                    [ Html.text error ]

            else
                Html.text ""



-- MOCKING


mockTranslators : Shared.Translators
mockTranslators =
    { t =
        \x ->
            case x of
                "thousands_separator" ->
                    "."

                _ ->
                    x
    , tr = \x _ -> x
    }


mockShared : { endpoints : { api : String }, translators : Shared.Translators }
mockShared =
    { endpoints = { api = "https://staging.cambiatus.io" }
    , translators = mockTranslators
    }


mockUsers : List Profile.Minimal
mockUsers =
    [ { name = Just "Henrique da Cunha Buss"
      , account = Eos.Account.stringToName "henriquebuss"
      , avatar = Avatar.fromString "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/485a572da7c54d26806579866f59145e"
      , email = Just "henrique.buss@hotmail.com"
      , bio = Just (Markdown.fromTranslation mockTranslators "My name is **Henrique**, I work as a *Web Developer* at [cambiatus](https://cambiatus.com) and love Functional Programming. ~~I have edited this description again~~")
      , contacts = []
      }
    , { name = Just "Henrique Buss4"
      , account = Eos.Account.stringToName "henriquebus4"
      , avatar = Avatar.fromString "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/ea371c5d6e7843c484c774154f1c4f35"
      , email = Just "henriquebus4@test.com"
      , bio = Just (Markdown.fromTranslation mockTranslators "Isso é um teste 2")
      , contacts = []
      }
    , { name = Just "Julien Lucca"
      , account = Eos.Account.stringToName "lucca"
      , avatar = Avatar.fromString "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/6a8fcc6b1fec4dcfb77cda10a98c8778"
      , email = Just "julienlucca@gmail.com"
      , bio = Just (Markdown.fromTranslation mockTranslators "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. lorem")
      , contacts = []
      }
    ]


type Fruit
    = Banana
    | Apple
    | Orange
    | Grapes
    | Lemon
    | Mango
    | DragonFruit


allFruits : List Fruit
allFruits =
    [ Banana, Apple, Orange, Grapes, Lemon, Mango, DragonFruit ]


fruitToString : Fruit -> String
fruitToString fruit =
    case fruit of
        Banana ->
            "Banana"

        Apple ->
            "Apple"

        Orange ->
            "Orange"

        Grapes ->
            "Grapes"

        Lemon ->
            "Lemon"

        Mango ->
            "Mango"

        DragonFruit ->
            "DragonFruit"
