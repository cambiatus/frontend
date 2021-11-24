module Book.Helpers exposing (mockTranslators, mockUsers, viewError)

import Avatar
import Eos.Account
import Html exposing (Html)
import Html.Attributes
import Profile
import Session.Shared as Shared


mockTranslators : Shared.Translators
mockTranslators =
    { t = identity, tr = \x _ -> x }


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


mockUsers : List Profile.Minimal
mockUsers =
    [ { name = Just "Henrique da Cunha Buss"
      , account = Eos.Account.stringToName "henriquebuss"
      , avatar = Avatar.fromString "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/485a572da7c54d26806579866f59145e"
      , email = Just "henrique.buss@hotmail.com"
      , bio = Just "My name is **Henrique**, I work as a *Web Developer* at [cambiatus](https://cambiatus.com) and love Functional Programming. ~~I have edited this description again~~\n"
      , contacts = []
      }
    , { name = Just "Henrique Buss4"
      , account = Eos.Account.stringToName "henriquebus4"
      , avatar = Avatar.fromString "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/ea371c5d6e7843c484c774154f1c4f35"
      , email = Just "henriquebus4@test.com"
      , bio = Just "Isso é um teste 2"
      , contacts = []
      }
    , { name = Just "Julien Lucca"
      , account = Eos.Account.stringToName "lucca"
      , avatar = Avatar.fromString "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/6a8fcc6b1fec4dcfb77cda10a98c8778"
      , email = Just "julienlucca@gmail.com"
      , bio = Just "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. lorem\n"
      , contacts = []
      }
    ]
