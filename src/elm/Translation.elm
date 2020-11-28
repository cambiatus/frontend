module Translation exposing (get)

import Http
import I18Next exposing (Translations)
import Url.Builder


get : String -> (Result Http.Error Translations -> msg) -> Cmd msg
get language toMsg =
    let
        translation =
            if String.startsWith "es" language then
                "es-ES.json"

            else if String.startsWith "pt" language then
                "pt-BR.json"

            else if String.startsWith "cat" language then
                "cat-CAT.json"

            else if String.startsWith "amh" language then
                "amh-ETH.json"

            else
                "en-US.json"
    in
    Http.get
        { url = Url.Builder.absolute [ "translations", translation ] []
        , expect = Http.expectJson toMsg I18Next.translationsDecoder
        }
