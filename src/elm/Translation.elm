module Translation exposing (get)

import Api
import Flags exposing (Endpoints)
import Http
import I18Next exposing (Translations)
import Url.Builder exposing (QueryParameter)


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

            else
                "en-US.json"
    in
    Http.get
        { url = Url.Builder.absolute [ "translations", translation ] []
        , expect = Http.expectJson toMsg I18Next.translationsDecoder
        }
