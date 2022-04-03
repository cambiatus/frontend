module Environment exposing (Environment(..), communityDomain, fromUrl)

import Url exposing (Url)


type Environment
    = Development
    | Staging
    | Demo
    | Production


fromUrl : Url -> Environment
fromUrl url =
    if String.endsWith "localhost" url.host then
        Development

    else if String.endsWith ".staging.cambiatus.io" url.host then
        Staging

    else if String.endsWith ".demo.cambiatus.io" url.host then
        Demo

    else if String.endsWith ".cambiatus.io" url.host then
        Production

    else
        Staging


{-| Returns the full `subdomain` of a community based on the current url
-}
communityDomain : Url -> String
communityDomain url =
    String.join "."
        (communitySubdomainParts url
            ++ [ "cambiatus", "io" ]
        )


{-| Get the community subdomain and the current environment, based on current
url. Example possible outputs:

    [ "cambiatus", "staging" ] -- Cambiatus community in the staging environment

    [ "cambiatus", "demo" ] -- Cambiatus community in the demo environment

    [ "cambiatus" ] -- Cambiatus community in the prod environment

-}
communitySubdomainParts : Url -> List String
communitySubdomainParts url =
    let
        allParts =
            url.host |> String.split "."

        isStaging =
            case fromUrl url of
                Development ->
                    True

                Staging ->
                    True

                _ ->
                    False

        addStaging parts =
            if isStaging then
                parts ++ [ "staging" ]

            else
                parts
    in
    case allParts of
        [] ->
            addStaging [ "cambiatus" ]

        [ subdomain ] ->
            addStaging [ subdomain ]

        subdomain :: "localhost" :: _ ->
            [ subdomain, "staging" ]

        subdomain :: "cambiatus" :: _ ->
            [ subdomain ]

        subdomain :: env :: _ ->
            [ subdomain, env ]
