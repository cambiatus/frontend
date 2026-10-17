module Page.AdminMoved exposing (loginUrl, view, viewContent)

{-| Shown on every old Elm community-settings route. Community administration moved to
the LiveView panel at /app; this page explains that and sends the admin to its login.
-}

import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (class, href, id, target)
import Page
import Route
import Translation


view : Page.Session -> { title : String, content : Html msg }
view session =
    let
        shared =
            Page.toShared session
    in
    { title = shared.translators.t "admin_moved.title"
    , content = viewContent shared.translators (loginUrl shared.endpoints.api)
    }


{-| The /app login lives on the same Phoenix server as the API.
-}
loginUrl : String -> String
loginUrl apiUrl =
    (if String.endsWith "/" apiUrl then
        String.dropRight 1 apiUrl

     else
        apiUrl
    )
        ++ "/app/login"


viewContent : Translation.Translators -> String -> Html msg
viewContent { t } url =
    div [ class "container mx-auto px-4 py-16 md:py-24 flex flex-col items-center text-center" ]
        [ h1 [ class "text-2xl font-bold text-black" ] [ text (t "admin_moved.title") ]
        , p [ class "mt-4 max-w-md text-gray-900" ] [ text (t "admin_moved.body") ]
        , p [ class "mt-2 max-w-md text-gray-900" ] [ text (t "admin_moved.login_hint") ]

        -- target "_self" is required: without it Elm routes this same-host URL inside
        -- the SPA (Not Found) instead of loading the Phoenix login page.
        , a
            [ id "admin-moved-login"
            , href url
            , target "_self"
            , class "button button-primary w-full md:w-auto mt-8"
            ]
            [ text (t "admin_moved.go") ]
        , a [ Route.href Route.Dashboard, class "button button-secondary w-full md:w-auto mt-4" ]
            [ text (t "admin_moved.back") ]
        ]
