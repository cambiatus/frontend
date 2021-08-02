module Page.Community.ThankYou exposing (view)

import Html exposing (Html, a, div, img, p, text)
import Html.Attributes exposing (class, src)
import Session.LoggedIn as LoggedIn



-- VIEW


view : LoggedIn.Model -> { content : Html msg, title : String }
view loggedIn =
    let
        title =
            loggedIn.shared.translators.t "community.thank_you.title"

        text_ =
            loggedIn.shared.translators.t >> text

        content =
            div [ class "bg-green flex-grow text-white text-center md:flex" ]
                [ div [ class "container mx-auto my-10 px-4 flex flex-col items-center space-y-4 md:justify-center" ]
                    [ img [ src "/images/sponsor-celebration.svg" ] []
                    , p [ class "font-bold text-3xl leading-tight" ]
                        [ text_ "community.thank_you.headline"
                        ]
                    , p []
                        [ text_ "community.thank_you.importance" ]
                    , a
                        [ class "underline cursor-pointer"

                        -- TODO - Add CommunitySupporters Route
                        -- , Route.href Route.CommunitySupporters
                        ]
                        [ text_ "community.thank_you.all_supporters" ]
                    ]
                ]
    in
    { content = content, title = title }
