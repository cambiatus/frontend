module Page.Community.ThankYou exposing (view)

import Html exposing (Html, a, div, img, p, text)
import Html.Attributes exposing (class, src)
import Session.LoggedIn as LoggedIn



-- VIEW


view : LoggedIn.Model -> { content : Html msg, title : String }
view loggedIn =
    let
        -- TODO - I18N
        title =
            "Thank you!"

        content =
            div [ class "bg-green flex-grow text-white text-center md:flex" ]
                [ div [ class "container mx-auto my-10 px-4 flex flex-col items-center space-y-4 md:justify-center" ]
                    [ img [ src "/images/sponsor-celebration.svg" ] []
                    , p [ class "font-bold text-3xl leading-tight" ]
                        [ text "Thank you for sponsoring this community!"
                        ]
                    , p []
                        [ text "Your contribution is very important for the people in this community." ]
                    , a
                        [ class "underline"

                        -- TODO - Add CommunitySupporters Route
                        -- , Route.href Route.CommunitySupporters
                        ]
                        [ text "See the list of all supporters" ]
                    ]
                ]
    in
    { content = content, title = title }
