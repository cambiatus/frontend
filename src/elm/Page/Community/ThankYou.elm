module Page.Community.ThankYou exposing (view)

import Html exposing (Html, a, div, img, p, text)
import Html.Attributes exposing (class, src)
import Markdown
import Page
import RemoteData
import Route
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
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    div [ class "bg-green flex-grow text-white text-center md:flex" ]
                        [ div [ class "container mx-auto my-10 px-4 flex flex-col items-center space-y-4 md:justify-center" ]
                            [ img [ src "/images/sponsor-celebration.svg" ] []
                            , p [ class "font-bold text-3xl leading-tight" ]
                                [ community.contributionConfiguration
                                    |> Maybe.andThen .thankYouTitle
                                    |> Maybe.withDefault
                                        (loggedIn.shared.translators.tr
                                            "sponsorship.thank_you_message.default_title"
                                            [ ( "community", community.name ) ]
                                        )
                                    |> text
                                ]
                            , p []
                                [ community.contributionConfiguration
                                    |> Maybe.andThen .thankYouDescription
                                    |> Maybe.withDefault
                                        (Markdown.fromTranslation
                                            loggedIn.shared.translators
                                            "sponsorship.thank_you_message.default_message"
                                        )
                                    |> Markdown.view []
                                ]
                            , a
                                [ class "underline cursor-pointer"
                                , Route.href Route.CommunitySupporters
                                ]
                                [ text_ "community.thank_you.all_supporters" ]
                            ]
                        ]

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { content = content
    , title = title
    }
