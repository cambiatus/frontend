module NewAppCallout exposing (buttonClass, isVisible, view, viewArrowIcon, viewDoggo, viewEmphasised, viewEyebrow, viewSpark)

{-| Band at the top of the dashboard inviting MUDA members to the new app (the LiveView
app at /app), and reminding them to keep their 12 words: the new app signs in with them.

The copy announces MUDA's launch, so it only shows in that community. It is permanent:
every visit shows it, and it has no close button, since members are moving to the new app.

The pieces below `view` (doggo, eyebrow, sparks, button) are the new app's invitation look,
shared with `ShopMoved` so every invitation to the new app reads as the same voice.

-}

import Eos
import Html exposing (Html, a, div, h2, img, p, section, span, strong, text)
import Html.Attributes exposing (alt, attribute, class, href, id, src, target)
import ReviewMoved
import Route
import Svg exposing (Svg, svg)
import Svg.Attributes as SvgAttr
import Translation


isVisible : Eos.Symbol -> Bool
isVisible symbol =
    Eos.symbolToSymbolCodeString symbol == "MUDA"


view : Translation.Translators -> { apiUrl : String } -> Html msg
view { t } { apiUrl } =
    section
        [ id "new-app-callout"
        , class "relative overflow-hidden text-white bg-gradient-to-r from-[#2E4A2C] to-[#3F6B3A]"
        , attribute "aria-label" (t "dashboard.new_app.label")
        ]
        [ viewSpark "left-[18%] top-[10px] text-[#FFE400]"
        , viewSpark "left-[62%] bottom-2 text-[#5AD1C9]"
        , div [ class "container mx-auto relative flex flex-col gap-4 p-6 md:flex-row md:items-center md:gap-6 md:py-8 md:px-8" ]
            [ viewDoggo "w-14 md:w-[72px] self-start md:self-auto"
            , div [ class "flex-1 min-w-0" ]
                [ viewEyebrow (t "dashboard.new_app.eyebrow")
                , h2 [ class "text-lg font-bold" ] [ text (t "dashboard.new_app.title") ]
                , p [ class "mt-1 text-[#D7E6D2]" ] [ text (t "dashboard.new_app.body") ]
                , p [ class "mt-2 flex items-start gap-2" ]
                    [ viewWarningIcon
                    , span []
                        (viewEmphasised (t "dashboard.new_app.words_warning")
                            ++ [ text " "
                               , a
                                    [ id "new-app-callout-words"
                                    , Route.href Route.Settings
                                    , class "font-semibold underline focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-[#FFE400]"
                                    ]
                                    [ text (t "dashboard.new_app.words_link") ]
                               ]
                        )
                    ]
                ]

            -- target "_self" is required: without it Elm routes this same-host URL
            -- inside the SPA (Not Found), see `Page.AdminMoved`.
            , a
                [ id "new-app-callout-go"
                , href (ReviewMoved.appUrl apiUrl "/app")
                , target "_self"
                , class (buttonClass ++ " md:w-auto")
                ]
                [ text (t "dashboard.new_app.action")
                , viewArrowIcon
                ]
            ]
        ]


viewDoggo : String -> Html msg
viewDoggo class_ =
    img
        [ src "/images/new-app-doggo.png"
        , alt ""
        , class ("h-auto flex-none transform rotate-[-6deg] " ++ class_)
        ]
        []


viewEyebrow : String -> Html msg
viewEyebrow label =
    span [ class "inline-flex items-center gap-1 mb-2 px-2.5 py-0.5 rounded-full bg-[#FFE400] text-[#3A3000] text-sm font-bold uppercase tracking-wide" ]
        [ span [ attribute "aria-hidden" "true" ] [ text "✦" ]
        , text label
        ]


{-| The white call to action; pair it with `viewArrowIcon`.
-}
buttonClass : String
buttonClass =
    "inline-flex flex-none items-center justify-center gap-2 w-full min-h-[48px] px-5 py-3 rounded bg-white text-[#2E4A2C] font-bold whitespace-nowrap focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-[#FFE400]"


{-| Translations mark the words to stress with `**`, as in "your **12 words**".
-}
viewEmphasised : String -> List (Html msg)
viewEmphasised translation =
    String.split "**" translation
        |> List.indexedMap
            (\index part ->
                if modBy 2 index == 1 then
                    strong [] [ text part ]

                else
                    text part
            )


viewSpark : String -> Svg msg
viewSpark class_ =
    svg
        [ SvgAttr.class ("absolute w-3.5 h-3.5 fill-current opacity-90 " ++ class_)
        , SvgAttr.viewBox "0 0 24 24"
        , attribute "aria-hidden" "true"
        ]
        [ Svg.path [ SvgAttr.d "M12 0c.6 6 2.4 9.4 12 12-9.6 2.6-11.4 6-12 12-.6-6-2.4-9.4-12-12C9.6 9.4 11.4 6 12 0z" ] [] ]


viewWarningIcon : Svg msg
viewWarningIcon =
    viewStrokeIcon "w-4 h-4 mt-0.5 text-[#FFE400]"
        [ "M2.5 18 10.3 4.3a2 2 0 0 1 3.4 0L21.5 18a2 2 0 0 1-1.7 3H4.2a2 2 0 0 1-1.7-3Z"
        , "M12 9v4M12 17h.01"
        ]


viewArrowIcon : Svg msg
viewArrowIcon =
    viewStrokeIcon "w-[18px] h-[18px]" [ "M5 12h14M13 6l6 6-6 6" ]


viewStrokeIcon : String -> List String -> Svg msg
viewStrokeIcon class_ paths =
    svg
        [ SvgAttr.class ("flex-none " ++ class_)
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        , attribute "aria-hidden" "true"
        ]
        (List.map (\path -> Svg.path [ SvgAttr.d path ] []) paths)
