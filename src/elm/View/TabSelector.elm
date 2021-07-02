module View.TabSelector exposing
    ( InitialOptions, init
    , withContainerAttrs
    , toHtml
    )

{-| Creates a Cambiatus-style tab selector

    type Tab
        = Analyzed
        | WaitingVote

    analyzedTab =
        { tab = Analyzed, count = Nothing, label = "Analyzed" }

    View.TabSelector.init
        { tabs =
            [ { tab = Analyzed, count = Nothing, label = "Analyzed" }
            , { tab = WaitingVote, count = Nothing, label = "Waiting vote" }
            ]
        , selectedTab = Analyzed
        , onSelectTab = SelectedTab
        }
        |> View.TabSelector.toHtml


# Initializing

@docs InitialOptions, init


# Helpers

@docs withContainerAttrs


# Converting to HTML

@docs toHtml

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)



-- TYPES


type alias TabSelector tab msg =
    { tabs : List (Tab tab)
    , selectedTab : tab
    , onSelectTab : tab -> msg
    , containerAttrs : List (Html.Attribute msg)
    }


type alias Tab tab =
    { tab : tab
    , count : Maybe Int
    , label : String
    }



-- INITIALIZING


type alias InitialOptions tab msg =
    { tabs : List (Tab tab)
    , selectedTab : tab
    , onSelectTab : tab -> msg
    }


init : InitialOptions tab msg -> TabSelector tab msg
init initialOptions =
    { tabs = initialOptions.tabs
    , selectedTab = initialOptions.selectedTab
    , onSelectTab = initialOptions.onSelectTab
    , containerAttrs = []
    }



-- HELPERS


withContainerAttrs : List (Html.Attribute msg) -> TabSelector tab msg -> TabSelector tab msg
withContainerAttrs attrs tabSelector =
    { tabSelector | containerAttrs = tabSelector.containerAttrs ++ attrs }



-- TO HTML


toHtml : TabSelector tab msg -> Html msg
toHtml tabSelector =
    div (class "flex" :: tabSelector.containerAttrs)
        (List.indexedMap
            (\idx ->
                let
                    position =
                        if idx == 0 then
                            Left

                        else if idx == List.length tabSelector.tabs - 1 then
                            Right

                        else
                            Center
                in
                viewTab position tabSelector
            )
            tabSelector.tabs
        )



-- INTERNALS


viewTab : Position -> TabSelector tab msg -> Tab tab -> Html msg
viewTab position tabSelector tab =
    let
        isActive =
            tab.tab == tabSelector.selectedTab

        maybeCount =
            tab.count
                |> Maybe.map (\c -> "(" ++ String.fromInt c ++ ")")
    in
    button
        [ class "text-center py-3 px-8 w-1/2 focus:outline-none flex flex-col items-center justify-center sm:flex-row"
        , classList
            [ ( "bg-orange-300 text-white cursor-default", isActive )
            , ( "bg-gray-100 text-black hover:bg-gray-200", not isActive )
            , ( "rounded-l-full", isLeft position )
            , ( "rounded-r-full", isRight position )
            ]
        , onClick (tabSelector.onSelectTab tab.tab)
        ]
        (case maybeCount of
            Nothing ->
                [ text tab.label ]

            Just count ->
                [ text tab.label
                , span [ class "sm:ml-1" ] [ text count ]
                ]
        )


type Position
    = Left
    | Center
    | Right


isLeft : Position -> Bool
isLeft position =
    case position of
        Left ->
            True

        _ ->
            False


isRight : Position -> Bool
isRight position =
    case position of
        Right ->
            True

        _ ->
            False
