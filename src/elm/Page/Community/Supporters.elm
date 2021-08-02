module Page.Community.Supporters exposing (Model, Msg, init, msgToString, update, view)

import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class)
import Page
import Session.LoggedIn as LoggedIn
import UpdateResult as UR



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- TYPES


type Msg
    = Ignored


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update _ model _ =
    UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn _ =
    let
        title =
            "Our Supporters"

        content =
            div [ class "flex flex-col flex-grow" ]
                (Page.viewHeader loggedIn title
                    :: view_
                )
    in
    { title = title
    , content = content
    }


view_ : List (Html Msg)
view_ =
    [ div [ class "container mx-auto px-4" ]
        [ p [ class "text-heading py-4" ]
            [ span [ class "text-gray-900" ] [ text "Our " ]
            , span [ class "text-indigo-500 font-bold" ] [ text "Supporters" ]
            ]
        ]
    , div [ class "w-full bg-white flex-grow pt-5" ]
        [ div [ class "container mx-auto px-4 space-y-4" ]
            (List.repeat 3 ()
                |> List.repeat 2
                |> List.map
                    (\day ->
                        div []
                            [ span [ class "text-caption text-black uppercase" ] [ text "today" ]
                            , div [ class "divide-y" ] (List.map (\_ -> viewSupporter) day)
                            ]
                    )
            )
        ]
    ]


viewSupporter : Html Msg
viewSupporter =
    div [ class "flex py-3" ]
        [ div [ class "bg-gray-500 rounded-full w-14 h-14" ] []
        , div [ class "ml-4" ]
            [ p [ class "font-bold text-sm md:text-base" ] [ text "Cecília Braga" ]
            , p [ class "text-green" ]
                [ span [ class "text-heading font-bold mr-1" ] [ text "10" ]
                , span [ class "uppercase text-caption md:text-xs" ] [ text "dollars" ]
                ]
            ]
        , span [ class "ml-auto uppercase text-caption text-gray-900" ] [ text "22 feb 2021" ]
        ]



-- UTILS


msgToString : Msg -> List String
msgToString _ =
    -- TODO
    Debug.todo "Implement msgToString"
