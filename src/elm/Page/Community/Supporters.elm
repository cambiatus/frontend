module Page.Community.Supporters exposing (Model, Msg, init, msgToString, update, view)

import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class)
import Page
import Session.LoggedIn as LoggedIn
import UpdateResult as UR
import View.Components
import View.Sponsorship as Sponsorship



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- TYPES


type Msg
    = RequestedMoreSupporters


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update _ model _ =
    -- TODO - update model
    UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn _ =
    let
        title =
            "Our Supporters"

        content =
            div [ class "flex flex-col flex-grow" ]
                [ Page.viewHeader loggedIn title
                , view_
                ]
    in
    { title = title
    , content = content
    }


view_ : Html Msg
view_ =
    View.Components.infiniteList
        { onRequestedItems = Just RequestedMoreSupporters
        , distanceToRequest = 1000
        , elementToTrack = View.Components.TrackWindow
        }
        []
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
                                -- TODO - Fix text
                                [ span [ class "text-caption text-black uppercase" ] [ text "today" ]
                                , div [ class "divide-y" ]
                                    (List.map (\_ -> Sponsorship.viewSupporter) day)
                                ]
                        )
                )
            ]
        ]



-- UTILS


msgToString : Msg -> List String
msgToString _ =
    -- TODO - Implement new Msgs
    [ "RequestedMoreSupporters" ]
