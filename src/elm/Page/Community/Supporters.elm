module Page.Community.Supporters exposing (Model, Msg, init, msgToString, update, view)

import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class)
import Page
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Time
import UpdateResult as UR
import View.Components



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
            [ "community.index.our_supporters"
            , "community.index.supporters"
            ]
                |> List.map loggedIn.shared.translators.t
                |> String.join " "

        content =
            div [ class "flex flex-col flex-grow" ]
                [ Page.viewHeader loggedIn title
                , view_ loggedIn.shared
                ]
    in
    { title = title
    , content = content
    }


view_ : Shared -> Html Msg
view_ shared =
    let
        { t } =
            shared.translators
    in
    View.Components.infiniteList
        { onRequestedItems = Just RequestedMoreSupporters
        , distanceToRequest = 1000
        , elementToTrack = View.Components.TrackWindow
        }
        []
        [ div [ class "container mx-auto px-4" ]
            [ p [ class "text-heading py-4" ]
                [ span [ class "text-gray-900" ] [ text <| t "community.index.our_supporters" ]
                , text " "
                , span [ class "text-indigo-500 font-bold" ] [ text <| t "community.index.supporters" ]
                ]
            ]
        , div [ class "w-full bg-white flex-grow pt-5" ]
            [ div [ class "container mx-auto px-4 space-y-4" ]
                (List.repeat 3 ()
                    |> List.repeat 2
                    |> List.map
                        (\day ->
                            div []
                                [ View.Components.dateViewer [ class "text-caption text-black uppercase" ]
                                    identity
                                    shared
                                    (Time.millisToPosix 123123)
                                , div [ class "divide-y" ]
                                    (List.map (\_ -> viewSupporter) day)
                                ]
                        )
                )
            ]
        ]


viewSupporter : Html msg
viewSupporter =
    -- TODO - Use supporter
    div [ class "flex items-center py-4" ]
        [ div [ class "bg-gray-500 rounded-full w-14 h-14" ] []
        , p [ class "ml-4 font-bold text-black" ] [ text "Cecília Braga" ]
        ]



-- UTILS


msgToString : Msg -> List String
msgToString _ =
    -- TODO - Implement new Msgs
    [ "RequestedMoreSupporters" ]
