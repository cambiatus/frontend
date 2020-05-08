module Page.Community.Settings.Settings exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Community exposing (Community)
import Eos exposing (Symbol)
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR


init : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
init { shared } symbol =
    ( initModel symbol
    , Api.Graphql.query shared (Community.communityQuery symbol) CompletedLoad
    )


type alias Model =
    { currency : String
    , status : Status
    }


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Community))
    | Loaded Community


type Msg
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Community)) (Maybe Community))


initModel : Symbol -> Model
initModel symbol =
    { currency = "MUDA"
    , status = Loading
    }


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    div []
        [ Page.viewHeader loggedIn "Edit community" Route.Dashboard
        , view_ model
        ]


view_ : Model -> Html Msg
view_ model =
    div
        [ class "grid my-4"
        , style "grid-template-columns" "0 1fr 0"
        , style "grid-template-rows" "auto"
        , style "grid-gap" "16px"
        ]
        [ settingCard "Community information" "Logo, name, description"
        , settingCard "Currency" "MUDA"
        , settingCard "Objectives and Actions" ""
        , settingCard "Team" "Team building"
        , settingCard "Features" "Actions, shop"
        ]


settingCard : String -> String -> Html Msg
settingCard title description =
    div
        [ class "flex flex-col justify-around bg-white w-full h-32 rounded px-4 pt-3 pb-4"
        , style "grid-column" "2 / 3"
        ]
        [ span [ class "text-sm font-medium" ] [ text title ]
        , span [ class "text-xs text-gray-900 uppercase" ] [ text description ]
        , button
            [ class "w-full bg-orange-300 rounded-lg text-sm uppercase text-white font-medium h-8"
            ]
            [ text "Edit" ]
        ]


featuresView : Html Msg
featuresView =
    div [] [ text "Features!" ]



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoad (Ok (Just community)) ->
            UR.init { model | status = Loaded community }

        CompletedLoad (Ok Nothing) ->
            -- TODO: community not found
            UR.init model

        CompletedLoad (Err err) ->
            UR.init { model | status = LoadingFailed err }
                |> UR.logGraphqlError msg err


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]
