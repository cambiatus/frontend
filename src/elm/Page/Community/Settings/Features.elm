module Page.Community.Settings.Features exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Community
import Eos exposing (Symbol)
import Graphql.Http
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (checked, class, for, id, name, style, type_)
import Html.Events exposing (onCheck)
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR


init : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
init { shared } symbol =
    ( initModel symbol
    , Api.Graphql.query shared (Community.communityQuery symbol) CompletedLoad
    )


initModel : Symbol -> Model
initModel symbol =
    { status = Loading
    , symbol = symbol
    , actions = False
    , shop = False
    , test = ""
    }


type alias Model =
    { status : Status
    , symbol : Symbol
    , actions : Bool
    , shop : Bool
    , test : String
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Community.Model))
    | Loaded Community.Model


type Msg
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | ToggleActions Bool
    | ToggleShop Bool


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    div [ class "bg-white flex flex-col items-center" ]
        [ Page.viewHeader loggedIn "Features" (Route.CommunitySettings model.symbol)
        , div
            [ class "container w-full divide-y"
            ]
            [ toggleView "Actions" model.actions ToggleActions "actions"
            , toggleView "Shop" model.shop ToggleShop "shop"
            ]
        ]


toggleView : String -> Bool -> (Bool -> Msg) -> String -> Html Msg
toggleView labelText isEnabled toggleFunction inputId =
    let
        classes =
            class "flex items-center"

        statusText =
            if isEnabled then
                "Enabled"

            else
                "Disabled"
    in
    div
        [ class "grid w-full py-4"
        , style "grid-template" """
                                'label status toggle' 40px / auto 100px 50px
                                """
        ]
        [ span [ classes, style "grid-area" "label" ] [ text labelText ]
        , span [ classes, class "text-purple-500 font-medium lowercase", style "grid-area" "status" ] [ text statusText ]
        , div [ classes ]
            [ div [ class "form-switch inline-block align-middle" ]
                [ input
                    [ type_ "checkbox"
                    , id inputId
                    , name inputId
                    , class "form-switch-checkbox"
                    , checked isEnabled
                    , onCheck toggleFunction
                    ]
                    []
                , label [ class "form-switch-label", for inputId ] []
                ]
            ]
        ]


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model _ =
    case msg of
        CompletedLoad (Ok (Just community)) ->
            UR.init { model | status = Loaded community, actions = community.hasActions, shop = community.hasShop }

        CompletedLoad (Ok Nothing) ->
            UR.init model

        CompletedLoad (Err err) ->
            UR.init { model | status = LoadingFailed err }
                |> UR.logGraphqlError msg err

        ToggleActions state ->
            UR.init { model | actions = state }

        ToggleShop state ->
            UR.init { model | shop = state }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]

        ToggleActions _ ->
            [ "ToggleActions" ]

        ToggleShop _ ->
            [ "ToggleShop" ]
