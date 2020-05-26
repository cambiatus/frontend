module Page.Community.Settings.Features exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Community
import Eos exposing (Symbol)
import Feature exposing (Feature, FeatureFlags(..))
import Graphql.Http
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (checked, class, for, id, name, style, type_)
import Html.Events exposing (onCheck)
import I18Next exposing (Translations, t)
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
    , features = FeatureFlags []
    }


type alias Model =
    { status : Status
    , symbol : Symbol
    , features : FeatureFlags
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Community.Model))
    | Loaded Community.Model


type Msg
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | ToggleFeature Feature Bool


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    let
        translations =
            loggedIn.shared.translations

        translate =
            t translations
    in
    div [ class "bg-white flex flex-col items-center" ]
        [ Page.viewHeader loggedIn "Features" (Route.CommunitySettings model.symbol)
        , div
            [ class "container w-full divide-y"
            ]
            [ toggleView translations (translate "objectives.title_plural") (model.features |> Feature.has Feature.Actions) (ToggleFeature Feature.Actions) "actions"
            , toggleView translations (translate "menu.shop") (model.features |> Feature.has Feature.Shop) (ToggleFeature Feature.Shop) "shop"
            ]
        ]


toggleView : Translations -> String -> Bool -> (Bool -> Msg) -> String -> Html Msg
toggleView translations labelText isEnabled toggleFunction inputId =
    let
        translate =
            t translations

        classes =
            class "flex items-center"

        statusText =
            if isEnabled then
                translate "settings.features.enabled"

            else
                translate "settings.features.disabled"
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
            UR.init
                { model
                    | status = Loaded community
                    , features =
                        model.features
                            |> (if community.hasActions then
                                    Feature.add Feature.Actions

                                else
                                    \r -> r
                               )
                            |> (if community.hasShop then
                                    Feature.add Feature.Shop

                                else
                                    \r -> r
                               )
                }

        CompletedLoad (Ok Nothing) ->
            UR.init model

        CompletedLoad (Err err) ->
            UR.init { model | status = LoadingFailed err }
                |> UR.logGraphqlError msg err

        ToggleFeature feature state ->
            UR.init
                { model
                    | features =
                        model.features
                            |> toggleFeature state feature
                }


toggleFeature : Bool -> Feature -> FeatureFlags -> FeatureFlags
toggleFeature state feature featureFlags =
    featureFlags
        |> (if state == True then
                Feature.add feature

            else
                Feature.remove feature
           )


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]

        ToggleFeature _ _ ->
            [ "ToggleFeature" ]
