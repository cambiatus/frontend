module Page.Community.Settings.Features exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Community
import Eos exposing (Symbol)
import Eos.Account
import Graphql.Http
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (checked, class, for, id, name, style, type_)
import Html.Events exposing (onCheck)
import I18Next exposing (Translations, t)
import Json.Encode
import Page
import Ports
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
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
    , hasShop = False
    , hasObjectives = False
    }


type alias Model =
    { status : Status
    , symbol : Symbol
    , hasShop : Bool
    , hasObjectives : Bool
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Community.Model))
    | Loaded Community.Model


type Feature
    = Shop
    | Objectives


type Msg
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | ToggleShop Bool
    | ToggleActions Bool
    | Ignored


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
            [ toggleView translations (translate "community.objectives.title_plural") model.hasObjectives ToggleActions "actions"
            , toggleView translations (translate "menu.shop") model.hasShop ToggleShop "shop"
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
                                'label status toggle' 40px / auto 80px 50px
                                """
        ]
        [ span [ classes, style "grid-area" "label" ] [ text labelText ]
        , span [ classes, class "text-purple-500 font-medium lowercase mr-auto", style "grid-area" "status" ] [ text statusText ]
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
update msg model loggedIn =
    case msg of
        CompletedLoad (Ok (Just community)) ->
            UR.init
                { model
                    | status = Loaded community
                    , hasShop = community.hasShop
                    , hasObjectives = community.hasObjectives
                }

        CompletedLoad (Ok Nothing) ->
            UR.init model

        CompletedLoad (Err err) ->
            UR.init { model | status = LoadingFailed err }
                |> UR.logGraphqlError msg err

        ToggleShop state ->
            { model | hasShop = state }
                |> UR.init
                |> saveFeaturePort loggedIn Shop model.status state

        ToggleActions state ->
            { model | hasObjectives = state }
                |> UR.init
                |> saveFeaturePort loggedIn Objectives model.status state

        Ignored ->
            UR.init model


saveFeaturePort : LoggedIn.Model -> Feature -> Status -> Bool -> (UR.UpdateResult Model Msg (External Msg) -> UR.UpdateResult Model Msg (External Msg))
saveFeaturePort loggedIn feature status state =
    let
        authorization =
            { actor = loggedIn.accountName
            , permissionName = Eos.Account.samplePermission
            }
    in
    case status of
        Loaded community ->
            if LoggedIn.isAuth loggedIn then
                UR.addPort (saveFeature feature state authorization loggedIn.accountName community)

            else
                UR.addExt (Just (ToggleShop state) |> LoggedIn.RequiredAuthentication)

        Loading ->
            UR.addExt (ShowFeedback Failure "Error")

        LoadingFailed _ ->
            UR.addExt (ShowFeedback Failure "Error")


saveFeature : Feature -> Bool -> Eos.Authorization -> Eos.Account.Name -> Community.Model -> Ports.JavascriptOutModel Msg
saveFeature feature state authorization accountName community =
    let
        hasShop =
            case feature of
                Shop ->
                    state

                Objectives ->
                    community.hasObjectives

        hasObjectives =
            case feature of
                Shop ->
                    community.hasShop

                Objectives ->
                    state

        data =
            { accountName = accountName
            , symbol = community.symbol
            , logoHash = community.logo
            , name = community.title
            , description = community.description
            , inviterReward = community.inviterReward
            , invitedReward = community.invitedReward
            , hasShop = hasShop
            , hasObjectives = hasObjectives
            }
    in
    { responseAddress = Ignored
    , responseData = Json.Encode.null
    , data =
        Eos.encodeTransaction
            [ { accountName = "bes.cmm"
              , name = "update"
              , authorization = authorization
              , data =
                    data
                        |> Community.createCommunityData
                        |> Community.encodeCreateCommunityData
              }
            ]
    }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]

        ToggleShop _ ->
            [ "ToggleShop" ]

        ToggleActions _ ->
            [ "ToggleActions" ]

        Ignored ->
            [ "Ignored" ]
