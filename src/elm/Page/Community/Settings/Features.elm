module Page.Community.Settings.Features exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Api.Graphql
import Community
import Eos exposing (Symbol)
import Eos.Account
import Graphql.Http
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (checked, class, for, id, name, style, type_)
import Html.Events exposing (onCheck)
import I18Next exposing (Translations, t)
import Json.Decode exposing (Value)
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
    | Unauthorized


type Feature
    = Shop
    | Objectives


type Msg
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | ToggleShop Bool
    | ToggleObjectives Bool
    | SaveSuccess


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        translations =
            loggedIn.shared.translations

        translate =
            t translations

        title =
            translate "settings.features.title"

        content =
            case model.status of
                Loaded _ ->
                    div [ class "bg-white flex flex-col items-center" ]
                        [ Page.viewHeader loggedIn title (Route.CommunitySettings model.symbol)
                        , div
                            [ class "container divide-y px-4"
                            ]
                            [ toggleView translations (translate "community.objectives.title_plural") model.hasObjectives ToggleObjectives "actions"
                            , toggleView translations (translate "menu.shop") model.hasShop ToggleShop "shop"
                            ]
                        ]

                LoadingFailed error ->
                    Page.fullPageGraphQLError title error

                Loading ->
                    Page.fullPageLoading

                Unauthorized ->
                    div []
                        [ Page.viewHeader loggedIn title Route.Dashboard
                        , div [ class "card" ]
                            [ text (translate "community.edit.unauthorized") ]
                        ]
    in
    { title = title
    , content = content
    }


toggleView : Translations -> String -> Bool -> (Bool -> Msg) -> String -> Html Msg
toggleView translations labelText isEnabled toggleFunction inputId =
    let
        translate =
            t translations

        classes =
            class "flex items-center text-sm"

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
    let
        translate =
            t loggedIn.shared.translations
    in
    case msg of
        CompletedLoad (Ok (Just community)) ->
            let
                newStatus =
                    if community.creator == loggedIn.accountName then
                        Loaded community

                    else
                        Unauthorized
            in
            UR.init
                { model
                    | status = newStatus
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

        ToggleObjectives state ->
            { model | hasObjectives = state }
                |> UR.init
                |> saveFeaturePort loggedIn Objectives model.status state

        SaveSuccess ->
            model
                |> UR.init
                |> UR.addExt (ShowFeedback Success (translate "settings.success"))


saveFeaturePort : LoggedIn.Model -> Feature -> Status -> Bool -> (UR.UpdateResult Model Msg (External Msg) -> UR.UpdateResult Model Msg (External Msg))
saveFeaturePort loggedIn feature status state =
    let
        authorization =
            { actor = loggedIn.accountName
            , permissionName = Eos.Account.samplePermission
            }

        function =
            case feature of
                Shop ->
                    ToggleShop

                Objectives ->
                    ToggleObjectives
    in
    case status of
        Loaded community ->
            if LoggedIn.isAuth loggedIn then
                UR.addPort (saveFeature feature state authorization loggedIn.accountName community)

            else
                UR.addExt (Just (function state) |> LoggedIn.RequiredAuthentication)

        Loading ->
            UR.addExt (ShowFeedback Failure "Error")

        LoadingFailed _ ->
            UR.addExt (ShowFeedback Failure "Error")

        Unauthorized ->
            UR.addExt (ShowFeedback Failure "Error")


saveFeature : Feature -> Bool -> Eos.Authorization -> Eos.Account.Name -> Community.Model -> Ports.JavascriptOutModel Msg
saveFeature feature state authorization accountName community =
    let
        hasShop =
            case feature of
                Shop ->
                    state

                Objectives ->
                    community.hasShop

        hasObjectives =
            case feature of
                Shop ->
                    community.hasObjectives

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
    { responseAddress = SaveSuccess
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


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr _ =
    case addr of
        "SaveSuccess" :: _ ->
            Just SaveSuccess

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]

        ToggleShop _ ->
            [ "ToggleShop" ]

        ToggleObjectives _ ->
            [ "ToggleObjectives" ]

        SaveSuccess ->
            [ "SaveSuccess" ]
