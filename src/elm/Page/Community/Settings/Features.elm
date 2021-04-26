module Page.Community.Settings.Features exposing (Model, Msg, init, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Community
import Eos
import Eos.Account
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (Value)
import Json.Encode
import Page
import Ports
import RemoteData
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR
import View.Feedback as Feedback
import View.Toggle


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( initModel
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )


initModel : Model
initModel =
    { status = Loading
    , hasShop = False
    , hasObjectives = False
    , hasKyc = False
    }


type alias Model =
    { status : Status
    , hasShop : Bool
    , hasObjectives : Bool
    , hasKyc : Bool
    }


type Status
    = Loading
    | Authorized
    | Unauthorized


type Feature
    = Shop
    | Objectives
    | Kyc


type Msg
    = CompletedLoadCommunity Community.Model
    | ToggleShop Bool
    | ToggleObjectives Bool
    | ToggleKyc Bool
    | SaveSuccess


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        translate =
            loggedIn.shared.translators.t

        title =
            translate "settings.features.title"

        content =
            case ( loggedIn.selectedCommunity, model.status ) of
                ( RemoteData.Failure e, _ ) ->
                    Page.fullPageGraphQLError title e

                ( RemoteData.Loading, _ ) ->
                    Page.fullPageLoading loggedIn.shared

                ( RemoteData.NotAsked, _ ) ->
                    Page.fullPageLoading loggedIn.shared

                ( _, Loading ) ->
                    Page.fullPageLoading loggedIn.shared

                ( RemoteData.Success community, Authorized ) ->
                    div [ class "bg-white flex flex-col items-center" ]
                        [ Page.viewHeader loggedIn title Route.CommunitySettings
                        , div
                            [ class "container divide-y px-4"
                            ]
                            ([ View.Toggle.init
                                { label = "community.objectives.title_plural"
                                , id = "actions-toggle"
                                , onToggle = ToggleObjectives
                                , disabled = False
                                , value = community.hasObjectives
                                }
                             , View.Toggle.init
                                { label = "menu.shop"
                                , id = "shop-toggle"
                                , onToggle = ToggleShop
                                , disabled = False
                                , value = community.hasShop
                                }
                             , View.Toggle.init
                                { label = "community.kyc.title"
                                , id = "kyc-toggle"
                                , onToggle = ToggleKyc
                                , disabled = True
                                , value = community.hasKyc
                                }
                                |> View.Toggle.withTooltip "community.kyc.info"
                             ]
                                |> List.map
                                    (View.Toggle.withAttrs [ class "py-6" ]
                                        >> View.Toggle.toHtml loggedIn.shared.translators
                                    )
                            )
                        ]

                ( RemoteData.Success _, Unauthorized ) ->
                    div []
                        [ Page.viewHeader loggedIn title Route.Dashboard
                        , div [ class "card" ]
                            [ text (translate "community.edit.unauthorized") ]
                        ]
    in
    { title = title
    , content = content
    }


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        translate =
            loggedIn.shared.translators.t
    in
    case msg of
        CompletedLoadCommunity community ->
            let
                newStatus =
                    if community.creator == loggedIn.accountName then
                        Authorized

                    else
                        Unauthorized
            in
            UR.init
                { model
                    | status = newStatus
                    , hasShop = community.hasShop
                    , hasObjectives = community.hasObjectives
                    , hasKyc = community.hasKyc
                }

        ToggleShop state ->
            { model | hasShop = state }
                |> UR.init
                |> saveFeaturePort loggedIn Shop model.status state

        ToggleObjectives state ->
            { model | hasObjectives = state }
                |> UR.init
                |> saveFeaturePort loggedIn Objectives model.status state

        ToggleKyc _ ->
            { model | hasKyc = model.hasKyc }
                |> UR.init

        SaveSuccess ->
            let
                addBroadcast uResult =
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            uResult
                                |> UR.addExt
                                    (updateCommunity community model
                                        |> LoggedIn.CommunityLoaded
                                        |> LoggedIn.ExternalBroadcast
                                    )

                        _ ->
                            uResult
            in
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (translate "settings.success"))
                |> addBroadcast


updateCommunity : Community.Model -> Model -> Community.Model
updateCommunity community model =
    { community
        | hasShop = model.hasShop
        , hasObjectives = model.hasObjectives
        , hasKyc = model.hasKyc
    }


saveFeaturePort : LoggedIn.Model -> Feature -> Status -> Bool -> (UR.UpdateResult Model Msg (External Msg) -> UR.UpdateResult Model Msg (External Msg))
saveFeaturePort ({ shared } as loggedIn) feature status state =
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

                Kyc ->
                    ToggleKyc
    in
    case ( loggedIn.selectedCommunity, status ) of
        ( RemoteData.Success community, Authorized ) ->
            if LoggedIn.hasPrivateKey loggedIn then
                UR.addPort (saveFeature feature state authorization loggedIn community)

            else
                UR.addExt (Just (function state) |> LoggedIn.RequiredAuthentication)

        ( _, Authorized ) ->
            UR.addExt (Just (function state) |> LoggedIn.RequiredAuthentication)

        ( _, Loading ) ->
            UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "error.unknown"))

        ( _, Unauthorized ) ->
            UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "error.unknown"))


saveFeature : Feature -> Bool -> Eos.Authorization -> LoggedIn.Model -> Community.Model -> Ports.JavascriptOutModel Msg
saveFeature feature state authorization { shared, accountName } community =
    let
        hasShop =
            case feature of
                Shop ->
                    state

                _ ->
                    community.hasShop

        hasObjectives =
            case feature of
                Objectives ->
                    state

                _ ->
                    community.hasObjectives

        hasKyc =
            case feature of
                Kyc ->
                    state

                _ ->
                    community.hasKyc

        data =
            { accountName = accountName
            , symbol = community.symbol
            , logoUrl = community.logo
            , name = community.name
            , description = community.description
            , subdomain = community.subdomain
            , inviterReward = community.inviterReward
            , invitedReward = community.invitedReward
            , hasShop = hasShop
            , hasObjectives = hasObjectives
            , hasKyc = hasKyc
            }
    in
    { responseAddress = SaveSuccess
    , responseData = Json.Encode.null
    , data =
        Eos.encodeTransaction
            [ { accountName = shared.contracts.community
              , name = "update"
              , authorization = authorization
              , data =
                    data
                        |> Community.createCommunityData
                        |> Community.encodeCreateCommunityData
              }
            ]
    }


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


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
        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        ToggleShop _ ->
            [ "ToggleShop" ]

        ToggleObjectives _ ->
            [ "ToggleObjectives" ]

        ToggleKyc _ ->
            [ "ToggleKyc" ]

        SaveSuccess ->
            [ "SaveSuccess" ]
