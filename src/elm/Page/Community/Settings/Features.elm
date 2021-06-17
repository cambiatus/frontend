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
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR
import View.Feedback as Feedback
import View.Form.Toggle


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


type Msg
    = CompletedLoadCommunity Community.Model
    | ClosedAuthModal
    | ToggleShop Bool
    | ToggleObjectives Bool
    | ToggleKyc
    | SaveSuccess


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            t "settings.features.title"

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
                        [ Page.viewHeader loggedIn title
                        , div
                            [ class "container divide-y px-4"
                            ]
                            ([ View.Form.Toggle.init
                                { label = text (t "community.objectives.title_plural")
                                , id = "actions-toggle"
                                , onToggle = ToggleObjectives
                                , disabled = False
                                , value = community.hasObjectives
                                }
                             , View.Form.Toggle.init
                                { label = text (t "menu.shop")
                                , id = "shop-toggle"
                                , onToggle = ToggleShop
                                , disabled = False
                                , value = community.hasShop
                                }
                             , View.Form.Toggle.init
                                { label = text (t "community.kyc.title")
                                , id = "kyc-toggle"
                                , onToggle = \_ -> ToggleKyc
                                , disabled = True
                                , value = community.hasKyc
                                }
                                |> View.Form.Toggle.withTooltip "community.kyc.info"
                             ]
                                |> List.map
                                    (View.Form.Toggle.withAttrs [ class "py-6" ]
                                        >> View.Form.Toggle.toHtml loggedIn.shared.translators
                                    )
                            )
                        ]

                ( RemoteData.Success _, Unauthorized ) ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , div [ class "card" ]
                            [ text (t "community.edit.unauthorized") ]
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

        ClosedAuthModal ->
            UR.init model

        ToggleShop state ->
            { model | hasShop = state }
                |> UR.init
                |> saveFeaturePort loggedIn Shop model.status state
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = ClosedAuthModal }

        ToggleObjectives state ->
            { model | hasObjectives = state }
                |> UR.init
                |> saveFeaturePort loggedIn Objectives model.status state
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = ClosedAuthModal }

        ToggleKyc ->
            model
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
    in
    case ( loggedIn.selectedCommunity, status ) of
        ( RemoteData.Success community, Authorized ) ->
            UR.addPort (saveFeature feature state authorization loggedIn community)

        ( _, Authorized ) ->
            identity

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
            , hasKyc = community.hasKyc
            , hasAutoInvite = community.hasAutoInvite
            , website = Maybe.withDefault "" community.website
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

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        ToggleShop _ ->
            [ "ToggleShop" ]

        ToggleObjectives _ ->
            [ "ToggleObjectives" ]

        ToggleKyc ->
            [ "ToggleKyc" ]

        SaveSuccess ->
            [ "SaveSuccess" ]
