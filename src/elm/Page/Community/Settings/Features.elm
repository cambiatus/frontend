module Page.Community.Settings.Features exposing (Model, Msg, init, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Api.Graphql
import Cambiatus.Mutation
import Community
import Dict
import Eos
import Eos.Account
import Form.Toggle
import Graphql.SelectionSet
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (Value)
import Json.Encode as Encode
import Log
import Maybe.Extra
import Page
import Ports
import RemoteData
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR
import View.Feedback as Feedback


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
    , hasNews = False
    }


type alias Model =
    { status : Status
    , hasShop : Bool
    , hasObjectives : Bool
    , hasKyc : Bool
    , hasNews : Bool
    }


type Status
    = Loading
    | Authorized
    | Unauthorized


type Feature
    = Shop
    | Objectives


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | ClosedAuthModal
    | ToggleShop Bool
    | ToggleObjectives Bool
    | ToggleKyc
    | ToggleSponsorship
    | ToggleNews Bool
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
                    let
                        addTooltip maybeTooltip =
                            case maybeTooltip of
                                Nothing ->
                                    identity

                                Just tooltip ->
                                    Form.Toggle.withTooltip tooltip

                        viewToggle { label, id, action, disabled, value, tooltip } =
                            Form.Toggle.init { label = text <| t label, id = id }
                                |> Form.Toggle.withDisabled disabled
                                |> Form.Toggle.withContainerAttrs [ class "py-6" ]
                                |> addTooltip tooltip
                                |> (\options ->
                                        Form.Toggle.view options
                                            { onToggle = action
                                            , onBlur = NoOp
                                            , value = value
                                            , error = text ""
                                            , hasError = False
                                            , isRequired = False
                                            , translators = loggedIn.shared.translators
                                            }
                                   )
                    in
                    div [ class "bg-white flex flex-col items-center" ]
                        [ Page.viewHeader loggedIn title
                        , div
                            [ class "container divide-y px-4"
                            ]
                            ([ { label = "community.objectives.title_plural"
                               , id = "actions-toggle"
                               , action = ToggleObjectives
                               , disabled = False
                               , value = community.hasObjectives
                               , tooltip = Nothing
                               }
                             , { label = "menu.shop"
                               , id = "shop-toggle"
                               , action = ToggleShop
                               , disabled = False
                               , value = community.hasShop
                               , tooltip = Nothing
                               }
                             , { label = "community.kyc.title"
                               , id = "kyc-toggle"
                               , action = \_ -> ToggleKyc
                               , disabled = True
                               , value = community.hasKyc
                               , tooltip =
                                    Just
                                        { message = t "community.kyc.info"
                                        , iconClass = "text-orange-300"
                                        }
                               }
                             , { label = "sponsorship.title"
                               , id = "sponsorship-toggle"
                               , action = \_ -> ToggleSponsorship
                               , disabled = True
                               , value = Maybe.Extra.isJust community.contributionConfiguration
                               , tooltip = Nothing
                               }
                             , { label = "news.title"
                               , id = "news-toggle"
                               , action = ToggleNews
                               , disabled = False
                               , value = model.hasNews
                               , tooltip = Nothing
                               }
                             ]
                                |> List.map viewToggle
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
        NoOp ->
            UR.init model

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
                    , hasNews = community.hasNews
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

        ToggleSponsorship ->
            model
                |> UR.init

        ToggleNews newsValue ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    { model | hasNews = newsValue }
                        |> UR.init
                        |> UR.addCmd
                            (Api.Graphql.mutation loggedIn.shared
                                (Just loggedIn.authToken)
                                (Cambiatus.Mutation.hasNews
                                    { communityId = Eos.symbolToString community.symbol
                                    , hasNews = newsValue
                                    }
                                    Graphql.SelectionSet.empty
                                )
                                (\_ -> SaveSuccess)
                            )

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried toggling community news feature, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.Features", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

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
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = msg
                    , message = "Saved community feature"
                    , data =
                        Dict.fromList
                            [ ( "hasShop", Encode.bool model.hasShop )
                            , ( "hasObjectives", Encode.bool model.hasObjectives )
                            , ( "hasKyc", Encode.bool model.hasKyc )
                            ]
                    , level = Log.DebugLevel
                    }


updateCommunity : Community.Model -> Model -> Community.Model
updateCommunity community model =
    { community
        | hasShop = model.hasShop
        , hasObjectives = model.hasObjectives
        , hasKyc = model.hasKyc
        , hasNews = model.hasNews
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
    , responseData = Encode.null
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
        NoOp ->
            [ "NoOp" ]

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

        ToggleSponsorship ->
            [ "ToggleSponsorship" ]

        ToggleNews _ ->
            [ "ToggleNews" ]

        SaveSuccess ->
            [ "SaveSuccess" ]
