module Page.Community.Settings.Multisig exposing (Model, Msg, init, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Community
import Eos
import Eos.Account
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Page
import RemoteData
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Time
import UpdateResult as UR



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( {}
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- TYPES


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | ClickedChangeAccountPermissions
    | ClickedProposeNewObjective


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            model
                |> UR.init

        ClickedChangeAccountPermissions ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = ClickedChangeAccountPermissions
                    , responseData = Encode.null
                    , data =
                        Eos.encodeTransaction
                            [ Eos.updateAuth
                                { actor = loggedIn.accountName
                                , permissionName = Eos.Account.ownerPermission
                                }
                                -- TODO - Not hardcode this
                                (Eos.Account.stringToName "eosmsigadmin")
                                2
                                -- TODO - Not hardcode these
                                [ { name = Eos.Account.stringToName "henriquebuss"
                                  , weight = 1
                                  }
                                , { name = Eos.Account.stringToName "henriquebus2"
                                  , weight = 1
                                  }
                                , { name = Eos.Account.stringToName "henriquebus4"
                                  , weight = 1
                                  }
                                ]
                            ]
                    }
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = ClickedChangeAccountPermissions
                    , errorMsg = NoOp
                    }

        ClickedProposeNewObjective ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    model
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ClickedProposeNewObjective
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    [ Eos.proposeTransaction
                                        loggedIn.accountName
                                        "createobj"
                                        -- TODO - Not hardcode these
                                        [ { actor = Eos.Account.stringToName "henriquebus2"
                                          , permissionName = Eos.Account.samplePermission
                                          }
                                        , { actor = Eos.Account.stringToName "henriquebus4"
                                          , permissionName = Eos.Account.samplePermission
                                          }
                                        ]
                                        (loggedIn.shared.now
                                            |> Time.posixToMillis
                                            -- now + 10 days
                                            |> (\now -> now + 1000 * 60 * 60 * 24 * 10)
                                            |> Time.millisToPosix
                                        )
                                        -- TODO - Not hardcode these
                                        [ { accountName = loggedIn.shared.contracts.community
                                          , name = "newobjective"
                                          , authorization =
                                                { actor = Eos.Account.stringToName "eosmsigadmin"
                                                , permissionName = Eos.Account.samplePermission
                                                }
                                          , data =
                                                { asset = { amount = 0, symbol = community.symbol }
                                                , description = "Test objective with multisig transaction"
                                                , creator = loggedIn.accountName
                                                }
                                                    |> Community.encodeCreateObjectiveAction
                                          }
                                        ]
                                    ]
                            }
                        |> LoggedIn.withAuthentication loggedIn
                            model
                            { successMsg = ClickedProposeNewObjective
                            , errorMsg = NoOp
                            }

                _ ->
                    UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn _ =
    let
        title =
            "Multisig"

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , view_ loggedIn.shared community
                        ]

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { title = title, content = content }


view_ : Shared -> Community.Model -> Html Msg
view_ { translators } community =
    div [ class "container mx-auto" ]
        [ div [ class "px-4" ]
            [ button
                [ class "button button-danger w-full my-10"
                , onClick ClickedChangeAccountPermissions
                ]
                [ text "Change account permissions" ]
            , button
                [ class "button button-primary w-full mt-10"
                , onClick ClickedProposeNewObjective
                ]
                [ text "Propose new objective" ]
            ]
        ]



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        ClickedChangeAccountPermissions ->
            [ "ClickedChangeAccountPermissions" ]

        ClickedProposeNewObjective ->
            [ "ClickedProposeNewObjective" ]
