module Page.Community.ObjectiveEditor exposing (Model, Msg, initEdit, initNew, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Cambiatus.Object.Community as Community
import Community
import Eos
import Eos.Account as Eos
import Html exposing (Html, button, div, span, text, textarea)
import Html.Attributes exposing (class, disabled, maxlength, placeholder, required, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import I18Next exposing (t)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import UpdateResult as UR



-- INIT


initNew : LoggedIn.Model -> ( Model, Cmd Msg )
initNew loggedIn =
    ( { status = Loading, objectiveId = Nothing }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )


initEdit : LoggedIn.Model -> Int -> ( Model, Cmd Msg )
initEdit loggedIn objectiveId =
    ( { status = Loading, objectiveId = Just objectiveId }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- MODEL


type alias Model =
    { status : Status
    , objectiveId : Maybe Int
    }


type Status
    = Loading
    | Authorized EditStatus
    | Unauthorized
    | NotFound


type EditStatus
    = NewObjective ObjectiveForm
    | EditObjective Int ObjectiveForm


type SaveStatus
    = NotAsked
    | Saving
    | Saved
    | SaveFailed


type alias ObjectiveForm =
    { description : String
    , save : SaveStatus
    }


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoadCommunity Community.Model
    | EnteredDescription String
    | ClickedSaveObjective
    | GotSaveObjectiveResponse (Result Value String)


initObjectiveForm : ObjectiveForm
initObjectiveForm =
    { description = ""
    , save = NotAsked
    }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        title =
            case model.status of
                Authorized editStatus ->
                    let
                        action =
                            case editStatus of
                                NewObjective _ ->
                                    t "menu.create"

                                EditObjective _ _ ->
                                    t "menu.edit"
                    in
                    action
                        ++ " "
                        ++ t "community.objectives.title"

                _ ->
                    ""

        content =
            case ( loggedIn.selectedCommunity, model.status ) of
                ( _, Loading ) ->
                    Page.fullPageLoading shared

                ( _, NotFound ) ->
                    Page.fullPageNotFound (t "community.objectives.editor.not_found") ""

                ( RemoteData.Loading, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.NotAsked, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Failure e, _ ) ->
                    Page.fullPageGraphQLError (t "community.objectives.editor.error") e

                ( RemoteData.Success _, Unauthorized ) ->
                    text "not allowed to edit"

                ( RemoteData.Success _, Authorized editStatus ) ->
                    div []
                        [ Page.viewHeader loggedIn (t "community.objectives.title") Route.Objectives
                        , case editStatus of
                            NewObjective objForm ->
                                viewForm loggedIn objForm

                            EditObjective _ objForm ->
                                viewForm loggedIn objForm
                        ]
    in
    { title = title
    , content =
        case RemoteData.map .hasObjectives loggedIn.selectedCommunity of
            RemoteData.Success True ->
                content

            RemoteData.Success False ->
                Page.fullPageNotFound
                    (t "error.pageNotFound")
                    (t "community.objectives.disabled.description")

            RemoteData.Loading ->
                Page.fullPageLoading shared

            RemoteData.NotAsked ->
                Page.fullPageLoading shared

            RemoteData.Failure e ->
                Page.fullPageGraphQLError (t "community.error_loading") e
    }


viewForm : LoggedIn.Model -> ObjectiveForm -> Html Msg
viewForm { shared } objForm =
    let
        t =
            shared.translators.t

        isDisabled =
            objForm.save == Saving
    in
    div [ class "bg-white w-full p-10" ]
        [ div [ class "container mx-auto" ]
            [ Html.form
                [ class "mb-10"
                ]
                [ span [ class "input-label" ]
                    [ text (t "community.objectives.editor.description_label") ]
                , textarea
                    [ class "input textarea-input w-full"
                    , rows 5
                    , disabled isDisabled
                    , onInput EnteredDescription
                    , value objForm.description
                    , required True
                    , maxlength 254
                    , placeholder (t "community.objectives.editor.description_placeholder")
                    ]
                    []
                ]
            , button
                [ class "button button-primary"
                , type_ "submit"
                , onClick ClickedSaveObjective
                , disabled isDisabled
                ]
                [ text (t "community.objectives.editor.submit") ]
            ]
        ]



-- UPDATE


loadObjectiveForm : Community.Model -> Int -> ObjectiveForm
loadObjectiveForm community objectiveId =
    let
        maybeObjective =
            List.filterMap
                (\o ->
                    if o.id == objectiveId then
                        Just (ObjectiveForm o.description NotAsked)

                    else
                        Nothing
                )
                community.objectives
    in
    case maybeObjective of
        [ x ] ->
            x

        _ ->
            initObjectiveForm


updateObjective : Msg -> (ObjectiveForm -> ObjectiveForm) -> UpdateResult -> UpdateResult
updateObjective msg fn uResult =
    case uResult.model.status of
        Authorized (NewObjective objForm) ->
            UR.mapModel
                (\m ->
                    { m
                        | status =
                            Authorized (NewObjective (fn objForm))
                    }
                )
                uResult

        Authorized (EditObjective objId objForm) ->
            UR.mapModel
                (\m ->
                    { m
                        | status = Authorized (EditObjective objId (fn objForm))
                    }
                )
                uResult

        _ ->
            uResult
                |> UR.logImpossible msg []


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            loggedIn.shared.translators.t
    in
    case msg of
        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                case model.objectiveId of
                    Just objectiveId ->
                        if List.any (\o -> o.id == objectiveId) community.objectives then
                            { model
                                | status =
                                    Authorized
                                        (EditObjective
                                            objectiveId
                                            (loadObjectiveForm community objectiveId)
                                        )
                            }
                                |> UR.init

                        else
                            { model | status = NotFound }
                                |> UR.init

                    Nothing ->
                        { model | status = Authorized (NewObjective initObjectiveForm) }
                            |> UR.init

            else
                { model | status = Unauthorized }
                    |> UR.init

        EnteredDescription val ->
            UR.init model
                |> updateObjective msg (\o -> { o | description = val })

        ClickedSaveObjective ->
            let
                newModel =
                    UR.init model
                        |> updateObjective msg (\o -> { o | save = Saving })

                save form isEdit _ =
                    case ( loggedIn.selectedCommunity, isEdit ) of
                        ( RemoteData.Success _, Just objectiveId ) ->
                            newModel
                                |> UR.addPort
                                    { responseAddress = ClickedSaveObjective
                                    , responseData = Encode.null
                                    , data =
                                        Eos.encodeTransaction
                                            [ { accountName = loggedIn.shared.contracts.community
                                              , name = "updobjective"
                                              , authorization =
                                                    { actor = loggedIn.accountName
                                                    , permissionName = Eos.samplePermission
                                                    }
                                              , data =
                                                    { objectiveId = objectiveId
                                                    , description = form.description
                                                    , editor = loggedIn.accountName
                                                    }
                                                        |> Community.encodeUpdateObjectiveAction
                                              }
                                            ]
                                    }

                        ( RemoteData.Success community, Nothing ) ->
                            newModel
                                |> UR.addPort
                                    { responseAddress = ClickedSaveObjective
                                    , responseData = Encode.null
                                    , data =
                                        Eos.encodeTransaction
                                            [ { accountName = loggedIn.shared.contracts.community
                                              , name = "newobjective"
                                              , authorization =
                                                    { actor = loggedIn.accountName
                                                    , permissionName = Eos.samplePermission
                                                    }
                                              , data =
                                                    { asset = Eos.Asset 0 community.symbol
                                                    , description = form.description
                                                    , creator = loggedIn.accountName
                                                    }
                                                        |> Community.encodeCreateObjectiveAction
                                              }
                                            ]
                                    }

                        _ ->
                            newModel
            in
            if LoggedIn.isAuth loggedIn then
                case ( loggedIn.selectedCommunity, model.status ) of
                    ( RemoteData.Success community, Authorized (NewObjective objForm) ) ->
                        save objForm Nothing community.precision

                    ( RemoteData.Success community, Authorized (EditObjective objectiveId objForm) ) ->
                        save objForm (Just objectiveId) community.precision

                    _ ->
                        newModel
                            |> UR.logImpossible msg []

            else
                newModel
                    |> UR.addExt
                        (Just ClickedSaveObjective |> RequiredAuthentication)

        GotSaveObjectiveResponse (Ok _) ->
            UR.init model
                |> updateObjective msg (\o -> { o | save = Saved })
                |> UR.addExt (ShowFeedback Success (t "community.objectives.create_success"))
                -- TODO - This only works sometimes
                |> UR.addExt LoggedIn.ReloadCommunity
                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey Route.Community)

        GotSaveObjectiveResponse (Err v) ->
            UR.init model
                |> updateObjective msg (\o -> { o | save = SaveFailed })
                |> UR.logDebugValue msg v
                |> UR.addExt (ShowFeedback Failure (t "errors.unknown"))



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClickedSaveObjective" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotSaveObjectiveResponse)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        EnteredDescription _ ->
            [ "EnteredDescription" ]

        ClickedSaveObjective ->
            [ "ClickedSaveObjective" ]

        GotSaveObjectiveResponse r ->
            [ "GotSaveObjectiveResponse", UR.resultToString r ]
