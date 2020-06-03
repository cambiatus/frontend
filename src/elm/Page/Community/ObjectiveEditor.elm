module Page.Community.ObjectiveEditor exposing (Model, Msg, initEdit, initNew, jsAddressToMsg, msgToString, update, view)

import Api.Graphql
import Cambiatus.Object
import Cambiatus.Object.Community as Community
import Cambiatus.Object.Objective as Objective
import Cambiatus.Query as Query
import Community
import Eos as Eos exposing (Symbol, symbolToString)
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, button, div, span, text, textarea)
import Html.Attributes exposing (class, disabled, maxlength, placeholder, required, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import I18Next exposing (t)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import UpdateResult as UR



-- INIT


initNew : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
initNew { shared } communityId =
    ( { status = Loading, community = communityId, objectiveId = Nothing }
    , Api.Graphql.query shared (communityQuery communityId) CompletedCommunityLoad
    )


initEdit : LoggedIn.Model -> Symbol -> Int -> ( Model, Cmd Msg )
initEdit { shared } communityId objectiveId =
    ( { status = Loading, community = communityId, objectiveId = Just objectiveId }
    , Api.Graphql.query shared (communityQuery communityId) CompletedCommunityLoad
    )



-- MODEL


type alias Model =
    { status : Status
    , community : Symbol
    , objectiveId : Maybe Int
    }


type Status
    = Loading
    | Loaded Community EditStatus
      -- Errors
    | LoadCommunityFailed (Graphql.Http.Error (Maybe Community))
    | NotFound
    | Unauthorized


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


type alias Community =
    { symbol : Symbol
    , creator : Eos.Name
    , objectives : List Objective
    }


type alias Objective =
    { id : Int
    , description : String
    }


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedCommunityLoad (Result (Graphql.Http.Error (Maybe Community)) (Maybe Community))
    | EnteredDescription String
    | ClickedSaveObjective
    | GotSaveObjectiveResponse (Result Value String)


initObjectiveForm : ObjectiveForm
initObjectiveForm =
    { description = ""
    , save = NotAsked
    }



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view ({ shared } as loggedIn) model =
    case model.status of
        Loading ->
            Page.fullPageLoading

        NotFound ->
            Page.fullPageNotFound (t shared.translations "community.objectives.editor.not_found") ""

        LoadCommunityFailed err ->
            Page.fullPageGraphQLError (t shared.translations "community.objectives.editor.error") err

        Unauthorized ->
            text "not allowed to edit"

        Loaded { symbol } editStatus ->
            div []
                [ Page.viewHeader loggedIn (t shared.translations "community.objectives.title") (Route.Objectives symbol)
                , case editStatus of
                    NewObjective objForm ->
                        viewForm loggedIn objForm

                    EditObjective _ objForm ->
                        viewForm loggedIn objForm
                ]


viewForm : LoggedIn.Model -> ObjectiveForm -> Html Msg
viewForm { shared } objForm =
    let
        isDisabled =
            objForm.save == Saving
    in
    div [ class "bg-white w-full p-10" ]
        [ div [ class "container mx-auto" ]
            [ Html.form
                [ class "mb-10"
                ]
                [ span [ class "input-label" ]
                    [ text (t shared.translations "community.objectives.editor.description_label") ]
                , textarea
                    [ class "form-textarea w-full rounded border"
                    , rows 5
                    , disabled isDisabled
                    , onInput EnteredDescription
                    , value objForm.description
                    , required True
                    , maxlength 254
                    , placeholder (t shared.translations "community.objectives.editor.description_placeholder")
                    ]
                    []
                ]
            , button
                [ class "button button-primary"
                , type_ "submit"
                , onClick ClickedSaveObjective
                , disabled isDisabled
                ]
                [ text (t shared.translations "community.objectives.editor.submit") ]
            ]
        ]



-- UPDATE


communityQuery : Symbol -> SelectionSet (Maybe Community) RootQuery
communityQuery symbol =
    Query.community { symbol = symbolToString symbol } <|
        (SelectionSet.succeed Community
            |> with (Eos.symbolSelectionSet Community.symbol)
            |> with (Eos.nameSelectionSet Community.creator)
            |> with (Community.objectives objectiveSelectionSet)
        )


objectiveSelectionSet : SelectionSet Objective Cambiatus.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> with Objective.id
        |> with Objective.description


loadObjectiveForm : Community -> Int -> ObjectiveForm
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
        Loaded community (NewObjective objForm) ->
            UR.mapModel
                (\m ->
                    { m
                        | status =
                            Loaded community (NewObjective (fn objForm))
                    }
                )
                uResult

        Loaded community (EditObjective objId objForm) ->
            UR.mapModel
                (\m ->
                    { m
                        | status = Loaded community (EditObjective objId (fn objForm))
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
            I18Next.t loggedIn.shared.translations
    in
    case msg of
        CompletedCommunityLoad (Ok community) ->
            case community of
                Just cmm ->
                    if cmm.creator == loggedIn.accountName then
                        case model.objectiveId of
                            Just objectiveId ->
                                if List.any (\o -> o.id == objectiveId) cmm.objectives then
                                    { model
                                        | status =
                                            Loaded
                                                cmm
                                                (EditObjective
                                                    objectiveId
                                                    (loadObjectiveForm cmm objectiveId)
                                                )
                                    }
                                        |> UR.init

                                else
                                    { model | status = NotFound }
                                        |> UR.init

                            Nothing ->
                                { model | status = Loaded cmm (NewObjective initObjectiveForm) }
                                    |> UR.init

                    else
                        { model | status = Unauthorized }
                            |> UR.init

                Nothing ->
                    { model | status = NotFound }
                        |> UR.init

        CompletedCommunityLoad (Err err) ->
            { model | status = LoadCommunityFailed err }
                |> UR.init

        EnteredDescription val ->
            UR.init model
                |> updateObjective msg (\o -> { o | description = val })

        ClickedSaveObjective ->
            let
                newModel =
                    UR.init model
                        |> updateObjective msg (\o -> { o | save = Saving })

                save form isEdit =
                    case isEdit of
                        Just objectiveId ->
                            newModel
                                |> UR.addPort
                                    { responseAddress = ClickedSaveObjective
                                    , responseData = Encode.null
                                    , data =
                                        Eos.encodeTransaction
                                            [ { accountName = "bes.cmm"
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

                        Nothing ->
                            newModel
                                |> UR.addPort
                                    { responseAddress = ClickedSaveObjective
                                    , responseData = Encode.null
                                    , data =
                                        Eos.encodeTransaction
                                            [ { accountName = "bes.cmm"
                                              , name = "newobjective"
                                              , authorization =
                                                    { actor = loggedIn.accountName
                                                    , permissionName = Eos.samplePermission
                                                    }
                                              , data =
                                                    { symbol = model.community
                                                    , description = form.description
                                                    , creator = loggedIn.accountName
                                                    }
                                                        |> Community.encodeCreateObjectiveAction
                                              }
                                            ]
                                    }
            in
            if LoggedIn.isAuth loggedIn then
                case model.status of
                    Loaded _ (NewObjective objForm) ->
                        save objForm Nothing

                    Loaded _ (EditObjective objectiveId objForm) ->
                        save objForm (Just objectiveId)

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
                |> UR.addCmd
                    (Route.replaceUrl loggedIn.shared.navKey
                        (Route.Community model.community)
                    )
                |> UR.addExt (ShowFeedback Success (t "community.objectives.create_success"))

        GotSaveObjectiveResponse (Err v) ->
            UR.init model
                |> updateObjective msg (\o -> { o | save = SaveFailed })
                |> UR.logDebugValue msg v
                |> UR.addExt (ShowFeedback Failure (t "errors.unknown"))



-- UTILS


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
        CompletedCommunityLoad _ ->
            [ "CompletedCommunityLoad" ]

        EnteredDescription _ ->
            [ "EnteredDescription" ]

        ClickedSaveObjective ->
            [ "ClickedSaveObjective" ]

        GotSaveObjectiveResponse r ->
            [ "GotSaveObjectiveResponse", UR.resultToString r ]
