module Page.Community.ObjectiveEditor exposing (Model, Msg, initEdit, initNew, jsAddressToMsg, msgToString, update, view)

import Api.Graphql
import Community exposing (Community)
import Eos as Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import I18Next exposing (Translations, t)
import Icons
import Json.Decode as Json exposing (Value)
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR



-- INIT


initNew : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
initNew ({ shared } as loggedIn) communityId =
    ( { status = LoadingNew communityId }
    , Api.Graphql.query shared (Community.communityQuery communityId) CompletedCommunityLoad
    )


initEdit : LoggedIn.Model -> Symbol -> Int -> ( Model, Cmd Msg )
initEdit loggedIn communityId objectiveId =
    ( { status = LoadingEdit communityId objectiveId }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { status : Status
    }


type Status
    = Loading Symbol
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
    | SaveFailed


type alias ObjectiveForm =
    { description : String
    , save : SaveStatus
    }


initObjectiveForm =
    { description = ""
    , save = NotAsked
    }



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view ({ shared } as loggedIn) model =
    case model.status of
        LoadingNew _ ->
            Page.fullPageLoading

        NotFound ->
            Page.fullPageNotFound (t shared.translations "community.objectives.editor.not_found") ""

        LoadCommunityFailed err ->
            Page.fullPageGraphQLError (t shared.translations "community.objectives.editor.error") err

        Unauthorized ->
            text "not allowed to edit"

        Creating symbol ->
            div []
                [ viewHeader loggedIn model symbol
                , viewForm loggedIn model False
                ]

        LoadingEdit symbol objectiveId ->
            div [] []

        Saving symbol ->
            div []
                [ viewHeader loggedIn model symbol
                , viewForm loggedIn model True
                ]


viewHeader : LoggedIn.Model -> Model -> Symbol -> Html Msg
viewHeader ({ shared } as loggedIn) model symbol =
    div [ class "h-16 w-full bg-indigo-500 flex px-4 items-center" ]
        [ a
            [ class "items-center flex absolute"
            , Route.href (Route.Community symbol)
            ]
            [ Icons.back ""
            , p [ class "text-white text-sm ml-2" ]
                [ text (t shared.translations "back")
                ]
            ]
        , p [ class "text-white mx-auto" ] [ text (t shared.translations "community.objectives.title") ]
        ]


viewForm : LoggedIn.Model -> Model -> Bool -> Html Msg
viewForm ({ shared } as loggedIn) model isDisabled =
    div [ class "bg-white w-full p-10" ]
        [ div [ class "w-form mx-auto" ]
            [ Html.form [ class "mb-10", onSubmit ClickedSaveObjective ]
                [ span [ class "input-label" ] [ text (t shared.translations "community.objectives.editor.description_label") ]
                , textarea
                    [ class "form-textarea block w-full rounded border"
                    , rows 5
                    , disabled isDisabled
                    , placeholder (t shared.translations "community.objectives.editor.description_placeholder")
                    ]
                    []
                ]
            , button [ class "button button-primary", disabled isDisabled ] [ text (t shared.translations "community.objectives.editor.submit") ]
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedCommunityLoad (Result (Graphql.Http.Error (Maybe Community)) (Maybe Community))
    | ClickedSaveObjective Symbol


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedCommunityLoad (Ok community) ->
            case community of
                Just cmm ->
                    if cmm.creator == loggedIn.accountName then
                        { model | status = Creating cmm.symbol }
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

        ClickedSaveObjective symbol ->
            if LoggedIn.isAuth loggedIn then
                { model | status = Saving symbol }
                    |> UR.init
                -- |> UR.addPort
                -- { responseAddress = ClickedSaveObjective
                -- , responseData = Encode.null
                -- , data =
                -- Eos.encodeTransaction
                -- { actions =
                -- [ { accountName = "bes.cmm"
                -- , name = "newobjective"
                -- , authorization =
                --       { actor = loggedIn.accountName
                --       , permissionName = Eos.samplePermission
                --       }
                -- , data =
                --       { symbol = comm.symbol
                --       , description = obj.description
                --       , creator = loggedIn.accountName
                --       }
                -- |> Community.encodeCreateObjectiveAction
                -- }
                -- ]
                -- }

            else
                model
                    |> UR.init
                    |> UR.addExt
                        (Just ClickedSaveObjective |> RequiredAuthentication)



-- UTILS


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedCommunityLoad _ ->
            [ "CompletedCommunityLoad" ]
