module Page.Community.ActionEditor exposing (Model, Msg, initEdit, initNew, jsAddressToMsg, msgToString, update, view)

import Account exposing (Profile)
import Api.Graphql
import Avatar exposing (Avatar)
import Bespiral.Scalar exposing (DateTime(..))
import Community exposing (Community)
import DataValidator exposing (Validator, getInput, greaterThanOrEqual, hasErrors, listErrors, longerThan, newValidator, oneOf, updateInput, validate)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, targetValue)
import I18Next exposing (t)
import Icons
import Json.Decode as Json exposing (Value)
import Json.Encode as Encode
import MaskedInput.Text as MaskedDate
import Page
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import Time exposing (Posix)
import UpdateResult as UR
import Utils



-- INIT


initNew : LoggedIn.Model -> Symbol -> Int -> ( Model, Cmd Msg )
initNew loggedIn symbol objId =
    ( { status = Loading
      , communityId = symbol
      , objectiveId = objId
      , actionId = Nothing
      , form = initForm symbol
      , multiSelectState = Select.newState ""
      }
    , Api.Graphql.query loggedIn.shared (Community.communityQuery symbol) CompletedCommunityLoad
    )


initEdit : LoggedIn.Model -> Symbol -> Int -> Int -> ( Model, Cmd Msg )
initEdit loggedIn symbol objectiveId actionId =
    ( { status = Loading
      , communityId = symbol
      , objectiveId = objectiveId
      , actionId = Just actionId
      , form = initForm symbol
      , multiSelectState = Select.newState ""
      }
    , Api.Graphql.query loggedIn.shared (Community.communityQuery symbol) CompletedCommunityLoad
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    { status : Status
    , communityId : Symbol
    , objectiveId : Int
    , actionId : Maybe Int
    , form : Form
    , multiSelectState : Select.State
    }


type Status
    = Loading
    | Loaded Community
      -- Errors
    | LoadFailed (Graphql.Http.Error (Maybe Community))
    | NotFound
    | Unauthorized


type ActionValidation
    = NoValidation
    | Validations (Maybe (Validator String)) (Maybe (Validator Int)) -- Date validation, usage validate


type Verification
    = Automatic
    | Manual (List Profile) (Validator String) (Validator Int) -- Manual: users list, verification reward and min votes


type SaveStatus
    = NotAsked
    | Saving
    | Saved
    | Failed


type alias Form =
    { description : Validator String
    , reward : Validator String
    , validation : ActionValidation
    , verification : Verification
    , saveStatus : SaveStatus
    }


initForm : Symbol -> Form
initForm sym =
    { description = newValidator "" (\s -> Just s) True []
    , reward = newValidator "" (\s -> Just "0.0") True []
    , validation = NoValidation
    , verification = Automatic
    , saveStatus = NotAsked
    }


editForm : Community.Action -> Form
editForm action =
    initForm Eos.bespiralSymbol


hasDateValidation : ActionValidation -> Bool
hasDateValidation validation =
    case validation of
        NoValidation ->
            False

        Validations maybeDate _ ->
            case maybeDate of
                Just _ ->
                    True

                Nothing ->
                    False


hasUnitValidation : ActionValidation -> Bool
hasUnitValidation validation =
    case validation of
        NoValidation ->
            False

        Validations _ maybeUnit ->
            case maybeUnit of
                Just _ ->
                    True

                Nothing ->
                    False



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedCommunityLoad (Result (Graphql.Http.Error (Maybe Community)) (Maybe Community))
    | OnSelectVerifier (Maybe Profile)
    | OnRemoveVerifier Profile
    | SelectMsg (Select.Msg Profile)
    | EnteredDescription String
    | EnteredReward String
    | EnteredDeadline String
    | DeadlineChanged MaskedDate.State
      -- | EnteredUsages String
      -- | EnteredVerifierReward String
      -- | EnteredMinVotes String
      -- | SubmittedData
    | ToggleValidity Bool
    | ToggleDeadline Bool
    | ToggleUsages Bool
    | SetVerification String
    | ValidateDeadline
    | GotInvalidDate
    | SaveAction (Result Value String)
    | GotSaveAction (Result Value String)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        shared =
            loggedIn.shared
    in
    case msg of
        CompletedCommunityLoad (Err err) ->
            { model | status = LoadFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedCommunityLoad (Ok c) ->
            case c of
                Just community ->
                    if community.creator == loggedIn.accountName then
                        -- Check the action belongs to the objective
                        let
                            maybeObjective =
                                List.filterMap
                                    (\o ->
                                        if o.id == model.objectiveId then
                                            Just o

                                        else
                                            Nothing
                                    )
                                    community.objectives
                                    |> List.head
                        in
                        case maybeObjective of
                            Just objective ->
                                case model.actionId of
                                    Just actionId ->
                                        -- Edit form
                                        let
                                            maybeAction =
                                                List.filterMap
                                                    (\a ->
                                                        if a.id == actionId then
                                                            Just a

                                                        else
                                                            Nothing
                                                    )
                                                    objective.actions
                                                    |> List.head
                                        in
                                        case maybeAction of
                                            Just action ->
                                                { model
                                                    | status = Loaded community
                                                    , form = editForm action
                                                }
                                                    |> UR.init

                                            Nothing ->
                                                { model | status = NotFound }
                                                    |> UR.init

                                    Nothing ->
                                        -- New form
                                        { model
                                            | status = Loaded community
                                            , form = initForm model.communityId
                                        }
                                            |> UR.init

                            Nothing ->
                                { model | status = NotFound }
                                    |> UR.init

                    else
                        { model | status = Unauthorized }
                            |> UR.init

                Nothing ->
                    { model | status = NotFound }
                        |> UR.init
                        |> UR.logImpossible msg []

        OnSelectVerifier maybeProfile ->
            let
                oldForm =
                    model.form
            in
            case model.form.verification of
                Automatic ->
                    model
                        |> UR.init

                Manual selectedVerifiers verificationReward minVotes ->
                    { model
                        | form =
                            { oldForm
                                | verification =
                                    Manual
                                        (maybeProfile
                                            |> Maybe.map (List.singleton >> List.append selectedVerifiers)
                                            |> Maybe.withDefault []
                                        )
                                        verificationReward
                                        minVotes
                            }
                    }
                        |> UR.init

        OnRemoveVerifier profile ->
            let
                oldForm =
                    model.form

                verification =
                    case model.form.verification of
                        Automatic ->
                            model.form.verification

                        Manual selectedVerifiers a b ->
                            Manual (List.filter (\currVerifier -> currVerifier.accountName /= profile.accountName) selectedVerifiers) a b
            in
            { model | form = { oldForm | verification = verification } }
                |> UR.init

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfig loggedIn.shared False) subMsg model.multiSelectState
            in
            { model | multiSelectState = updated }
                |> UR.init

        -- |> UR.addCmd cmd
        EnteredDescription val ->
            let
                oldForm =
                    model.form
            in
            { model | form = { oldForm | description = updateInput val model.form.description } }
                |> UR.init

        EnteredReward val ->
            let
                oldForm =
                    model.form
            in
            { model | form = { oldForm | reward = updateInput val model.form.reward } }
                |> UR.init

        EnteredDeadline deadlineStr ->
            -- let
            --     currentForm =
            --         model.form
            --     updatedForm =
            --         { currentForm | deadline = deadlineStr }
            -- in
            -- { model | form = updatedForm }
            model
                |> UR.init

        ValidateDeadline ->
            -- let
            --     month =
            --         String.slice 0 2 model.form.deadline
            --     day =
            --         String.slice 2 4 model.form.deadline
            --     year =
            --         String.slice 4 8 model.form.deadline
            --     -- Hand date over as mm/dd/yyyy
            --     dateStr =
            --         String.join "/" [ month, day, year ]
            -- in
            model
                |> UR.init

        -- |> UR.addPort
        --     { responseAddress = ValidateDeadline
        --     , responseData = Encode.null
        --     , data =
        --         Encode.object
        --             [ ( "name", Encode.string "validateDeadline" )
        --             , ( "deadline", Encode.string dateStr )
        --             ]
        --     }
        DeadlineChanged state ->
            model
                |> UR.init

        ToggleValidity bool ->
            model
                |> UR.init

        ToggleDeadline bool ->
            let
                oldForm =
                    model.form

                deadlineValidation =
                    if bool then
                        Just (newValidator "" (\s -> Just "0/0/0") True [])

                    else
                        Nothing

                usagesValidation =
                    case model.form.validation of
                        NoValidation ->
                            Nothing

                        Validations _ maybeUsages ->
                            maybeUsages
            in
            { model
                | form =
                    { oldForm
                        | validation =
                            if deadlineValidation /= Nothing || usagesValidation /= Nothing then
                                Validations deadlineValidation usagesValidation

                            else
                                NoValidation
                    }
            }
                |> UR.init

        ToggleUsages bool ->
            let
                oldForm =
                    model.form

                usagesValidation =
                    if bool then
                        Just (newValidator 10 (\s -> Just "0") True [])

                    else
                        Nothing

                deadlineValidation =
                    case model.form.validation of
                        NoValidation ->
                            Nothing

                        Validations maybeDate _ ->
                            maybeDate
            in
            { model
                | form =
                    { oldForm
                        | validation =
                            if deadlineValidation /= Nothing || usagesValidation /= Nothing then
                                Validations deadlineValidation usagesValidation

                            else
                                NoValidation
                    }
            }
                |> UR.init

        SetVerification val ->
            let
                oldForm =
                    model.form

                verificationReward =
                    newValidator "" (\s -> Just "0,0") False []

                minVotes =
                    newValidator 0 (\s -> Just "") False []
            in
            { model
                | form =
                    { oldForm
                        | verification =
                            if val == "automatic" then
                                Automatic

                            else
                                Manual [] verificationReward minVotes
                    }
            }
                |> UR.init

        GotInvalidDate ->
            model
                |> UR.init

        SaveAction isoDate ->
            case isoDate of
                Ok date ->
                    let
                        dateInt =
                            if String.length date == 0 then
                                0

                            else
                                Just (DateTime date)
                                    |> Utils.posixDateTime
                                    |> Time.posixToMillis

                        validatorsStr =
                            []
                                |> List.map (\v -> Eos.nameToString v.accountName)
                                |> String.join "-"
                    in
                    -- if LoggedIn.isAuth loggedIn then
                    --     model
                    --         |> UR.init
                    --         |> UR.addPort
                    --             { responseAddress = SaveAction isoDate
                    --             , responseData = Encode.null
                    --             , data =
                    --                 Eos.encodeTransaction
                    --                     { actions =
                    --                         [ { accountName = "bes.cmm"
                    --                           , name = "upsertaction"
                    --                           , authorization =
                    --                                 { actor = loggedIn.accountName
                    --                                 , permissionName = Eos.samplePermission
                    --                                 }
                    --                           , data =
                    --                                 { actionId = 0
                    --                                 , objectiveId = model.objectiveId
                    --                                 , description = model.form.description
                    --                                 , reward = String.fromFloat model.form.reward ++ " " ++ model.form.symbol
                    --                                 , verifier_reward = String.fromFloat model.form.verifierReward ++ " " ++ model.form.symbol
                    --                                 , deadline = dateInt
                    --                                 , usages = model.form.maxUsage
                    --                                 , usagesLeft = model.form.maxUsage
                    --                                 , verifications = model.form.minVotes
                    --                                 , verificationType = model.form.verificationType
                    --                                 , validatorsStr = validatorsStr
                    --                                 , isCompleted = 0
                    --                                 , creator = loggedIn.accountName
                    --                                 }
                    --                                     |> Community.encodeCreateActionAction
                    --                           }
                    --                         ]
                    --                     }
                    --             }
                    -- else
                    model
                        |> UR.init
                        |> UR.addExt
                            (Just (SaveAction isoDate)
                                |> RequiredAuthentication
                            )

                Err _ ->
                    update GotInvalidDate model loggedIn

        GotSaveAction (Ok tId) ->
            model
                |> UR.init
                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey (Route.Community model.communityId))

        GotSaveAction (Err val) ->
            model
                |> UR.init
                |> UR.logImpossible msg []



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    let
        shared =
            loggedIn.shared

        t s =
            I18Next.t shared.translations s

        text_ s =
            text (t s)
    in
    case model.status of
        Loading ->
            Page.fullPageLoading

        Loaded community ->
            div [ class "bg-white" ]
                [ Page.viewHeader loggedIn (t "community.actions.title") (Route.Objectives model.communityId)
                , viewForm loggedIn community model
                ]

        LoadFailed err ->
            Page.fullPageGraphQLError (t "error.invalidSymbol") err

        NotFound ->
            Page.fullPageNotFound (t "community.actions.form.not_found") ""

        Unauthorized ->
            Page.fullPageNotFound "not authorized" ""



-- case model.status of
--     InvalidObjective badId ->
--         defaultContainer
--             [ Page.viewTitle (t "error.objective.invalid_id")
--             , span [] [ text badId ]
--             ]
--     LoadingFailed err ->
--         Page.fullPageGraphQLError (t "error.invalidSymbol") err
--     LoadingCommunity ->
--         defaultContainer
--             [ Page.fullPageLoading ]
--     EditingNew ->
--         case model.community of
--             Nothing ->
--                 defaultContainer
--                     [ Page.fullPageLoading ]
--             Just comm ->
--                 defaultContainer
--                     ([ Page.viewTitle (t "community.actions.new") ]
--                         ++ viewForm loggedIn.shared comm model
--                     )
--     ActionSaved ->
--         case model.community of
--             Nothing ->
--                 defaultContainer
--                     [ Page.fullPageLoading ]
--             Just comm ->
--                 defaultContainer
--                     ([ div [ class "mx-10 h-10 bg-green flex flex-col items-center font-sans text-white" ]
--                         [ span [ class "my-auto" ] [ text_ "community.actions.form.success" ] ]
--                      ]
--                         ++ [ Page.viewTitle (t "community.actions.new") ]
--                         ++ viewForm loggedIn.shared comm model
--                     )
--     ActionSaveFailed _ ->
--         case model.community of
--             Nothing ->
--                 defaultContainer
--                     [ Page.fullPageLoading ]
--             Just comm ->
--                 defaultContainer
--                     ([ div [ class "mx-10 h-10 bg-red flex flex-col items-center font-sans text-white" ]
--                         [ text_ "community.actions.form.fail" ]
--                      ]
--                         ++ [ Page.viewTitle (t "community.actions.new") ]
--                         ++ viewForm loggedIn.shared comm model
--                     )


viewForm : LoggedIn.Model -> Community -> Model -> Html Msg
viewForm ({ shared } as loggedIn) community model =
    let
        ipfsUrl =
            shared.endpoints.ipfs

        logoLink =
            Community.logoUrl ipfsUrl (Just community.logo)

        text_ s =
            text (t shared.translations s)

        dateOptions =
            MaskedDate.defaultOptions EnteredDeadline DeadlineChanged

        usageColor =
            if True then
                " text-green"

            else
                " text-black"

        deadlineColor =
            if True then
                " text-green"

            else
                " text-black"
    in
    div [ class "container mx-auto" ]
        [ div [ class "py-6 px-4" ]
            [ div [ class "mb-10" ]
                [ span [ class "input-label" ]
                    [ text_ "community.actions.form.description_label" ]
                , textarea
                    [ class "w-full input rounded-sm"
                    , rows 5
                    , onInput EnteredDescription
                    ]
                    []
                ]
            , div [ class "mb-10" ]
                [ span [ class "input-label" ]
                    [ text_ "community.actions.form.reward_label" ]
                , div [ class "flex sm:w-2/5 h-12 rounded-sm border border-gray-500" ]
                    [ input
                        [ class "block w-4/5 border-none px-4 py-3 outline-none"
                        , type_ "number"
                        , placeholder "0.00"
                        , onInput EnteredReward
                        ]
                        []
                    , span
                        [ class "w-1/5 flex text-white items-center justify-center bg-indigo-500 text-body uppercase" ]
                        [ text (Eos.symbolToString community.symbol) ]
                    ]
                ]
            , div [ class "mb-6" ]
                [ div [ class "mb-10" ]
                    [ p [ class "input-label mb-6" ] [ text_ "community.actions.form.validity_label" ]
                    , div [ class "flex" ]
                        [ div [ class "form-switch inline-block align-middle" ]
                            [ input
                                [ type_ "checkbox"
                                , id "expiration-toggle"
                                , name "expiration-toggle"
                                , class "form-switch-checkbox"
                                , checked (model.form.validation /= NoValidation)
                                , onCheck ToggleValidity
                                ]
                                []
                            , label [ class "form-switch-label", for "expiration-toggle" ] []
                            ]
                        , label [ class "flex text-body text-green", for "expiration-toggle" ]
                            [ p [ class "font-bold mr-1" ]
                                [ if model.form.validation == NoValidation then
                                    text_ "community.actions.form.validation_off"

                                  else
                                    text_ "community.actions.form.validation_on"
                                ]
                            , text_ "community.actions.form.validation_detail"
                            ]
                        ]
                    ]
                ]
            , div
                [ class "" ]
                [ div [ class "mb-3 flex flex-row text-body items-bottom" ]
                    [ input
                        [ id "date"
                        , type_ "checkbox"
                        , class "form-checkbox mr-2 p-1"
                        , checked (hasDateValidation model.form.validation)
                        , onCheck ToggleDeadline
                        ]
                        []
                    , label
                        [ for "date", class ("flex " ++ deadlineColor) ]
                        [ p [ class "font-bold mr-1" ] [ text_ "community.actions.form.date_validity" ]
                        , text_ "community.actions.form.date_validity_details"
                        ]
                    ]
                , case model.form.validation of
                    NoValidation ->
                        text ""

                    Validations dateValidation _ ->
                        case dateValidation of
                            Just validation ->
                                div []
                                    [ span [ class "input-label" ]
                                        [ text_ "community.actions.form.date_label" ]
                                    , div [ class "mb-10" ]
                                        [ input [ class "input" ] []

                                        --  MaskedDate.input
                                        --     { dateOptions
                                        --         | pattern = "##/##/####"
                                        --         , inputCharacter = '#'
                                        --     }
                                        --     [ class ("w-full h-12 font-sans borde rounded form-input bg-gray-500 text-black placeholder-black" ++ borderColor Deadline)
                                        --     , placeholder "mm/dd/yyyy"
                                        --     , disabled (not model.hasDeadline)
                                        --     ]
                                        --     model.form.deadlineState
                                        --     model.form.deadline
                                        -- , viewFieldErrors Deadline model.problems
                                        ]
                                    ]

                            Nothing ->
                                text ""
                , div [ class "mb-6 flex flex-row text-body items-bottom" ]
                    [ input
                        [ id "quantity"
                        , type_ "checkbox"
                        , class "form-checkbox mr-2"
                        , checked (hasUnitValidation model.form.validation)
                        , onCheck ToggleUsages
                        ]
                        []
                    , label [ for "quantity", class ("flex " ++ usageColor) ]
                        [ p [ class "font-bold mr-1" ] [ text_ "community.actions.form.quantity_validity" ]
                        , text_ "community.actions.form.quantity_validity_details"
                        ]
                    ]
                ]
            , case model.form.validation of
                NoValidation ->
                    text ""

                Validations _ usagesValidation ->
                    case usagesValidation of
                        Just validation ->
                            div []
                                [ span [ class "input-label" ]
                                    [ text_ "community.actions.form.quantity_label" ]
                                , div [ class "mb-10" ]
                                    [ input
                                        [ type_ "number"
                                        , class "input"

                                        -- , onInput EnteredUsages
                                        ]
                                        []
                                    ]
                                ]

                        Nothing ->
                            text ""
            , div [ class "mb-10" ]
                [ div [ class "flex flex-row justify-between mb-6" ]
                    [ p [ class "input-label" ]
                        [ text_ "community.actions.form.verification_label" ]
                    ]
                , div [ class "mb-6" ]
                    [ label [ class "inline-flex items-center" ]
                        [ input
                            [ type_ "radio"
                            , class "form-radio h-5 w-5 text-green"
                            , name "verification"
                            , value "automatic"
                            , checked (model.form.verification == Automatic)
                            , onClick (SetVerification "automatic")
                            ]
                            []
                        , span
                            [ class "flex ml-3 text-body"
                            , classList [ ( "text-green", model.form.verification == Automatic ) ]
                            ]
                            [ p [ class "font-bold mr-1" ] [ text_ "community.actions.form.automatic" ]
                            , text_ "community.actions.form.automatic_detail"
                            ]
                        ]
                    ]
                , div [ class "mb-6" ]
                    [ label [ class "inline-flex items-center" ]
                        [ input
                            [ type_ "radio"
                            , class "form-radio h-5 w-5 text-green"
                            , name "verification"
                            , value "manual"
                            , checked (model.form.verification /= Automatic)
                            , onClick (SetVerification "manual")
                            ]
                            []
                        , span
                            [ class "flex ml-3 text-body"
                            , classList [ ( "text-green", model.form.verification /= Automatic ) ]
                            ]
                            [ p [ class "font-bold mr-1" ] [ text_ "community.actions.form.manual" ]
                            , text_ "community.actions.form.manual_detail"
                            ]
                        ]
                    ]
                , if model.form.verification /= Automatic then
                    div []
                        [ span [ class "input-label" ]
                            [ text_ "community.actions.form.verifiers_label" ]
                        , div []
                            [ viewVerifierSelect shared model False
                            , viewSelectedVerifiers shared model
                            ]
                        , span [ class "input-label" ]
                            [ text_ "community.actions.form.verifiers_reward_label" ]
                        , div [ class "mb-10" ]
                            [ div [ class "flex flex-row border rounded-sm" ]
                                [ input
                                    [ class "input w-4/5 border-none"
                                    , type_ "number"
                                    , placeholder "0.00"

                                    -- , onInput EnteredVerifierReward
                                    ]
                                    []
                                , span
                                    [ class "w-1/5 flex input-token" ]
                                    [ text (Eos.symbolToString community.symbol) ]
                                ]
                            ]
                        , div [ class "flex flex-row justify-between" ]
                            [ p [ class "input-label" ]
                                [ text_ "community.actions.form.votes_label" ]
                            , div [ class "input-label text-orange-300 relative mb-1 tooltip-container" ]
                                [ text_ "community.actions.form.tooltip_label"
                                , p [ class "bg-black text-white absolute z-10 py-3 px-4 top-1 w-select right-0 rounded whitespace-pre-line leading-normal text-body font-sans normal-case" ]
                                    [ text_ "community.actions.form.votes_tooltip" ]
                                ]
                            ]
                        , div []
                            [ input
                                [ class "w-full input border rounded"

                                -- , onInput EnteredMinVotes
                                , type_ "number"
                                ]
                                []
                            ]
                        ]

                  else
                    text ""
                ]
            , div [ class "flex align-center justify-center" ]
                [ button
                    [ class "button button-primary"

                    -- , onClick SubmittedData
                    ]
                    [ text_ "menu.create" ]
                ]
            ]
        ]


viewSelectedVerifiers : Shared -> Model -> Html Msg
viewSelectedVerifiers shared model =
    let
        ipfsUrl =
            shared.endpoints.ipfs

        text_ s =
            text (t shared.translations s)

        verifiers =
            case model.form.verification of
                Automatic ->
                    [ text "" ]

                Manual selectedVerifiers _ _ ->
                    selectedVerifiers
                        |> List.map
                            (\p ->
                                div
                                    [ class "flex flex-col m-3 items-center" ]
                                    [ div [ class "relative h-10 w-12 ml-2" ]
                                        [ Avatar.view ipfsUrl p.avatar "h-10 w-10"
                                        , div
                                            [ onClick (OnRemoveVerifier p)
                                            , class "absolute top-0 right-0 z-10 rounded-full h-6 w-6 flex items-center"
                                            ]
                                            [ Icons.remove "" ]
                                        ]
                                    , span [ class "mt-2 text-black font-sans text-body leading-normal" ]
                                        [ text (Eos.nameToString p.accountName) ]
                                    ]
                            )
    in
    div [ class "flex flex-row mt-3 mb-10 flex-wrap" ] verifiers



-- Configure Select


filter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
filter minChars toLabel query items =
    if String.length query < minChars then
        Nothing

    else
        items
            |> Simple.Fuzzy.filter toLabel query
            |> Just


selectConfig : Shared -> Bool -> Select.Config Msg Profile
selectConfig shared isDisabled =
    Select.newConfig
        { onSelect = OnSelectVerifier
        , toLabel = \p -> Eos.nameToString p.accountName
        , filter = filter 2 (\p -> Eos.nameToString p.accountName)
        }
        |> Select.withMultiSelection True
        |> Select.withInputClass "form-input h-12 w-full font-sans placeholder-gray-900"
        |> Select.withClear False
        |> Select.withMultiInputItemContainerClass "hidden h-0"
        |> Select.withNotFound "No matches"
        |> Select.withNotFoundClass "text-red  border-solid border-gray-100 border rounded z-30 bg-white w-select"
        |> Select.withNotFoundStyles [ ( "padding", "0 2rem" ) ]
        |> Select.withDisabled isDisabled
        |> Select.withHighlightedItemClass "autocomplete-item-highlight"
        |> Select.withPrompt (t shared.translations "community.actions.form.verifier_placeholder")
        |> Select.withItemHtml (viewAutoCompleteItem shared)
        |> Select.withMenuClass "border-t-none border-solid border-gray-100 border rounded-b z-30 bg-white"


viewAutoCompleteItem : Shared -> Profile -> Html Never
viewAutoCompleteItem shared profile =
    let
        ipfsUrl =
            shared.endpoints.ipfs
    in
    div [ class "pt-3 pl-3 flex flex-row items-center w-select z-30" ]
        [ div [ class "pr-3" ] [ Avatar.view ipfsUrl profile.avatar "h-7 w-7" ]
        , div [ class "flex flex-col font-sans border-b border-gray-500 pb-3 w-full" ]
            [ span [ class "text-black text-body leading-loose" ]
                [ text (Eos.nameToString profile.accountName) ]
            , span [ class "leading-caption uppercase text-green text-caption" ]
                [ case profile.userName of
                    Just name ->
                        text name

                    Nothing ->
                        text ""
                ]
            ]
        ]


viewVerifierSelect : Shared -> Model -> Bool -> Html Msg
viewVerifierSelect shared model isDisabled =
    let
        users =
            case model.status of
                Loaded community ->
                    community.members

                _ ->
                    []
    in
    case model.form.verification of
        Automatic ->
            text ""

        Manual selectedUsers _ _ ->
            div []
                [ Html.map SelectMsg
                    (Select.view (selectConfig shared isDisabled)
                        model.multiSelectState
                        users
                        selectedUsers
                    )
                ]



-- UTILS


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ValidateDeadline" :: _ ->
            Json.decodeValue
                (Json.oneOf
                    [ Json.field "date" Json.string
                        |> Json.map Ok
                    , Json.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << SaveAction)
                |> Result.withDefault (Just GotInvalidDate)

        "UploadAction" :: _ ->
            Json.decodeValue
                (Json.oneOf
                    [ Json.field "transactionId" Json.string
                        |> Json.map Ok
                    , Json.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotSaveAction)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedCommunityLoad _ ->
            [ "CompletedCommunityLoad" ]

        OnSelectVerifier _ ->
            [ "OnSelectVerifier" ]

        OnRemoveVerifier _ ->
            [ "OnRemoveVerifier" ]

        EnteredDescription _ ->
            [ "EnteredDescription" ]

        EnteredReward _ ->
            [ "EnteredReward" ]

        EnteredDeadline _ ->
            [ "EnteredDeadline" ]

        DeadlineChanged _ ->
            [ "DeadlineChanged" ]

        SelectMsg _ ->
            [ "SelectMsg" ]

        ToggleValidity _ ->
            [ "ToggleValidity" ]

        ToggleDeadline _ ->
            [ "ToggleDeadline" ]

        ToggleUsages _ ->
            [ "ToggleDeadline" ]

        SetVerification _ ->
            [ "SetVerification" ]

        ValidateDeadline ->
            [ "ValidateDeadline" ]

        SaveAction _ ->
            [ "SaveAction" ]

        GotInvalidDate ->
            [ "GotInvalidDate" ]

        GotSaveAction _ ->
            [ "GotSaveAction" ]
