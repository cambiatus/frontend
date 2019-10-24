module Page.Community exposing (Model, Msg, init, jsAddressToMsg, msgToString, subscriptions, update, view)

import Account
import Api
import Api.Graphql
import Asset.Icon as Icon
import Avatar exposing (Avatar)
import Bespiral.Enum.VerificationType as VerificationType exposing (VerificationType)
import Bespiral.Scalar exposing (DateTime(..))
import Community exposing (Community, ObjectiveId, Validator)
import Dict exposing (Dict)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, targetValue)
import Html.Lazy
import Http
import I18Next exposing (Translations, t)
import Icons exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Page
import Ports
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Strftime
import Task
import Time exposing (Posix, posixToMillis)
import Transfer exposing (Transfer)
import UpdateResult as UR
import Utils



-- INIT


init : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) symbol =
    ( initModel loggedIn
    , Cmd.batch
        [ Api.Graphql.query shared (Community.communityQuery symbol) CompletedLoadCommunity
        , Task.perform GotTime Time.now
        ]
    )


fetchLastObjectiveId : LoggedIn.Model -> (Result Http.Error ObjectiveId -> msg) -> Cmd msg
fetchLastObjectiveId { shared, accountName } toMsg =
    let
        expect =
            Decode.field "rows" (Decode.list (Decode.field "objective_id" Community.decodeObjectiveId))
                |> Decode.andThen
                    (\ns ->
                        List.head ns
                            |> Maybe.map Decode.succeed
                            |> Maybe.withDefault (Decode.fail "No objective.")
                    )
    in
    Api.getTableRows shared (Eos.encodeName accountName) "objectiveidx" 5 expect toMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { date : Maybe Posix
    , community : LoadStatus
    , members : List Member
    , openObjective : Maybe Int
    , modalStatus : ModalStatus
    , messageStatus : MessageStatus
    }


initModel : LoggedIn.Model -> Model
initModel loggedIn =
    { date = Nothing
    , community = Loading
    , members = []
    , openObjective = Nothing
    , modalStatus = Closed
    , messageStatus = None
    }


type LoadStatus
    = Loading
    | Loaded Community EditStatus
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Community))


type EditStatus
    = NoEdit
    | OpenObjective Int


type SaveStatus
    = NotAsked
    | Saving
    | SaveFailed (Dict String FormError)


type ModalStatus
    = Opened Bool Int -- Action id
    | Closed


type MessageStatus
    = None
    | Success String
    | Failure String


type alias ObjectiveForm =
    { description : String
    , save : SaveStatus
    }


emptyObjectiveForm : ObjectiveForm
emptyObjectiveForm =
    { description = ""
    , save = NotAsked
    }


type Verification
    = Manually
    | Automatically


type FormError
    = Required
    | TooShort Int
    | TooLong Int
    | InvalidChar Char
    | AlreadyTaken
    | NotMember


type alias Member =
    { id : String
    , accountName : String
    , nameWithAt : String
    }


decodeMembers : Decoder (List Member)
decodeMembers =
    Decode.list
        (Decode.map2
            (\id_ acc -> Member id_ acc ("@" ++ acc))
            (Decode.field "_id" Decode.string)
            (Decode.field "account" Decode.string)
        )
        |> Decode.field "data"



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    let
        ipfsUrl =
            loggedIn.shared.endpoints.ipfs

        t s =
            I18Next.t loggedIn.shared.translations s

        text_ s =
            text (t s)

        plcH s =
            placeholder (t s)
    in
    case model.community of
        Loading ->
            Page.fullPageLoading

        NotFound ->
            Page.viewCardEmpty [ text "Community not found" ]

        Failed e ->
            Page.fullPageGraphQLError (t "community.objectives.title") e

        Loaded community editStatus ->
            let
                canEdit =
                    LoggedIn.isAccount community.creator loggedIn
            in
            div [ class "" ]
                [ viewHeader loggedIn community
                , div [ class "bg-white p-20" ]
                    [ div [ class "flex flex-wrap w-full items-center" ]
                        [ p [ class "text-4xl font-bold" ]
                            [ text community.title ]
                        , if canEdit then
                            a
                                [ classList
                                    [ ( "hidden", editStatus /= NoEdit )
                                    ]
                                , class "button button-secondary button-small w-16 ml-4"
                                , Route.href (Route.EditCommunity community.symbol)
                                ]
                                [ text "edit" ]

                          else
                            text ""
                        ]
                    , p [ class "text-grey-200 text-sm" ] [ text community.description ]
                    ]
                , div [ class "container mx-auto px-4" ]
                    [ viewClaimModal loggedIn model
                    , viewMessageStatus loggedIn model
                    , div [ class "bg-white py-6 sm:py-8 px-3 sm:px-6 rounded-lg sm:rounded mt-4" ]
                        ([ Page.viewTitle (t "community.objectives.title_plural") ]
                            ++ List.indexedMap (viewObjective loggedIn model editStatus community)
                                community.objectives
                            ++ [ if canEdit then
                                    viewObjectiveNew loggedIn (List.length community.objectives) editStatus community.symbol

                                 else
                                    text ""
                               ]
                        )
                    , Transfer.getTransfers (Just community)
                        |> viewSections loggedIn model
                    ]
                ]



-- VIEW OBJECTIVE


viewObjective : LoggedIn.Model -> Model -> EditStatus -> Community -> Int -> Community.Objective -> Html Msg
viewObjective loggedIn model editStatus metadata index objective =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        text_ s =
            text (t s)

        plcH s =
            placeholder (t s)

        canEdit =
            LoggedIn.isAccount metadata.creator loggedIn

        isOpen : Bool
        isOpen =
            case model.openObjective of
                Just obj ->
                    obj == index

                Nothing ->
                    False

        arrowStyle : String
        arrowStyle =
            if canEdit then
                " sm:flex-grow-2"

            else
                " "

        objIdStr : String
        objIdStr =
            Community.unwrapObjectiveId objective.id
                |> String.fromInt

        actsNButton : List (Html Msg)
        actsNButton =
            List.map (viewAction loggedIn metadata model.date) objective.actions
                |> List.intersperse (hr [ class "bg-border-grey text-border-grey" ] [])
                |> (\acts ->
                        if canEdit then
                            acts
                                ++ [ button
                                        [ class "border border-dashed border-button-orange mt-6 w-full flex flex-row content-start px-4 py-2"
                                        , onClick (CreateAction metadata.symbol objIdStr)
                                        ]
                                        [ span [ class "px-2 text-button-orange font-medium" ] [ text "+" ]
                                        , span [ class "text-button-orange font-medium" ] [ text_ "community.actions.new" ]
                                        ]
                                   ]

                        else
                            acts
                   )
    in
    div [ class "my-2" ]
        [ div
            [ class "px-3 py-4 bg-body-blue flex flex-col sm:flex-row sm:items-center sm:h-10"
            ]
            [ div [ class "sm:flex-grow-7 sm:w-5/12" ]
                [ div
                    [ class "truncate overflow-hidden whitespace-no-wrap text-white font-medium text-sm overflow-hidden sm:flex-grow-8 sm:leading-normal sm:text-lg"
                    , if isOpen then
                        onClick ClickedCloseObjective

                      else
                        onClick (ClickedOpenObjective index)
                    ]
                    [ text objective.description ]
                ]
            , div [ class ("flex flex-row justify-between mt-5 sm:mt-0" ++ arrowStyle) ]
                [ if canEdit then
                    a
                        [ class "text-white font-medium underline text-xs uppercase sm:text-sm"
                        , Route.href (Route.EditObjective metadata.symbol (Community.unwrapObjectiveId objective.id))
                        ]
                        [ text_ "menu.edit" ]

                  else
                    text ""
                , if isOpen then
                    button
                        [ class "align-middle"
                        , if isOpen then
                            onClick ClickedCloseObjective

                          else
                            onClick (ClickedOpenObjective index)
                        ]
                        [ img
                            [ class "rotate-180 fill-current text-white h-2 w-4 stroke-current"
                            , src "/icons/objective_arrow.svg"
                            ]
                            []
                        ]

                  else
                    button
                        [ class "align-middle"
                        , if isOpen then
                            onClick ClickedCloseObjective

                          else
                            onClick (ClickedOpenObjective index)
                        ]
                        [ img
                            [ class "h-2 w-4 self-end stroke-current"
                            , src "/icons/objective_arrow.svg"
                            ]
                            []
                        ]
                ]
            ]
        , if isOpen then
            div [ class "pt-5 px-3 pb-3 sm:px-6 bg-white rounded-b-lg border-solid border-grey border" ]
                actsNButton

          else
            text ""
        ]


viewObjectiveNew : LoggedIn.Model -> Int -> EditStatus -> Symbol -> Html Msg
viewObjectiveNew loggedIn index edit communityId =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    a
        [ class "border border-dashed border-button-orange mt-6 w-full flex flex-row content-start px-4 py-2"
        , Route.href (Route.NewObjective communityId)
        , disabled (edit /= NoEdit)
        ]
        [ span [ class "px-2 text-button-orange font-medium" ] [ text "+" ]
        , span [ class "text-button-orange font-medium" ] [ text (t "community.objectives.new") ]
        ]



-- VIEW ACTION


viewAction : LoggedIn.Model -> Community -> Maybe Posix -> Community.Action -> Html Msg
viewAction loggedIn metadata maybeDate action =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        text_ s =
            text (t s)

        canEdit =
            -- LoggedIn.isAccount metadata.creator loggedIn
            False

        posixDeadline : Posix
        posixDeadline =
            action.deadline
                |> Utils.posixDateTime

        deadlineStr : String
        deadlineStr =
            posixDeadline
                |> Strftime.format "%d %B %Y" Time.utc

        pastDeadline : Bool
        pastDeadline =
            case action.deadline of
                Just deadline ->
                    case maybeDate of
                        Just today ->
                            posixToMillis today > posixToMillis posixDeadline

                        Nothing ->
                            False

                Nothing ->
                    False

        rewardStrike : String
        rewardStrike =
            if pastDeadline || (action.usagesLeft < 1 && action.usages > 0) then
                " line-through"

            else
                ""

        dateColor : String
        dateColor =
            case pastDeadline of
                True ->
                    " text-date-red"

                False ->
                    " text-date-purple"

        usagesColor : String
        usagesColor =
            if action.usagesLeft >= 1 || action.usages == 0 then
                " text-date-purple"

            else
                " text-date-red"

        ( claimColors, claimText ) =
            if pastDeadline || (action.usagesLeft < 1 && action.usages > 0) then
                ( " text-text-grey bg-grey cursor-not-allowed", "dashboard.closed" )

            else
                ( " text-white bg-button-orange", "dashboard.claim" )

        claimSize =
            if canEdit then
                " w-4/5"

            else
                " w-1/2"

        ipfsUrl =
            loggedIn.shared.endpoints.ipfs

        validatorAvatars =
            List.take 3 action.validators
                |> List.indexedMap
                    (\vIndex v ->
                        let
                            margin =
                                if vIndex /= 0 then
                                    " -ml-5"

                                else
                                    ""
                        in
                        ("h-10 w-10 border-white border-4 rounded-full bg-white" ++ margin)
                            |> Avatar.view ipfsUrl v.validator.avatar
                    )
                |> (\vals ->
                        let
                            numValidators =
                                List.length action.validators
                        in
                        if numValidators > 3 then
                            vals
                                ++ [ div
                                        [ class "h-10 w-10 flex flex-col border-white border-4 bg-grey rounded-full -ml-5" ]
                                        [ p [ class "text-date-purple m-auto text-xs font-black leading-none tracking-wide" ]
                                            [ text ("+" ++ String.fromInt (numValidators - 3)) ]
                                        ]
                                   ]

                        else
                            vals
                   )

        rewardStr =
            String.fromFloat action.reward ++ " " ++ Eos.symbolToString metadata.symbol

        ( usages, usagesLeft ) =
            ( String.fromInt action.usages, String.fromInt action.usagesLeft )

        tr r_id replaces =
            I18Next.tr loggedIn.shared.translations I18Next.Curly r_id replaces

        validationType : String
        validationType =
            action.verificationType
                |> VerificationType.toString
    in
    div [ class "py-6 px-2" ]
        [ div [ class "flex flex-col border-l-8 border-light-grey rounded-l-sm pl-2 sm:pl-6" ]
            [ span [ class "text-text-grey text-sm sm:text-base" ]
                [ text action.description ]
            , div [ class "flex flex-col sm:flex-row sm:items-center sm:justify-between" ]
                [ div [ class "text-xs mt-5 sm:w-1/3" ]
                    [ case action.deadline of
                        Just deadline ->
                            div []
                                [ span [ class "capitalize text-text-grey" ] [ text_ "community.actions.available_until" ]
                                , span [ class dateColor ] [ text deadlineStr ]
                                , span [] [ text_ "community.actions.or" ]
                                ]

                        Nothing ->
                            text ""
                    , if action.usages > 0 then
                        p [ class usagesColor ]
                            [ text (tr "community.actions.usages" [ ( "usages", usages ), ( "usagesLeft", usagesLeft ) ]) ]

                      else
                        text ""
                    ]
                , div [ class "sm:self-end" ]
                    [ div [ class "mt-3 flex flex-row items-center" ]
                        (if validationType == "CLAIMABLE" then
                            validatorAvatars

                         else
                            [ span [ class "text-date-purple uppercase text-sm mr-1" ]
                                [ text_ "community.actions.automatically_label" ]
                            , img [ src "/icons/tooltip.svg" ] []
                            ]
                        )
                    , div [ class "capitalize text-text-grey text-sm sm:text-right" ]
                        [ text_ "community.actions.verifiers" ]
                    ]
                ]
            , div [ class "mt-5 flex flex-row items-baseline" ]
                [ div [ class ("text-reward-green text-base mt-5 flex-grow-1" ++ rewardStrike) ]
                    [ span [] [ text (t "community.actions.reward" ++ ": ") ]
                    , span [ class "font-medium" ] [ text rewardStr ]
                    ]
                , div [ class "hidden sm:flex sm:visible flex-row justify-end flex-grow-1" ]
                    [ if canEdit then
                        button
                            [ class "bg-white uppercase underline w-4/5 h-10 text-button-orange" ]
                            [ text_ "menu.edit" ]

                      else
                        text ""
                    , if validationType == "CLAIMABLE" then
                        button
                            [ class ("h-10 uppercase rounded-lg ml-1" ++ claimColors ++ claimSize)
                            , onClick (OpenClaimConfirmation action.id)
                            ]
                            [ text_ claimText ]

                      else
                        text ""
                    ]
                ]
            ]
        , div [ class "flex flex-row mt-8 justify-between sm:hidden" ]
            [ if canEdit then
                button
                    [ class "bg-white rounded-lg uppercase w-4/5 h-10 text-button-orange border border-button-orange border-solid" ]
                    [ text_ "menu.edit" ]

              else
                text ""
            , if validationType == "CLAIMABLE" then
                button
                    [ class ("h-10 uppercase rounded-lg ml-1" ++ claimColors ++ claimSize)
                    , onClick (OpenClaimConfirmation action.id)
                    ]
                    [ text_ claimText ]

              else
                text ""
            ]
        ]


viewHeader : LoggedIn.Model -> Community -> Html Msg
viewHeader ({ shared } as loggedIn) community =
    div []
        [ div [ class "h-16 w-full bg-indigo-500 flex px-4 items-center" ]
            [ a
                [ class "items-center flex absolute"
                , Route.href Route.Communities
                ]
                [ Icons.back ""
                , p [ class "text-white text-sm ml-2" ]
                    [ text (t shared.translations "back")
                    ]
                ]
            , p [ class "text-white mx-auto" ] [ text community.title ]
            ]
        , div [ class "h-24 lg:h-56 bg-indigo-500 flex flex-wrap content-end" ]
            [ div [ class "h-24 w-24 h-24 w-24 rounded-full mx-auto pt-12" ]
                [ img
                    [ src (shared.endpoints.ipfs ++ "/" ++ community.logo)
                    , class "object-scale-down "
                    ]
                    []
                ]
            ]
        ]


viewMessageStatus : LoggedIn.Model -> Model -> Html msg
viewMessageStatus loggedIn model =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        text_ s =
            text (t s)
    in
    case model.messageStatus of
        None ->
            text ""

        Success message ->
            div [ class "z-40 bg-green fixed w-11/12 p-2" ]
                [ p [ class "text-white font-sans" ] [ text_ message ]
                ]

        Failure message ->
            div [ class "z-40 bg-red fixed w-11/12 p-2" ]
                [ p [ class "text-white font-sans font-bold" ] [ text_ message ]
                ]


viewClaimModal : LoggedIn.Model -> Model -> Html Msg
viewClaimModal loggedIn model =
    case model.modalStatus of
        Opened isLoading actionId ->
            let
                isDisabled =
                    if isLoading then
                        [ disabled True ]

                    else
                        []

                t s =
                    I18Next.t loggedIn.shared.translations s

                text_ s =
                    text (t s)
            in
            div [ class "modal container" ]
                [ div [ class "modal-bg", onClick CloseClaimConfirmation ] []
                , div [ class "modal-content" ]
                    [ div [ class "w-full" ]
                        [ p [ class "font-sans w-full font-bold text-heading text-2xl mb-4" ]
                            [ text_ "community.claimAction.title" ]
                        , button
                            ([ onClick CloseClaimConfirmation ]
                                ++ isDisabled
                            )
                            [ Icons.close "absolute fill-current text-gray-400 top-0 right-0 mx-8 my-4"
                            ]
                        , p [ class "text-body w-full font-sans mb-10" ]
                            [ text_ "community.claimAction.body" ]
                        ]
                    , div [ class "w-full md:bg-gray-100 md:flex md:absolute rounded-b-lg md:inset-x-0 md:bottom-0 md:p-4" ]
                        [ div [ class "flex-1" ] []
                        , button
                            ([ class "flex-1 block button button-secondary mb-4 button-large w-full md:w-40 md:mb-0"
                             , onClick CloseClaimConfirmation
                             ]
                                ++ isDisabled
                            )
                            [ text_ "community.claimAction.no" ]
                        , div [ class "flex-1" ] []
                        , button
                            ([ class "flex-1 block button button-primary button-large w-full md:w-40"
                             , onClick (ClaimAction actionId)
                             ]
                                ++ isDisabled
                            )
                            [ text_ "community.claimAction.yes" ]
                        , div [ class "flex-1" ] []
                        ]
                    ]
                ]

        Closed ->
            text ""



-- HELPERS


formField : List (Html msg) -> Html msg
formField =
    div [ class "form-field" ]


indexToString : Int -> String
indexToString index =
    let
        rawString =
            String.fromInt (index + 1)
    in
    if String.length rawString == 1 then
        "0" ++ rawString

    else
        rawString


viewFieldError : Shared -> String -> SaveStatus -> Html msg
viewFieldError shared fieldId save =
    case save of
        SaveFailed errors ->
            case Dict.get fieldId errors of
                Just e ->
                    span [ class "field-error" ] [ text (errorToString shared e) ]

                Nothing ->
                    text ""

        _ ->
            text ""


errorToString : Shared -> FormError -> String
errorToString shared v =
    let
        t s =
            I18Next.t shared.translations s

        tr str values =
            I18Next.tr shared.translations I18Next.Curly str values
    in
    case v of
        Required ->
            t "error.required"

        TooShort i ->
            tr "error.tooShort" [ ( "minLength", String.fromInt i ) ]

        TooLong i ->
            tr "error.tooLong" [ ( "maxLength", String.fromInt i ) ]

        InvalidChar c ->
            tr "error.invalidChar" [ ( "invalidChar", String.fromChar c ) ]

        AlreadyTaken ->
            t "error.alreadyTaken"

        NotMember ->
            t "error.notMember"



-- SECTIONS


viewSections : LoggedIn.Model -> Model -> List Transfer -> Html Msg
viewSections loggedIn model allTransfers =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        viewAccountName accountName =
            Eos.nameToString accountName

        transferInfo from value to =
            let
                val =
                    String.fromFloat value
            in
            [ ( "from", viewAccountName from )
            , ( "value", val )
            , ( "to", viewAccountName to )
            ]
                |> I18Next.tr loggedIn.shared.translations I18Next.Curly "transfer.info"
    in
    Page.viewMaxTwoColumn
        [ Page.viewTitle (t "community.actions.last_title")
        , Page.viewCardEmpty [ text (t "community.actions.no_actions_yet") ]
        ]
        [ Page.viewTitle (t "transfer.last_title")
        , case allTransfers of
            [] ->
                Page.viewCardEmpty [ text (t "transfer.no_transfers_yet") ]

            transfers ->
                Page.viewCardList
                    (List.map
                        (\transfer ->
                            ( [ text (transferInfo transfer.from transfer.value transfer.to)
                              , case transfer.memo of
                                    Nothing ->
                                        text ""

                                    Just mem ->
                                        p [ class "card__list-memo" ]
                                            [ text mem ]
                              ]
                            , Utils.posixDateTime (Just transfer.blockTime)
                            , model.date
                            )
                        )
                        transfers
                    )
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = GotTime Posix
    | CompletedLoadCommunity (Result (Graphql.Http.Error (Maybe Community)) (Maybe Community))
      -- Objective
    | ClickedOpenObjective Int
    | ClickedCloseObjective
      -- Action
    | CreateAction Symbol String
    | OpenClaimConfirmation Int
    | CloseClaimConfirmation
    | ClaimAction Int
    | GotClaimActionResponse (Result Value String)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        GotTime date ->
            UR.init { model | date = Just date }

        CompletedLoadCommunity (Ok community) ->
            case community of
                Just c ->
                    UR.init { model | community = Loaded c NoEdit }

                Nothing ->
                    UR.init { model | community = NotFound }

        CompletedLoadCommunity (Err err) ->
            { model | community = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        ClickedOpenObjective index ->
            { model | openObjective = Just index }
                |> UR.init

        ClickedCloseObjective ->
            { model | openObjective = Nothing }
                |> UR.init

        CreateAction sym id ->
            model
                |> UR.init
                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey (Route.NewAction sym id))

        OpenClaimConfirmation actionId ->
            { model | modalStatus = Opened False actionId }
                |> UR.init

        CloseClaimConfirmation ->
            { model | modalStatus = Closed }
                |> UR.init

        ClaimAction actionId ->
            case LoggedIn.isAuth loggedIn of
                True ->
                    model
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ClaimAction actionId
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    { actions =
                                        [ { accountName = "bes.cmm"
                                          , name = "claimaction"
                                          , authorization =
                                                { actor = loggedIn.accountName
                                                , permissionName = Eos.samplePermission
                                                }
                                          , data =
                                                { actionId = actionId
                                                , maker = loggedIn.accountName
                                                }
                                                    |> Community.encodeClaimAction
                                          }
                                        ]
                                    }
                            }

                False ->
                    model
                        |> UR.init
                        |> UR.addExt (Just (ClaimAction actionId) |> RequiredAuthentication)

        GotClaimActionResponse (Ok txId) ->
            { model
                | modalStatus = Closed
                , messageStatus = Success "community.claimAction.success"
            }
                |> UR.init

        GotClaimActionResponse (Err v) ->
            { model
                | modalStatus = Closed
                , messageStatus = Failure "community.claimAction.failure"
            }
                |> UR.init


updateCommunity : Model -> LoadStatus -> Model
updateCommunity model c =
    { model | community = c }


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClaimAction" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string |> Decode.map Ok
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotClaimActionResponse)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        GotTime _ ->
            [ "GotTime" ]

        CompletedLoadCommunity r ->
            [ "CompletedLoadCommunity", UR.resultToString r ]

        ClickedOpenObjective _ ->
            [ "ClickedOpenObjective" ]

        ClickedCloseObjective ->
            [ "ClickedCloseObjective" ]

        CreateAction _ _ ->
            [ "CreateAction" ]

        OpenClaimConfirmation _ ->
            [ "OpenClaimConfirmation" ]

        CloseClaimConfirmation ->
            [ "CloseClaimConfirmation" ]

        ClaimAction _ ->
            [ "ClaimAction" ]

        GotClaimActionResponse r ->
            [ "GotClaimActionResponse", UR.resultToString r ]
