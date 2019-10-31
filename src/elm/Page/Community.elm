module Page.Community exposing (Model, Msg, init, jsAddressToMsg, msgToString, subscriptions, update, view)

import Account
import Api
import Api.Graphql
import Asset.Icon as Icon
import Avatar exposing (Avatar)
import Bespiral.Enum.VerificationType as VerificationType exposing (VerificationType)
import Bespiral.Object
import Bespiral.Query exposing (ClaimsRequiredArguments)
import Bespiral.Scalar exposing (DateTime(..))
import Community exposing (ActionVerification, ActionVerificationsResponse, ClaimResponse, Community, ObjectiveId, Validator)
import Dict exposing (Dict)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, targetValue)
import Html.Lazy
import Http
import I18Next exposing (Translations)
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
import View.Tag as Tag



-- INIT


init : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) symbol =
    ( initModel loggedIn
    , Cmd.batch
        [ Api.Graphql.query shared (Community.communityQuery symbol) CompletedLoadCommunity
        , fetchCommunityActions shared symbol
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


fetchCommunityActions : Shared -> Symbol -> Cmd Msg
fetchCommunityActions shared sym =
    let
        stringSymbol : String
        stringSymbol =
            Eos.symbolToString sym

        selectionSet : SelectionSet ActionVerificationsResponse RootQuery
        selectionSet =
            verificationHistorySelectionSet stringSymbol
    in
    Api.Graphql.query
        shared
        selectionSet
        CompletedLoadActions



-- Actions SelectionSet


verificationHistorySelectionSet : String -> SelectionSet ActionVerificationsResponse RootQuery
verificationHistorySelectionSet stringSym =
    let
        vInput : ClaimsRequiredArguments
        vInput =
            { input = { validator = Absent, claimer = Absent, symbol = Present stringSym } }

        selectionSet : SelectionSet ClaimResponse Bespiral.Object.Claim
        selectionSet =
            Community.claimSelectionSet stringSym
    in
    SelectionSet.succeed ActionVerificationsResponse
        |> with (Bespiral.Query.claims vInput selectionSet)



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
    , actions : Maybe ActionVerificationsResponse
    }


initModel : LoggedIn.Model -> Model
initModel loggedIn =
    { date = Nothing
    , community = Loading
    , members = []
    , openObjective = Nothing
    , modalStatus = Closed
    , messageStatus = None
    , actions = Nothing
    }


type LoadStatus
    = Loading
    | Loaded Community EditStatus
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Community))


type EditStatus
    = NoEdit
    | OpenObjective Int
    | NewObjective ObjectiveForm
    | EditObjective Int ObjectiveForm
    | NewAction Int ActionForm
    | EditAction Int Int ActionForm


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


type alias ActionForm =
    { description : String
    , reward : String
    , verification : Verification
    , verificationReward : String
    , database : String
    , save : SaveStatus
    , validators : List String
    }


emptyActionForm : ActionForm
emptyActionForm =
    { description = ""
    , reward = ""
    , verification = Manually
    , verificationReward = ""
    , database = ""
    , save = NotAsked
    , validators = []
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


actionFormToAction : LoggedIn.Model -> ActionForm -> Community.Action
actionFormToAction loggedIn action =
    { description = action.description
    , reward = String.toFloat action.reward |> Maybe.withDefault 0
    , verificationReward = String.toFloat action.verificationReward |> Maybe.withDefault 0
    , creator = loggedIn.accountName
    , validators = []
    , usages = 0
    , usagesLeft = 0
    , deadline = Just (DateTime "")
    , verificationType = VerificationType.Automatic
    , id = 0
    }



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
            div [ class "container mx-auto px-4" ]
                [ viewClaimModal loggedIn model
                , viewMessageStatus loggedIn model
                , Page.viewTitle (Eos.symbolToString community.symbol ++ " - " ++ community.title)
                , div [ class "card card--community-view" ]
                    [ div [ class "card-community-view" ]
                        [ h3 [ class "card-community-view__description-title" ]
                            [ text_ "community.create.labels.description" ]
                        , div [ class "card-community-view__description-container" ]
                            [ div
                                [ class "card__image-background"
                                , Community.logoBackground ipfsUrl
                                    (Just community.logo)
                                ]
                                []
                            , p [ class "card-community-view__description" ] [ text community.description ]
                            ]
                        ]
                    , if canEdit then
                        a
                            [ classList
                                [ ( "edit-button", True )
                                , ( "circle-background", True )
                                , ( "circle-background--primary", True )
                                , ( "hidden", editStatus /= NoEdit )
                                ]
                            , Route.href (Route.EditCommunity community.symbol)
                            ]
                            [ Icon.edit "" ]

                      else
                        text ""
                    ]
                , div [ class "bg-white py-6 sm:py-8 px-3 sm:px-6 rounded-lg sm:rounded" ]
                    ([ Page.viewTitle (t "community.objectives.title_plural") ]
                        ++ List.indexedMap (viewObjective loggedIn model editStatus community)
                            community.objectives
                        ++ [ if canEdit then
                                viewObjectiveNew loggedIn (List.length community.objectives) editStatus

                             else
                                text ""
                           ]
                    )
                , Transfer.getTransfers (Just community)
                    |> viewSections loggedIn model
                , a
                    [ class "btn btn--outline btn--big"
                    , style "margin-top" "2rem"
                    , Route.href Route.Dashboard
                    ]
                    [ text_ "menu.back_to_dashboard" ]
                ]



-- VIEW VERIFICATIONS


viewVerification : String -> ActionVerification -> Html Msg
viewVerification url verification =
    let
        maybeLogo =
            if String.isEmpty verification.logo then
                Nothing

            else
                Just (url ++ "/" ++ verification.logo)

        description =
            verification.description

        date =
            Just verification.createdAt
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc

        status =
            verification.status

        route =
            case verification.symbol of
                Just symbol ->
                    Route.VerifyClaim
                        symbol
                        (String.fromInt verification.objectiveId)
                        (String.fromInt verification.actionId)
                        (String.fromInt verification.claimId)

                Nothing ->
                    Route.ComingSoon
    in
    a
        [ class "border-b last:border-b-0 border-gray-500 flex items-start lg:items-center hover:bg-gray-100 first-hover:rounded-t-lg last-hover:rounded-b-lg p-4"
        , Route.href route
        ]
        [ div
            [ class "flex-none" ]
            [ case maybeLogo of
                Just logoUrl ->
                    img
                        [ class "w-10 h-10 object-scale-down"
                        , src logoUrl
                        ]
                        []

                Nothing ->
                    div
                        [ class "w-10 h-10 object-scale-down" ]
                        []
            ]
        , div
            [ class "flex-col flex-grow-1 pl-4" ]
            [ p
                [ class "font-sans text-black text-sm leading-relaxed" ]
                [ text description ]
            , p
                [ class "font-normal font-sans text-gray-900 text-caption uppercase" ]
                [ text date ]
            , div
                [ class "lg:hidden mt-4" ]
                [ Tag.view status ]
            ]
        , div
            [ class "hidden lg:visible lg:flex lg:flex-none pl-4" ]
            [ Tag.view status ]
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
            , if isOpen then
                onClick ClickedCloseObjective

              else
                onClick (ClickedOpenObjective index)
            ]
            [ div [ class "sm:flex-grow-7 sm:w-5/12" ]
                [ div
                    [ class "truncate overflow-hidden whitespace-no-wrap text-white font-medium text-sm overflow-hidden sm:flex-grow-8 sm:leading-normal sm:text-lg" ]
                    [ text objective.description ]
                ]
            , div [ class ("flex flex-row justify-between mt-5 sm:mt-0" ++ arrowStyle) ]
                [ if canEdit then
                    span [ class "text-white font-medium underline text-xs uppercase sm:text-sm" ] [ text_ "menu.edit" ]

                  else
                    text ""
                , if isOpen then
                    button [ class "align-middle" ]
                        [ img
                            [ class "rotate-180 fill-current text-white h-2 w-4 stroke-current"
                            , src "/icons/objective_arrow.svg"
                            ]
                            []
                        ]

                  else
                    button [ class "align-middle" ]
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


viewObjectiveForm : LoggedIn.Model -> Int -> ObjectiveForm -> Html Msg
viewObjectiveForm loggedIn index objective =
    let
        text_ s =
            text (I18Next.t loggedIn.shared.translations s)

        isDisabled =
            objective.save == Saving
    in
    Html.form [ onSubmit ClickedSaveObjective ]
        [ viewObjectiveFieldDescription loggedIn isDisabled ("comm-obj-desc-" ++ String.fromInt index) objective
        , div [ class "btn-row" ]
            [ button
                [ classList
                    [ ( "btn", True )
                    , ( "btn--primary", True )
                    , ( "btn--outline", True )
                    ]
                , disabled isDisabled
                , type_ "button"
                , onClick ClickedEditCancel
                ]
                [ text_ "menu.cancel" ]
            , button
                [ classList
                    [ ( "btn", True )
                    , ( "btn--primary", True )
                    ]
                , disabled isDisabled
                ]
                [ text_ "menu.save" ]
            ]
        ]


viewObjectiveFieldDescription : LoggedIn.Model -> Bool -> String -> ObjectiveForm -> Html Msg
viewObjectiveFieldDescription loggedIn isDisabled id_ objective =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    formField
        [ label [ for id_ ] [ text (t "community.objectives.description_label") ]
        , textarea
            [ id id_
            , class "input"
            , onInput EnteredObjectiveDescription
            , placeholder (t "community.objectives.description_placeholder")
            , value objective.description
            , disabled isDisabled
            , required True
            , maxlength 254
            ]
            []
        , viewFieldError loggedIn.shared id_ objective.save
        ]


viewObjectiveNew : LoggedIn.Model -> Int -> EditStatus -> Html Msg
viewObjectiveNew loggedIn index edit =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    case edit of
        NewObjective objForm ->
            div
                [ classList
                    [ ( "card", True )
                    , ( "card--form", True )
                    ]
                , style "margin-bottom" "3rem"
                ]
                [ h3 [ class "form-title form-title--with-button" ]
                    [ span [] [ text (t "community.objectives.title" ++ " " ++ indexToString index) ] ]
                , viewObjectiveForm loggedIn index objForm
                ]

        _ ->
            button
                [ class "border border-dashed border-button-orange mt-6 w-full flex flex-row content-start px-4 py-2"
                , onClick ClickedNewObjective
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
            LoggedIn.isAccount metadata.creator loggedIn

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

        toView verifications =
            List.map
                (viewVerification loggedIn.shared.endpoints.ipfs)
                verifications
    in
    Page.viewMaxTwoColumn
        [ Page.viewTitle (t "community.actions.last_title")
        , case model.actions of
            Nothing ->
                Page.viewCardEmpty [ text (t "community.actions.no_actions_yet") ]

            Just resp ->
                div []
                    [ if List.isEmpty resp.claims then
                        Page.viewCardEmpty [ text (t "community.actions.no_actions_yet") ]

                      else
                        div
                            [ class "shadow-md rounded-lg bg-white mt-5" ]
                            (toView (Community.toVerifications resp))
                    ]
        ]
        [ Page.viewTitle (t "transfer.last_title")
        , case allTransfers of
            [] ->
                Page.viewCardEmpty [ text (t "community.actions.no_transfers_yet") ]

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
    | EnteredObjectiveDescription String
    | ClickedSaveObjective
    | GotSaveObjectiveResponse (Result Value String)
    | ClickedNewObjective
    | ClickedOpenObjective Int
    | ClickedCloseObjective
    | ClickedEditObjective Int Community.Objective
    | ClickedEditCancel
      -- Action
    | CreateAction Symbol String
    | OpenClaimConfirmation Int
    | CloseClaimConfirmation
    | ClaimAction Int
    | GotClaimActionResponse (Result Value String)
    | CompletedLoadActions (Result (Graphql.Http.Error ActionVerificationsResponse) ActionVerificationsResponse)


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

        CompletedLoadActions (Ok resp) ->
            { model | actions = Just resp }
                |> UR.init

        CompletedLoadActions (Err err) ->
            model
                |> UR.init
                |> UR.logGraphqlError msg err

        EnteredObjectiveDescription s ->
            UR.init model
                |> updateObjective msg (\o -> { o | description = s })

        ClickedSaveObjective ->
            let
                saveObjective obj comm toModel =
                    toModel { obj | save = Saving }
                        |> updateCommunity model
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ClickedSaveObjective
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    { actions =
                                        [ { accountName = "bes.cmm"
                                          , name = "newobjective"
                                          , authorization =
                                                { actor = loggedIn.accountName
                                                , permissionName = Eos.samplePermission
                                                }
                                          , data =
                                                { symbol = comm.symbol
                                                , description = obj.description
                                                , creator = loggedIn.accountName
                                                }
                                                    |> Community.encodeCreateObjectiveAction
                                          }
                                        ]
                                    }
                            }
            in
            case ( model.community, LoggedIn.isAuth loggedIn ) of
                ( _, False ) ->
                    UR.init model
                        |> UR.addExt
                            (Just ClickedSaveObjective
                                |> RequiredAuthentication
                            )

                ( Loaded community (NewObjective objective), True ) ->
                    saveObjective
                        objective
                        community
                        (Loaded community << NewObjective)

                ( Loaded community (EditObjective index objective), True ) ->
                    saveObjective
                        objective
                        community
                        (Loaded community << EditObjective index)

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        GotSaveObjectiveResponse (Ok txId) ->
            case model.community of
                Loaded community (NewObjective objective) ->
                    UR.init model
                        |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey (Route.Community community.symbol))

                Loaded community (EditObjective index objective) ->
                    { model
                        | community =
                            Loaded
                                { community
                                    | objectives =
                                        List.indexedMap
                                            (\i o ->
                                                if i == index then
                                                    { o | description = objective.description }

                                                else
                                                    o
                                            )
                                            community.objectives
                                }
                                NoEdit
                    }
                        |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        GotSaveObjectiveResponse (Err v) ->
            UR.init model
                |> updateObjective msg (\o -> { o | save = SaveFailed Dict.empty })
                |> UR.logDebugValue msg v

        ClickedNewObjective ->
            case model.community of
                Loaded community NoEdit ->
                    { model
                        | community =
                            NewObjective emptyObjectiveForm
                                |> Loaded community
                    }
                        |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        ClickedEditObjective index objective ->
            case model.community of
                Loaded community NoEdit ->
                    { model
                        | community =
                            { emptyObjectiveForm
                                | description = objective.description
                            }
                                |> EditObjective index
                                |> Loaded community
                    }
                        |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        ClickedEditCancel ->
            case model.community of
                Loaded community _ ->
                    UR.init { model | community = Loaded community NoEdit }

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

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


updateObjective : Msg -> (ObjectiveForm -> ObjectiveForm) -> UpdateResult -> UpdateResult
updateObjective msg fn uResult =
    case uResult.model.community of
        Loaded community (NewObjective objective) ->
            UR.mapModel
                (\m ->
                    { m
                        | community =
                            Loaded community (NewObjective (fn objective))
                    }
                )
                uResult

        Loaded community (EditObjective index objective) ->
            UR.mapModel
                (\m ->
                    { m
                        | community =
                            Loaded community (EditObjective index (fn objective))
                    }
                )
                uResult

        _ ->
            uResult
                |> UR.logImpossible msg []


updateAction : Msg -> (ActionForm -> ActionForm) -> UpdateResult -> UpdateResult
updateAction msg fn uResult =
    case uResult.model.community of
        Loaded community (NewAction objIndex action) ->
            UR.mapModel
                (\m ->
                    Loaded community (NewAction objIndex (fn action))
                        |> updateCommunity m
                )
                uResult

        Loaded community (EditAction objIndex index action) ->
            UR.mapModel
                (\m ->
                    Loaded community (EditAction objIndex index (fn action))
                        |> updateCommunity m
                )
                uResult

        _ ->
            uResult
                |> UR.logImpossible msg []


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

        ClickedSaveObjective ->
            [ "ClickedSaveObjective" ]

        EnteredObjectiveDescription _ ->
            [ "EnteredObjectiveDescription" ]

        GotSaveObjectiveResponse r ->
            [ "GotSaveObjectiveResponse", UR.resultToString r ]

        ClickedNewObjective ->
            [ "ClickedNewObjective" ]

        ClickedOpenObjective _ ->
            [ "ClickedOpenObjective" ]

        ClickedCloseObjective ->
            [ "ClickedCloseObjective" ]

        ClickedEditObjective _ _ ->
            [ "ClickedEditObjective" ]

        ClickedEditCancel ->
            [ "ClickedEditCancel" ]

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

        CompletedLoadActions _ ->
            [ "CompletedLoadActions" ]
