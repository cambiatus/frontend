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
import I18Next exposing (Translations)
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
    , invite : String
    , inviteStatus : InviteStatus
    , openObjective : Maybe Int
    }


initModel : LoggedIn.Model -> Model
initModel loggedIn =
    { date = Nothing
    , community = Loading
    , members = []
    , invite = ""
    , inviteStatus = Editing
    , openObjective = Nothing
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


type InviteStatus
    = Editing
    | Sending
    | SendingFailed Http.Error
    | SendingSucceed


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
    , invite : String
    , invites : List String
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
    , invite = ""
    , invites = []
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

    -- Find the validators
    , validators = []
    , usages = 0
    , usagesLeft = 0
    , deadline = DateTime ""
    , verificationType = VerificationType.Automatic
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
            div [ class "main-content__container create-community" ]
                [ Page.viewTitle (Eos.symbolToString community.symbol ++ " - " ++ community.title)
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
                        , label [ for "community-invite" ]
                            [ text_ "community.invite.title" ]
                        , Html.form
                            [ onSubmit (ClickedInviteEmail community.symbol)
                            , class "input-group"
                            ]
                            [ input
                                [ id "community-invite"
                                , type_ "email"
                                , class "input flex100"
                                , onInput EnteredInviteEmail
                                , plcH "community.invite.placeholders.emails"
                                , disabled (model.inviteStatus == Sending)
                                , required True
                                ]
                                []
                            , button
                                [ class "btn btn--outline"
                                , style "flex-shrink" "0"
                                , disabled (model.inviteStatus == Sending)
                                ]
                                [ text_ "community.invite.submit" ]
                            ]
                        , case model.inviteStatus of
                            SendingFailed e ->
                                span [ class "field-error" ]
                                    [ text_ "community.invite.succeed" ]

                            SendingSucceed ->
                                span [ class "field-succeed" ]
                                    [ text_ "community.invite.failed" ]

                            _ ->
                                text ""
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

        actsNButton : List (Html Msg)
        actsNButton =
            List.map (viewAction loggedIn metadata model.date) objective.actions
                |> List.intersperse (hr [ class "bg-border-grey text-border-grey" ] [])
                |> (\acts ->
                        if canEdit then
                            acts
                                ++ [ button
                                        [ class "border border-dashed border-button-orange mt-6 w-full flex flex-row content-start px-4 py-2" ]
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
                |> Just
                |> Utils.posixDateTime

        deadlineStr : String
        deadlineStr =
            posixDeadline
                |> Strftime.format "%d %B %Y" Time.utc

        pastDeadline : Bool
        pastDeadline =
            case maybeDate of
                Just today ->
                    posixToMillis today > posixToMillis posixDeadline

                Nothing ->
                    False

        rewardStrike : String
        rewardStrike =
            if pastDeadline || action.usagesLeft < 1 then
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
            if action.usagesLeft >= 1 then
                " text-date-purple"

            else
                " text-date-red"

        ( claimColors, claimText ) =
            if pastDeadline || action.usagesLeft < 1 then
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
                    [ span [ class "capitalize text-text-grey" ] [ text_ "community.actions.available_until" ]
                    , span [ class dateColor ] [ text deadlineStr ]
                    , span [] [ text_ "community.actions.or" ]
                    , p [ class usagesColor ]
                        [ text (tr "community.actions.usages" [ ( "usages", usages ), ( "usagesLeft", usagesLeft ) ]) ]
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
                    , button
                        [ class ("h-10 uppercase rounded-lg ml-1" ++ claimColors ++ claimSize) ]
                        [ text_ claimText ]
                    ]
                ]
            ]
        , div [ class "flex flex-row mt-8 justify-between sm:hidden" ]
            [ if canEdit then
                button
                    [ class "bg-white rounded-lg uppercase w-4/5 h-10 text-button-orange border border-button-orange border-solid" ]
                    [ text "menu.edit" ]

              else
                text ""
            , button
                [ class ("h-10 uppercase rounded-lg ml-1" ++ claimColors ++ claimSize) ]
                [ text_ claimText ]
            ]
        ]


viewActionForm : LoggedIn.Model -> List Member -> Int -> Int -> ActionForm -> Html Msg
viewActionForm loggedIn members objIndex actionIndex action =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        uniqueId s =
            s ++ String.fromInt objIndex ++ String.fromInt actionIndex

        isDisabled =
            action.save == Saving
    in
    Html.form
        [ class "form-field form-field--secondary"
        , onSubmit ClickedSaveAction
        ]
        [ h4 [ class "form-title" ]
            [ text (t "community.actions.title" ++ " " ++ indexToString actionIndex) ]
        , viewActionFieldDescription loggedIn isDisabled (uniqueId "comm-action-") action
        , viewFieldReward loggedIn isDisabled (uniqueId "comm-reward-") action
        , viewFieldVerification loggedIn isDisabled (uniqueId "comm-verif-") action
        , viewFieldInvite loggedIn members isDisabled (uniqueId "comm-invite-") action
        , viewFieldVerificationReward loggedIn isDisabled (uniqueId "comm-verif-reward-") action
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
                [ text (t "menu.cancel") ]
            , button
                [ classList
                    [ ( "btn", True )
                    , ( "btn--primary", True )
                    ]
                , disabled isDisabled
                ]
                [ text (t "menu.save") ]
            ]
        ]


viewActionFieldDescription : LoggedIn.Model -> Bool -> String -> ActionForm -> Html Msg
viewActionFieldDescription loggedIn isDisabled id_ action =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    formField
        [ label [ for id_ ]
            [ text (t "community.actions.description_label") ]
        , input
            [ id id_
            , type_ "text"
            , class "input"
            , value action.description
            , onInput EnteredActionDescription
            , disabled isDisabled
            ]
            []
        , viewFieldError loggedIn.shared id_ action.save
        ]


viewFieldReward : LoggedIn.Model -> Bool -> String -> ActionForm -> Html Msg
viewFieldReward loggedIn isDisabled id_ action =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    formField
        [ Page.labelWithTooltip id_ (t "community.actions.reward") (t "community.actions.reward_label")
        , input
            [ id id_
            , type_ "number"
            , class "input"
            , value action.reward
            , Html.Attributes.min "0"
            , onInput EnteredReward
            , disabled isDisabled
            ]
            []
        , viewFieldError loggedIn.shared id_ action.save
        ]


viewFieldVerification : LoggedIn.Model -> Bool -> String -> ActionForm -> Html Msg
viewFieldVerification loggedIn isDisabled id_ action =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    formField
        [ label [ for id_ ]
            [ text (t "community.actions.verification_label") ]
        , select
            [ id id_
            , class "input"
            , Html.Events.on "change"
                (Decode.map
                    (\s ->
                        if s == t "community.actions.manually_label" then
                            EnteredVerification Manually

                        else
                            EnteredVerification Automatically
                    )
                    targetValue
                )
            , disabled True
            ]
            [ option
                [ selected (action.verification == Manually) ]
                [ text (t "community.actions.manually_label") ]
            , option
                [ selected (action.verification == Automatically) ]
                [ text (t "community.actions.automatically_label") ]
            ]
        , span [ class "form-field-description" ] [ text (t "community.actions.manually_info") ]
        ]


viewFieldInvite : LoggedIn.Model -> List Member -> Bool -> String -> ActionForm -> Html Msg
viewFieldInvite loggedIn members isDisabled id_ action =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    formField
        [ Page.labelWithTooltip id_ (t "community.actions.invite_label") (t "community.actions.invite_tooltip")
        , Html.form
            [ onSubmit AddInvite
            , class "input-group"
            ]
            [ input
                [ id id_
                , type_ "text"
                , class "input flex100"
                , onInput EnteredInvite
                , placeholder (t "community.actions.invite_email_or_username")
                , value action.invite
                , disabled isDisabled
                , required True
                , list (id_ ++ "-list")
                ]
                []
            , Html.Lazy.lazy3 viewInviteDatalist id_ members action.invites
            , button
                [ class "btn btn--outline flex000"
                , disabled isDisabled
                ]
                [ text (t "community.actions.invite_label") ]
            ]
        , viewFieldError loggedIn.shared id_ action.save
        , div [ class "form-tags" ]
            (List.indexedMap
                (\i s ->
                    div []
                        [ span [] [ text s ]
                        , button
                            [ onClick (RemoveInvite i)
                            , disabled isDisabled
                            , type_ "button"
                            ]
                            [ Icon.close "" ]
                        ]
                )
                action.invites
            )
        ]


viewInviteDatalist : String -> List Member -> List String -> Html Msg
viewInviteDatalist id_ members invites =
    datalist [ id (id_ ++ "-list") ]
        (List.map
            (\m ->
                if List.member m.nameWithAt invites then
                    text ""

                else
                    option [ value m.nameWithAt ] []
            )
            members
        )


viewFieldVerificationReward : LoggedIn.Model -> Bool -> String -> ActionForm -> Html Msg
viewFieldVerificationReward loggedIn isDisabled id_ action =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    formField
        [ label [ for id_ ]
            [ text (t "community.actions.manually_reward") ]
        , input
            [ id id_
            , type_ "number"
            , class "input"
            , onInput EnteredVerificationReward
            , value action.verificationReward
            , Html.Attributes.min "0"
            , disabled isDisabled
            ]
            []
        , viewFieldError loggedIn.shared id_ action.save
        ]


viewActionNew : LoggedIn.Model -> List Member -> EditStatus -> Int -> Int -> Html Msg
viewActionNew loggedIn members editStatus objIndex index =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        viewButton =
            button
                [ classList
                    [ ( "card__full-button", True )
                    , ( "hidden", editStatus /= NoEdit )
                    ]
                , onClick (ClickedNewAction objIndex)
                , disabled (editStatus /= NoEdit)
                ]
                [ span [ class "card__plus-symbol" ] [ text "+" ]
                , span [] [ text (t "community.actions.new") ]
                ]
    in
    case editStatus of
        NewAction objIndex_ actionForm ->
            if objIndex == objIndex_ then
                viewActionForm loggedIn members objIndex_ index actionForm

            else
                viewButton

        _ ->
            viewButton



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
            [ ( "from", viewAccountName from )
            , ( "value", value )
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
    | EnteredInviteEmail String
    | ClickedInviteEmail Symbol
    | CompletedInviteEmail (Result Http.Error ())
    | EnteredObjectiveDescription String
    | ClickedSaveObjective
    | GotSaveObjectiveResponse (Result Value String)
    | CompletedLastObjectiveId (Result Http.Error ObjectiveId)
    | ClickedNewObjective
    | ClickedOpenObjective Int
    | ClickedCloseObjective
    | ClickedEditObjective Int Community.Objective
    | ClickedEditCancel
      -- Action Msgs
    | ClickedEditAction Int Int Community.Action
    | EnteredActionDescription String
    | EnteredReward String
    | EnteredVerification Verification
    | EnteredInvite String
    | AddInvite
    | RemoveInvite Int
    | EnteredVerificationReward String
    | ClickedNewAction Int
    | ClickedSaveAction
    | GotSaveActionResponse (Result Value String)


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

        ClickedInviteEmail symbol ->
            { model | inviteStatus = Sending }
                |> UR.init
                |> UR.addCmd
                    (Api.communityInvite loggedIn.shared symbol loggedIn.accountName model.invite CompletedInviteEmail)

        EnteredInviteEmail s ->
            { model
                | invite = s
                , inviteStatus = Editing
            }
                |> UR.init

        CompletedInviteEmail (Ok ()) ->
            UR.init { model | inviteStatus = SendingSucceed }

        CompletedInviteEmail (Err err) ->
            UR.init { model | inviteStatus = SendingFailed err }

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
                        |> UR.addCmd
                            (fetchLastObjectiveId loggedIn CompletedLastObjectiveId)

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

        CompletedLastObjectiveId (Ok objId) ->
            case model.community of
                Loaded community (NewObjective objective) ->
                    { model
                        | community =
                            Loaded
                                { community
                                    | objectives =
                                        community.objectives
                                            ++ [ { id = objId
                                                 , description = objective.description
                                                 , creator = loggedIn.accountName
                                                 , actions = []
                                                 }
                                               ]
                                }
                                NoEdit
                    }
                        |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        CompletedLastObjectiveId (Err err) ->
            UR.init model
                |> updateObjective msg (\o -> { o | save = SaveFailed Dict.empty })
                |> UR.logHttpError msg err

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

        -- Action Msgs
        ClickedEditAction commIndex index action ->
            case model.community of
                Loaded community NoEdit ->
                    { emptyActionForm
                        | description = action.description
                        , reward = String.fromFloat action.reward
                    }
                        |> EditAction commIndex index
                        |> Loaded community
                        |> updateCommunity model
                        |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        EnteredActionDescription s ->
            UR.init model
                |> updateAction msg (\a -> { a | description = s })

        EnteredReward s ->
            UR.init model
                |> updateAction msg (\a -> { a | reward = s })

        EnteredVerification v ->
            UR.init model
                |> updateAction msg (\a -> { a | verification = v })

        EnteredInvite s ->
            UR.init model
                |> updateAction msg (\a -> { a | invite = s })

        AddInvite ->
            UR.init model
                |> updateAction msg
                    (\a ->
                        if not (String.startsWith "@" a.invite) || List.any (\m -> a.invite == m.nameWithAt) model.members then
                            { a
                                | invite = ""
                                , invites =
                                    a.invites
                                        ++ (String.split "," a.invite
                                                |> List.map String.trim
                                           )
                                , save = NotAsked
                            }

                        else
                            { a | save = SaveFailed (Dict.singleton "comm-invite-00" NotMember) }
                     -- TODO: validar email e quando tiver objectiveId, usar ao invés do "comm-invite-00" hardcoded.
                    )

        RemoveInvite index ->
            UR.init model
                |> updateAction msg
                    (\a ->
                        { a
                            | invites =
                                List.indexedMap
                                    (\i inv ->
                                        if i == index then
                                            Nothing

                                        else
                                            Just inv
                                    )
                                    a.invites
                                    |> List.filterMap identity
                        }
                    )

        EnteredVerificationReward s ->
            UR.init model
                |> updateAction msg (\a -> { a | verificationReward = s })

        ClickedNewAction objIndex ->
            case model.community of
                Loaded community NoEdit ->
                    NewAction objIndex emptyActionForm
                        |> Loaded community
                        |> updateCommunity model
                        |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        ClickedSaveAction ->
            let
                toPort comm obj a =
                    { responseAddress = ClickedSaveAction
                    , responseData = Encode.null
                    , data =
                        Eos.encodeTransaction
                            { actions =
                                [ { accountName = "bes.cmm"
                                  , name = "newaction"
                                  , authorization =
                                        { actor = loggedIn.accountName
                                        , permissionName = Eos.samplePermission
                                        }
                                  , data =
                                        { objective_id = obj.id
                                        , description = a.description
                                        , reward = a.reward ++ " " ++ Eos.symbolToString comm.symbol
                                        , verifier_reward = a.verificationReward ++ " " ++ Eos.symbolToString comm.symbol
                                        , creator = loggedIn.accountName
                                        }
                                            |> Community.encodeCreateActionAction
                                  }
                                ]
                            }
                    }

                saveAction a objIndex comm toModel =
                    case List.head (List.drop objIndex comm.objectives) of
                        Just obj ->
                            toModel { a | save = Saving }
                                |> Loaded comm
                                |> updateCommunity model
                                |> UR.init
                                |> UR.addPort (toPort comm obj a)

                        Nothing ->
                            UR.init model
                                |> UR.logImpossible msg []
            in
            case ( model.community, LoggedIn.isAuth loggedIn ) of
                ( _, False ) ->
                    UR.init model
                        |> UR.addExt
                            (Just ClickedSaveAction
                                |> RequiredAuthentication
                            )

                ( Loaded community (NewAction objIndex action), True ) ->
                    saveAction action objIndex community (NewAction objIndex)

                ( Loaded community (EditAction objIndex index action), True ) ->
                    saveAction action objIndex community (EditAction objIndex index)

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        GotSaveActionResponse (Ok txId) ->
            let
                updateObjectiveAction comm objIndex fn =
                    { comm
                        | objectives =
                            List.indexedMap
                                (\i obj ->
                                    if i == objIndex then
                                        fn obj

                                    else
                                        obj
                                )
                                comm.objectives
                    }
            in
            case model.community of
                Loaded community (NewAction objIndex action) ->
                    Loaded
                        (updateObjectiveAction community
                            objIndex
                            (\obj ->
                                { obj
                                    | actions =
                                        obj.actions
                                            ++ [ actionFormToAction loggedIn action ]
                                }
                            )
                        )
                        NoEdit
                        |> updateCommunity model
                        |> UR.init

                Loaded community (EditAction objIndex index action) ->
                    Loaded
                        (updateObjectiveAction community
                            objIndex
                            (\obj ->
                                { obj
                                    | actions =
                                        List.indexedMap
                                            (\i a ->
                                                if i == index then
                                                    actionFormToAction loggedIn action

                                                else
                                                    a
                                            )
                                            obj.actions
                                }
                            )
                        )
                        NoEdit
                        |> updateCommunity model
                        |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        GotSaveActionResponse (Err v) ->
            UR.init model
                |> UR.logDebugValue msg v
                |> updateAction msg (\a -> { a | save = SaveFailed Dict.empty })


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

        "ClickedSaveAction" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotSaveActionResponse)
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

        EnteredInviteEmail _ ->
            [ "EnteredInviteEmail" ]

        ClickedInviteEmail _ ->
            [ "ClickedInviteEmail" ]

        CompletedInviteEmail r ->
            [ "CompletedInviteEmail", UR.resultToString r ]

        EnteredObjectiveDescription _ ->
            [ "EnteredObjectiveDescription" ]

        ClickedSaveObjective ->
            [ "ClickedSaveObjective" ]

        GotSaveObjectiveResponse r ->
            [ "GotSaveObjectiveResponse", UR.resultToString r ]

        CompletedLastObjectiveId r ->
            [ "CompletedLastObjectiveId", UR.resultToString r ]

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

        ClickedEditAction _ _ _ ->
            [ "ClickedEditAction" ]

        EnteredActionDescription _ ->
            [ "EnteredActionDescription" ]

        EnteredReward _ ->
            [ "EnteredReward" ]

        EnteredVerification _ ->
            [ "EnteredVerification" ]

        EnteredInvite _ ->
            [ "EnteredInvite" ]

        AddInvite ->
            [ "AddInvite" ]

        RemoveInvite _ ->
            [ "RemoveInvite" ]

        EnteredVerificationReward _ ->
            [ "EnteredVerificationReward" ]

        ClickedNewAction _ ->
            [ "ClickedNewAction" ]

        ClickedSaveAction ->
            [ "ClickedSaveAction" ]

        GotSaveActionResponse r ->
            [ "SavedAction", UR.resultToString r ]
