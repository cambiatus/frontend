module Page.Community exposing
    ( Model
    , Msg
    , init
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Action exposing (Action)
import Avatar
import Cambiatus.Enum.VerificationType as VerificationType
import Community
import Community.News
import Eos
import Html exposing (Html, a, button, div, h2, img, p, span, text)
import Html.Attributes exposing (class, classList, disabled, id, src, style)
import Html.Events exposing (onClick)
import Http
import Icons
import List.Extra
import Log
import Maybe.Extra
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared, Translators)
import Time exposing (Posix, posixToMillis)
import Token
import UpdateResult as UR
import Utils
import View.Components
import View.MarkdownEditor



-- INIT


init : LoggedIn.Model -> UpdateResult
init loggedIn =
    initModel loggedIn
        |> UR.init
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ObjectivesField)
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.ContributionsField)
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.NewsField)


initModel : LoggedIn.Model -> Model
initModel _ =
    { openObjectiveId = Nothing
    , tokenInfo = RemoteData.Loading
    }



-- MODEL


type alias Model =
    { openObjectiveId : Maybe Int
    , tokenInfo : RemoteData Http.Error Token.Model
    }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        t =
            loggedIn.shared.translators.t

        title =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    community.name

                RemoteData.Loading ->
                    t ""

                RemoteData.NotAsked ->
                    t ""

                _ ->
                    t "community.not_found"

        objectivesContainer children =
            div [ class "px-4" ]
                [ div [ class "container bg-white py-6 sm:py-8 px-3 sm:px-6 rounded mt-4" ]
                    children
                ]

        viewLoading =
            objectivesContainer
                [ View.Components.loadingLogoAnimated loggedIn.shared.translators ""
                ]

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure e ->
                    Page.fullPageGraphQLError (t "community.objectives.title") e

                RemoteData.Success community ->
                    let
                        showSponsorCard =
                            community.contributionConfiguration
                                |> Maybe.andThen .paypalAccount
                                |> Maybe.Extra.isJust
                    in
                    div []
                        [ Page.viewHeader loggedIn community.name
                        , div [ class "bg-white p-4" ]
                            [ div [ class "container mx-auto px-4" ]
                                [ div [ class "h-24 w-24 rounded-full mx-auto" ]
                                    [ img [ src community.logo, class "max-h-full m-auto object-scale-down" ] []
                                    ]
                                , div [ class "flex flex-wrap w-full items-center" ]
                                    [ p [ class "text-4xl font-bold" ]
                                        [ text community.name ]
                                    ]
                                , View.MarkdownEditor.viewReadOnly [ class "text-sm" ]
                                    community.description
                                ]
                            ]
                        , div [ class "container mx-auto" ]
                            [ if community.hasObjectives then
                                case community.objectives of
                                    RemoteData.Success objectives ->
                                        objectivesContainer
                                            (Page.viewTitle (t "community.objectives.title_plural")
                                                :: List.indexedMap (viewObjective loggedIn model community)
                                                    objectives
                                            )

                                    RemoteData.Loading ->
                                        viewLoading

                                    RemoteData.NotAsked ->
                                        viewLoading

                                    RemoteData.Failure err ->
                                        objectivesContainer
                                            [ div [ class "w-full" ]
                                                [ p [ class "text-2xl font-bold text-center" ] [ text (t "community.objectives.error_loading") ]
                                                , p [ class "text-center" ] [ text (Utils.errorToString err) ]
                                                ]
                                            , img [ class "w-1/3 mx-auto", src "/images/error.svg" ] []
                                            ]

                              else
                                text ""
                            , if showSponsorCard || community.hasNews then
                                div
                                    [ class "container mx-auto px-4 my-4 flex flex-col md:grid gap-4"
                                    , classList [ ( "md:grid-cols-2", showSponsorCard && community.hasNews ) ]
                                    ]
                                    [ if showSponsorCard then
                                        viewSponsorCard loggedIn community

                                      else
                                        text ""
                                    , if community.hasNews then
                                        viewNewsCard loggedIn community showSponsorCard

                                      else
                                        text ""
                                    ]

                              else
                                text ""
                            , viewCommunityStats loggedIn.shared.translators community model
                            ]
                        ]
    in
    { title = title
    , content =
        div
            -- id is used to scroll into view with a port
            [ id "communityPage" ]
            [ content ]
    }


viewSponsorCard : LoggedIn.Model -> Community.Model -> Html msg
viewSponsorCard loggedIn community =
    let
        { t, tr } =
            loggedIn.shared.translators

        text_ =
            text << t

        hasContributed =
            case loggedIn.contributionCount of
                RemoteData.Success contributionCount ->
                    contributionCount > 0

                _ ->
                    False

        compareAvatars first second =
            case ( Avatar.toMaybeString first, Avatar.toMaybeString second ) of
                ( Just _, Just _ ) ->
                    EQ

                ( Just _, Nothing ) ->
                    LT

                ( Nothing, Just _ ) ->
                    GT

                ( Nothing, Nothing ) ->
                    EQ

        viewLoading =
            List.range 0 5
                |> List.map
                    (\index ->
                        div
                            [ class "w-14 h-14 rounded-full -mr-2 border border-white bg-gray-300 animate-skeleton-loading"
                            , style "animation-delay" (String.fromInt (index * 100) ++ "ms")
                            ]
                            []
                    )
    in
    div [ class "w-full bg-white rounded p-4" ]
        [ h2 [ class "text-lg font-bold mb-6" ]
            [ span [ class "text-gray-900" ] [ text_ "community.index.our_supporters" ]
            , text " "
            , span [ class "text-purple-500" ] [ text_ "community.index.supporters" ]
            ]
        , if not hasContributed then
            div [ class "flex items-center mb-4" ]
                [ div [ class "uppercase bg-gray-400 text-white w-14 h-14 rounded-full flex items-center justify-center flex-shrink-0" ]
                    [ text_ "community.index.you" ]
                , p [ class "ml-4" ]
                    [ text <|
                        tr "community.index.your_turn"
                            [ ( "community", community.name ) ]
                    ]
                ]

          else
            text ""
        , a
            [ class "button button-primary w-full mb-6"
            , Route.href Route.CommunitySponsor
            ]
            [ text_ "community.index.support_us" ]
        , p [ class "mb-4" ] [ text_ "community.index.see_supporters" ]
        , div [ class "flex mb-4" ]
            (case community.contributions of
                RemoteData.Success contributions ->
                    contributions
                        |> List.map (.user >> .avatar)
                        |> List.Extra.unique
                        |> List.sortWith compareAvatars
                        |> List.take 5
                        |> List.map (\avatar -> Avatar.view avatar "w-14 h-14 object-cover rounded-full -mr-2 border border-white")

                RemoteData.Loading ->
                    viewLoading

                RemoteData.NotAsked ->
                    viewLoading

                RemoteData.Failure _ ->
                    []
            )
        , a
            [ class "button button-secondary w-full"
            , Route.href Route.CommunitySupporters
            ]
            [ text_ "community.index.see_all_supporters" ]
        ]


viewNewsCard : LoggedIn.Model -> Community.Model -> Bool -> Html Msg
viewNewsCard loggedIn community isSponsorCardVisible =
    let
        text_ =
            loggedIn.shared.translators.t >> text
    in
    case community.news of
        RemoteData.Success news ->
            if List.isEmpty news then
                viewNewsContainer loggedIn.shared.translators
                    isSponsorCardVisible
                    [ p [ class "text-lg font-bold text-gray-900 m-auto mt-4 mb-14" ]
                        [ text_ "menu.coming_soon" ]
                    , img
                        [ class "mx-auto -mb-4"
                        , src "/images/woman_announcer.svg"
                        ]
                        []
                    ]

            else
                div [ class "w-full flex flex-col relative rounded overflow-hidden" ]
                    [ viewNewsContainer loggedIn.shared.translators
                        isSponsorCardVisible
                        [ news
                            |> List.filter (Community.News.isPublished loggedIn.shared.now)
                            |> Community.News.viewList loggedIn.shared
                                [ class "hidden md:block" ]
                        , news
                            |> List.filter (Community.News.isPublished loggedIn.shared.now)
                            |> List.take 2
                            |> Community.News.viewList loggedIn.shared
                                [ class "md:hidden" ]
                        , span [ class "pt-4 mt-4 border-t border-gray-500 md:mb-32" ]
                            [ a
                                [ class "text-orange-300 hover:underline focus:underline focus:outline-none"
                                , Route.href (Route.News Nothing)
                                ]
                                [ text_ "news.view_more" ]
                            ]
                        , img
                            [ class "mx-auto -mb-4 md:hidden"
                            , src "/images/woman_announcer.svg"
                            ]
                            []
                        ]
                    , img
                        [ class "absolute bottom-0 -right-8 hidden pointer-events-none md:block"
                        , src "/images/woman_announcer.svg"
                        ]
                        []
                    ]

        RemoteData.Loading ->
            viewNewsContainer loggedIn.shared.translators
                isSponsorCardVisible
                [ View.Components.loadingLogoAnimated loggedIn.shared.translators
                    ""
                ]

        RemoteData.NotAsked ->
            viewNewsContainer loggedIn.shared.translators
                isSponsorCardVisible
                [ View.Components.loadingLogoAnimated loggedIn.shared.translators
                    ""
                ]

        RemoteData.Failure _ ->
            viewNewsContainer loggedIn.shared.translators
                isSponsorCardVisible
                [ p [ class "text-lg font-bold text-gray-900" ]
                    [ text_ "news.error_fetching" ]
                ]


viewNewsContainer : Translators -> Bool -> List (Html msg) -> Html msg
viewNewsContainer { t } isSponsorCardVisible children =
    div
        [ class "flex flex-col w-full bg-white rounded p-4 relative md:overflow-y-auto"
        , classList [ ( "md:flex-basis-0 md:flex-grow-1", isSponsorCardVisible ) ]
        ]
        (h2 [ class "text-lg font-bold mb-6" ]
            [ span [ class "text-gray-900" ] [ text <| t "community.index.our_messages" ]
            , text " "
            , span [ class "text-purple-500" ] [ text <| t "community.index.messages" ]
            ]
            :: children
        )


viewCommunityStats : Translators -> Community.Model -> Model -> Html msg
viewCommunityStats ({ t, tr } as translators) community model =
    let
        card attrs =
            div (class "bg-white rounded p-4" :: attrs)

        hasTokenInfo =
            RemoteData.isSuccess model.tokenInfo
    in
    div
        [ class "container mx-auto px-4 my-4 grid grid-cols-2 grid-rows-6 gap-4 grid-flow-row-dense md:grid-cols-4 md:grid-rows-3 md:mb-10"
        , classList [ ( "grid-rows-4", not hasTokenInfo ) ]
        ]
        [ case model.tokenInfo of
            RemoteData.Success { supply } ->
                card [ class "col-span-2 flex items-center bg-green text-white" ]
                    [ Icons.coin "mr-6"
                    , div []
                        [ p [ class "font-bold text-3xl" ]
                            [ text (Eos.formatSymbolAmount translators supply.symbol supply.amount) ]
                        , p [ class "text-sm" ]
                            [ text
                                (tr "community.index.amount_in_circulation"
                                    [ ( "currency_name", Eos.symbolToSymbolCodeString community.symbol ) ]
                                )
                            ]
                        ]
                    ]

            _ ->
                text ""
        , case model.tokenInfo of
            RemoteData.Success { minBalance } ->
                card [ class "col-span-2 flex items-center" ]
                    [ Icons.coin "mr-6"
                    , div []
                        [ p [ class "font-bold text-3xl text-green" ]
                            [ text (Eos.formatSymbolAmount translators minBalance.symbol minBalance.amount) ]
                        , p [ class "text-sm text-gray-900" ]
                            [ text (t "community.index.minimum_balance") ]
                        ]
                    ]

            _ ->
                text ""
        , card [ class "row-span-2 relative overflow-hidden" ]
            [ p [ class "w-full font-bold text-green text-3xl" ]
                [ text <| String.fromInt community.memberCount ]
            , p [ class "text-gray-900 text-sm" ]
                [ text <| t "community.index.members" ]
            , img [ class "absolute bottom-0 right-0", src "/images/girl-playing-guitar.svg" ] []
            ]
        , card [ class "grid grid-cols-3 lg:grid-cols-5 xl:grid-cols-6 items-center" ]
            [ div [ class "col-span-2 lg:col-span-4 xl:col-span-5" ]
                [ p [ class "w-full font-bold text-green text-3xl" ]
                    [ text <| String.fromInt community.claimCount ]
                , p [ class "text-gray-900 text-sm" ]
                    [ text <| t "community.index.claims" ]
                ]
            , Icons.flag "fill-current text-orange-500 w-full md:w-2/3 md:mx-auto"
            ]
        , card [ class "col-span-2 row-span-2 relative overflow-hidden flex flex-col justify-center" ]
            [ p [ class "w-full font-bold text-green text-3xl" ]
                [ text <| String.fromInt community.productCount ]
            , p [ class " text-gray-900 text-sm" ]
                [ text <| t "community.index.products" ]
            , p
                [ class "w-full font-bold text-green text-3xl mt-4" ]
                [ text <| String.fromInt community.orderCount ]
            , p [ class " text-gray-900 text-sm" ]
                [ text <| t "community.index.orders" ]
            , img [ class "absolute right-0 h-full pt-10 md:pt-6", src "/images/booth.svg" ] []
            ]
        , card [ class "grid grid-cols-3 lg:grid-cols-5 xl:grid-cols-6 items-center" ]
            [ div [ class "col-span-2 lg:col-span-4 xl:col-span-5" ]
                [ p [ class "w-full font-bold text-green text-3xl" ]
                    [ text <| String.fromInt community.transferCount ]
                , p [ class " text-gray-900 text-sm" ]
                    [ text <| t "community.index.transfers" ]
                ]
            , Icons.arrowsLeft "w-full"
            ]
        ]



-- VIEW OBJECTIVE


viewObjective : LoggedIn.Model -> Model -> Community.Model -> Int -> Community.Objective -> Html Msg
viewObjective loggedIn model metadata index objective =
    let
        canEdit =
            LoggedIn.isAccount metadata.creator loggedIn

        isOpen : Bool
        isOpen =
            case model.openObjectiveId of
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

        isDisabled =
            case loggedIn.claimingAction.status of
                Action.ClaimInProgress _ _ ->
                    True

                _ ->
                    False

        actsNButton : List (Html Msg)
        actsNButton =
            objective.actions
                |> List.sortBy (\a -> a.position |> Maybe.withDefault 0)
                |> List.map
                    (\action ->
                        viewAction loggedIn.shared
                            (LoggedIn.isAccount metadata.creator loggedIn)
                            loggedIn.shared.now
                            action
                            isDisabled
                    )
    in
    if objective.isCompleted then
        text ""

    else
        div [ class "my-2" ]
            [ div
                [ class "px-3 bg-indigo-500 flex flex-col py-4 cursor-pointer sm:flex-row sm:items-center"
                , if isOpen then
                    onClick ClickedCloseObjective

                  else
                    onClick (ClickedOpenObjective index)
                ]
                [ div [ class "sm:flex-grow-7 sm:w-5/12" ]
                    [ View.MarkdownEditor.viewReadOnly
                        [ class "text-white font-semibold"
                        , classList [ ( "truncate-children", not isOpen ) ]
                        ]
                        objective.description
                    ]
                , div
                    [ class ("flex flex-row justify-end" ++ arrowStyle)
                    , classList [ ( "mt-1 sm:mt-0 pb-4 sm:pb-0", isOpen ), ( "mt-5 sm:mt-0", not isOpen ) ]
                    ]
                    [ button
                        [ class ""
                        , if isOpen then
                            onClick ClickedCloseObjective

                          else
                            onClick (ClickedOpenObjective index)
                        ]
                        [ img
                            [ class "fill-current text-white h-2 w-4 stroke-current"
                            , src "/icons/objective_arrow.svg"
                            , classList [ ( "rotate-180", isOpen ) ]
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



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | GotTokenInfo (Result Http.Error Token.Model)
      -- Objective
    | ClickedOpenObjective Int
    | ClickedCloseObjective
    | GotActionMsg Action.Msg


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            UR.init model
                |> UR.addCmd (Token.getToken loggedIn.shared community.symbol GotTokenInfo)

        GotTokenInfo (Ok tokenInfo) ->
            { model | tokenInfo = RemoteData.Success tokenInfo }
                |> UR.init

        GotTokenInfo (Err err) ->
            { model | tokenInfo = RemoteData.Failure err }
                |> UR.init
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when retrieving token info"
                    { moduleName = "Page.Community", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        GotActionMsg (Action.ClaimButtonClicked action) ->
            model
                |> UR.init
                |> UR.addExt
                    (UpdatedLoggedIn
                        { loggedIn
                            | claimingAction =
                                { status = Action.ConfirmationOpen action
                                , feedback = Nothing
                                , needsPinConfirmation = False
                                }
                        }
                    )

        GotActionMsg _ ->
            model
                |> UR.init

        ClickedOpenObjective index ->
            { model | openObjectiveId = Just index }
                |> UR.init

        ClickedCloseObjective ->
            { model | openObjectiveId = Nothing }
                |> UR.init


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        GotTokenInfo r ->
            [ "GotTokenInfo", UR.resultToString r ]

        GotActionMsg _ ->
            [ "GotActionMsg" ]

        ClickedOpenObjective _ ->
            [ "ClickedOpenObjective" ]

        ClickedCloseObjective ->
            [ "ClickedCloseObjective" ]


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


viewAction : Shared -> Bool -> Posix -> Action -> Bool -> Html Msg
viewAction shared canEdit date action isDisabled =
    let
        { t, tr } =
            shared.translators

        text_ s =
            text (t s)

        posixDeadline : Posix
        posixDeadline =
            action.deadline
                |> Utils.fromMaybeDateTime

        pastDeadline : Bool
        pastDeadline =
            case action.deadline of
                Just _ ->
                    posixToMillis date > posixToMillis posixDeadline

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
            if pastDeadline then
                " text-red"

            else
                " text-indigo-500"

        usagesColor : String
        usagesColor =
            if action.usagesLeft >= 1 || action.usages == 0 then
                " text-indigo-500"

            else
                " text-red"

        ( claimColors, claimText ) =
            if pastDeadline || (action.usagesLeft < 1 && action.usages > 0) then
                ( " button-disabled", "dashboard.closed" )

            else
                ( " button button-primary", "dashboard.claim" )

        claimSize =
            if canEdit then
                " w-4/5"

            else
                " w-1/2"

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
                            |> Avatar.view v.avatar
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
                                        [ p [ class "m-auto text-sm font-black" ]
                                            [ text ("+" ++ String.fromInt (numValidators - 3)) ]
                                        ]
                                   ]

                        else
                            vals
                   )

        rewardStr =
            String.fromFloat action.reward ++ " " ++ Eos.symbolToSymbolCodeString action.objective.community.symbol

        ( usages, usagesLeft ) =
            ( String.fromInt action.usages, String.fromInt action.usagesLeft )

        isClosed =
            pastDeadline
                || (action.usages > 0 && action.usagesLeft == 0)

        viewClaimButton =
            button
                [ class ("h-10 uppercase rounded-lg ml-1" ++ claimColors ++ claimSize)
                , onClick
                    (if isClosed then
                        NoOp

                     else
                        (GotActionMsg << Action.ClaimButtonClicked) action
                    )
                , disabled isDisabled
                , classList [ ( "button-disabled", isDisabled ) ]
                ]
                [ if action.hasProofPhoto then
                    span [ class "inline-block w-4 align-middle mr-2" ] [ Icons.camera "" ]

                  else
                    text ""
                , span [ class "inline-block align-middle" ] [ text_ claimText ]
                ]
    in
    if action.isCompleted then
        text ""

    else
        div [ class "py-6 px-2" ]
            [ div [ class "flex flex-col border-l-8 border-light-grey rounded-l-sm pl-2 sm:pl-6" ]
                [ View.MarkdownEditor.viewReadOnly [ class "text-text-grey text-sm sm:text-base" ]
                    action.description
                , div [ class "flex flex-col sm:flex-row sm:items-center sm:justify-between" ]
                    [ div [ class "text-sm mt-5 sm:w-1/3" ]
                        [ case action.deadline of
                            Just _ ->
                                div []
                                    [ span [ class "capitalize text-text-grey" ] [ text_ "community.actions.available_until" ]
                                    , View.Components.dateViewer [ class ("capitalize " ++ dateColor) ]
                                        identity
                                        shared
                                        posixDeadline
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
                            (case action.verificationType of
                                VerificationType.Claimable ->
                                    validatorAvatars

                                VerificationType.Automatic ->
                                    [ span [ class "uppercase text-sm mr-1" ]
                                        [ text_ "community.actions.automatic_analyzers" ]
                                    , img [ src "/icons/tooltip.svg" ] []
                                    ]
                            )
                        , case action.verificationType of
                            VerificationType.Claimable ->
                                div [ class "capitalize text-text-grey text-sm sm:text-right" ]
                                    [ text_ "community.actions.verifiers" ]

                            VerificationType.Automatic ->
                                text ""
                        ]
                    ]
                , div [ class "mt-5 flex flex-row items-baseline" ]
                    [ div [ class ("text-green mt-5 flex-grow-1" ++ rewardStrike) ]
                        [ span [] [ text (t "community.actions.reward" ++ ": ") ]
                        , span [ class "font-semibold" ] [ text rewardStr ]
                        ]
                    , div [ class "hidden sm:flex sm:visible flex-row justify-end flex-grow-1" ]
                        (case action.verificationType of
                            VerificationType.Claimable ->
                                [ viewClaimButton ]

                            VerificationType.Automatic ->
                                []
                        )
                    ]
                ]
            , div [ class "flex flex-row mt-8 justify-between sm:hidden" ]
                (case action.verificationType of
                    VerificationType.Claimable ->
                        [ viewClaimButton ]

                    VerificationType.Automatic ->
                        []
                )
            ]
