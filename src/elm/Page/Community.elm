module Page.Community exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , subscriptions
    , update
    , view
    )

import Action
import Api
import Api.Graphql
import Avatar
import Cambiatus.Enum.VerificationType as VerificationType
import Claim
import Community exposing (Action, Model)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Html exposing (Html, button, div, img, input, label, p, span, text)
import Html.Attributes exposing (accept, class, classList, disabled, id, multiple, src, style, type_)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Session.Shared exposing (Shared, Translators)
import Strftime
import Task
import Time exposing (Posix, posixToMillis)
import UpdateResult as UR
import Utils
import View.Modal as Modal



-- INIT


init : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) symbol =
    ( initModel loggedIn symbol
    , Cmd.batch
        [ Api.Graphql.query shared (Community.communityQuery symbol) CompletedLoadCommunity
        , Task.perform GotTime Time.now
        ]
    )


initModel : LoggedIn.Model -> Symbol -> Model
initModel _ _ =
    { date = Nothing
    , pageStatus = Loading
    , actionId = Nothing
    , openObjective = Nothing
    , claimingAction = Nothing

    --, claimConfirmationModalStatus = Closed
    --, proof = Nothing
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.claimingAction of
        Just ca ->
            Sub.map GotActionMsg (Action.subscriptions ca)

        Nothing ->
            Sub.none



-- MODEL


type alias Model =
    { date : Maybe Posix
    , claimingAction : Maybe Action.Model
    , pageStatus : PageStatus
    , actionId : Maybe Int
    , openObjective : Maybe Int

    --, claimConfirmationModalStatus : ClaimConfirmationModalStatus
    --, proof : Maybe Proof
    }


type PageStatus
    = Loading
    | Loaded Community.Model ActiveSection
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Community.Model))


type ActiveSection
    = ObjectivesAndActions
    | ClaimWithProofs Community.Action



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        t =
            loggedIn.shared.translators.t

        text_ s =
            text (t s)

        title =
            case model.pageStatus of
                Loaded community _ ->
                    community.title

                Loading ->
                    t ""

                _ ->
                    t "community.not_found"

        content =
            case model.pageStatus of
                Loading ->
                    Page.fullPageLoading loggedIn.shared

                NotFound ->
                    Page.viewCardEmpty [ text_ "community.not_found" ]

                Failed e ->
                    Page.fullPageGraphQLError (t "community.objectives.title") e

                Loaded community pageStatus ->
                    case pageStatus of
                        ObjectivesAndActions ->
                            div []
                                [ Page.viewHeader loggedIn community.title Route.Dashboard
                                , div [ class "bg-white p-4" ]
                                    [ div [ class "container mx-auto px-4" ]
                                        [ div [ class "h-24 w-24 rounded-full mx-auto" ]
                                            [ img [ src community.logo, class "max-h-full m-auto object-scale-down" ] []
                                            ]
                                        , div [ class "flex flex-wrap w-full items-center" ]
                                            [ p [ class "text-4xl font-bold" ]
                                                [ text community.title ]
                                            ]
                                        , p [ class "text-grey-200 text-sm" ] [ text community.description ]
                                        ]
                                    ]
                                , div [ class "container mx-auto" ]
                                    [ if community.hasObjectives then
                                        div [ class "px-4 pb-4" ]
                                            [ Html.map GotActionMsg
                                                (case model.claimingAction of
                                                    Just ca ->
                                                        Action.viewClaimConfirmation
                                                            loggedIn.shared.translators
                                                            ca.claimConfirmationModalStatus

                                                    Nothing ->
                                                        text ""
                                                )
                                            , div [ class "container bg-white py-6 sm:py-8 px-3 sm:px-6 rounded-lg mt-4" ]
                                                (Page.viewTitle (t "community.objectives.title_plural")
                                                    :: List.indexedMap (viewObjective loggedIn model community)
                                                        community.objectives
                                                )
                                            ]

                                      else
                                        text ""
                                    , viewCommunityStats loggedIn.shared.translators community
                                    ]
                                ]

                        ClaimWithProofs action ->
                            case model.claimingAction of
                                Just ca ->
                                    Html.map GotActionMsg
                                        (Action.viewClaimWithProofs ca.proof loggedIn.shared.translators action)

                                Nothing ->
                                    text ""
    in
    { title = title
    , content =
        div
            -- id is used to scroll into view with a port
            [ id "communityPage" ]
            [ content ]
    }


viewCommunityStats : Translators -> Community.Model -> Html msg
viewCommunityStats { t } community =
    div [ class "flex flex-wrap px-4 container mb-6" ]
        [ div [ class "flex w-full lg:w-1/2 h-48 mb-4" ]
            [ div [ class "flex-grow" ]
                [ div
                    [ class " min-w-40 h-48 relative bg-white rounded-lg p-4 overflow-hidden" ]
                    [ p [ class "w-full font-bold text-green text-3xl" ]
                        [ text <| String.fromInt community.memberCount ]
                    , p [ class " text-gray-700 text-sm" ]
                        [ text <| t "community.index.members" ]
                    , img [ class "absolute bottom-0 right-0", src "/images/girl-playing-guitar.svg" ] []
                    ]
                ]
            , div [ class "px-2 mb-6" ]
                [ div [ class "flex flex-col w-40" ]
                    [ div [ class "w-40 h-24 bg-white rounded-lg px-4 py-2 mb-4" ]
                        [ p [ class "w-full font-bold text-green text-3xl" ]
                            [ text <| String.fromInt community.claimCount ]
                        , p [ class " text-gray-700 text-sm" ]
                            [ text <| t "community.index.claims" ]
                        ]
                    , div [ class "w-40 h-20 bg-white rounded-lg px-4 py-2" ]
                        [ p [ class "w-full font-bold text-green text-3xl" ]
                            [ text <| String.fromInt community.transferCount ]
                        , p [ class " text-gray-700 text-sm" ]
                            [ text <| t "community.index.transfers" ]
                        ]
                    ]
                ]
            ]
        , div [ class "w-full lg:w-1/2 h-48" ]
            [ div [ class "" ]
                [ div [ class "w-full relative bg-white rounded-lg p-4 h-48 overflow-hidden" ]
                    [ p [ class "w-full font-bold text-green text-3xl" ]
                        [ text <| String.fromInt community.productCount ]
                    , p [ class " text-gray-700 text-sm" ]
                        [ text <| t "community.index.products" ]
                    , p
                        [ class "w-full font-bold text-green text-3xl mt-4" ]
                        [ text <| String.fromInt community.orderCount ]
                    , p [ class " text-gray-700 text-sm" ]
                        [ text <| t "community.index.orders" ]
                    , img [ class "absolute right-0 bottom-0", src "/images/booth.svg" ] []
                    ]
                ]
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
            objective.actions
                |> List.sortBy (\a -> a.position |> Maybe.withDefault 0)
                |> List.map
                    (\action ->
                        Html.map GotActionMsg
                            (Action.viewAction loggedIn.shared.translators
                                (LoggedIn.isAccount metadata.creator loggedIn)
                                metadata.symbol
                                model.date
                                action
                            )
                    )
    in
    if objective.isCompleted then
        text ""

    else
        div [ class "my-2" ]
            [ div
                [ class "px-3 py-4 bg-indigo-500 flex flex-col sm:flex-row sm:items-center sm:h-10 cursor-pointer"
                , if isOpen then
                    onClick ClickedCloseObjective

                  else
                    onClick (ClickedOpenObjective index)
                ]
                [ div [ class "sm:flex-grow-7 sm:w-5/12" ]
                    [ div
                        [ class "truncate overflow-hidden whitespace-no-wrap text-white font-medium text-sm overflow-hidden sm:flex-grow-8 sm:leading-normal sm:text-lg"
                        ]
                        [ text objective.description ]
                    ]
                , div [ class ("flex flex-row justify-end mt-5 sm:mt-0" ++ arrowStyle) ]
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
    | GotTime Posix
    | CompletedLoadCommunity (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
      -- Objective
    | ClickedOpenObjective Int
    | ClickedCloseObjective
    | GotActionMsg Action.Msg


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared } as loggedIn) =
    let
        t =
            shared.translators.t
    in
    case msg of
        NoOp ->
            UR.init model

        GotTime date ->
            UR.init { model | date = Just date }

        GotActionMsg actionMsg ->
            let
                updateClaimingAction : Action.Model -> UR.UpdateResult Model Msg extMsg
                updateClaimingAction actionModel =
                    Action.update loggedIn actionMsg actionModel
                        |> UR.map
                            (\a -> { model | claimingAction = Just a })
                            GotActionMsg
                            (\extMsg uResult -> uResult)
            in
            case actionMsg of
                Action.OpenClaimConfirmation action ->
                    updateClaimingAction (Action.init action)

                _ ->
                    case model.claimingAction of
                        Just ca ->
                            updateClaimingAction ca

                        Nothing ->
                            model
                                |> UR.init

        CompletedLoadCommunity (Ok community) ->
            case community of
                Just c ->
                    { model
                        | pageStatus = Loaded c ObjectivesAndActions
                    }
                        |> UR.init

                Nothing ->
                    { model | pageStatus = NotFound }
                        |> UR.init

        CompletedLoadCommunity (Err err) ->
            { model | pageStatus = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        ClickedOpenObjective index ->
            { model | openObjective = Just index }
                |> UR.init

        ClickedCloseObjective ->
            { model | openObjective = Nothing }
                |> UR.init


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    --case addr of
    --    "ClaimAction" :: [] ->
    --        Decode.decodeValue
    --            (Decode.oneOf
    --                [ Decode.field "transactionId" Decode.string |> Decode.map Ok
    --                , Decode.succeed (Err val)
    --                ]
    --            )
    --            val
    --            |> Result.map (Just << GotClaimActionResponse)
    --            |> Result.withDefault Nothing
    --
    --    "GetUint64Name" :: [] ->
    --        Decode.decodeValue
    --            (Decode.oneOf
    --                [ Decode.field "uint64name" Decode.string |> Decode.map Ok
    --                , Decode.succeed (Err val)
    --                ]
    --            )
    --            val
    --            |> Result.map (Just << GotUint64Name)
    --            |> Result.withDefault Nothing
    --
    --    _ ->
    --        Nothing
    Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        GotTime _ ->
            [ "GotTime" ]

        GotActionMsg _ ->
            [ "GotActionMsg" ]

        --Tick _ ->
        --    [ "Tick" ]
        --
        --GotProofTime _ _ ->
        --    [ "GotProofTime" ]
        CompletedLoadCommunity r ->
            [ "CompletedLoadCommunity", UR.resultToString r ]

        ClickedOpenObjective _ ->
            [ "ClickedOpenObjective" ]

        ClickedCloseObjective ->
            [ "ClickedCloseObjective" ]



--OpenProofSection _ ->
--    [ "OpenAddPhotoProof" ]
--
--CloseProofSection _ ->
--    [ "CloseAddPhotoProof" ]
--
--EnteredPhoto _ ->
--    [ "EnteredPhoto" ]
--
--CompletedPhotoUpload r ->
--    [ "CompletedPhotoUpload", UR.resultToString r ]
--
--OpenClaimConfirmation _ ->
--    [ "OpenClaimConfirmation" ]
--
--CloseClaimConfirmation ->
--    [ "CloseClaimConfirmation" ]
--
--ClaimAction _ ->
--    [ "ClaimAction" ]
--
--GetUint64Name _ ->
--    [ "GetUint64Name" ]
--
--GotUint64Name n ->
--    [ "GotClaimActionResponse", UR.resultToString n ]
--
--GotClaimActionResponse r ->
--    [ "GotClaimActionResponse", UR.resultToString r ]
