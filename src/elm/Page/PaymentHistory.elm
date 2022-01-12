module Page.PaymentHistory exposing
    ( Model
    , Msg
    , init
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api.Graphql
import Api.Relay
import Avatar exposing (Avatar)
import Browser.Dom
import Cambiatus.Enum.TransferDirectionValue as TransferDirectionValue
import Cambiatus.Object
import Cambiatus.Object.User as User
import Cambiatus.Query
import Cambiatus.Scalar
import Community
import Date exposing (Date)
import DatePicker exposing (DateEvent(..), defaultSettings, off)
import Emoji
import Eos
import Eos.Account
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, h1, h2, img, p, text, ul)
import Html.Attributes as Attrs exposing (class, src, style, tabindex)
import Html.Events exposing (onClick)
import Icons
import Log
import Markdown exposing (Markdown)
import Page
import Profile
import Profile.Contact
import Profile.Summary
import RemoteData exposing (RemoteData)
import Select
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import Strftime
import Task
import Time exposing (Weekday(..))
import Transfer exposing (ConnectionTransfer, Transfer)
import UpdateResult as UR
import Utils
import View.Components



-- MSG


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | RecipientProfileWithTransfersLoaded (RemoteData (Graphql.Http.Error (Maybe ProfileWithTransfers)) (Maybe ProfileWithTransfers))
    | AutocompleteProfilesLoaded (RemoteData (Graphql.Http.Error (Maybe ProfileWithOnlyAutocomplete)) (Maybe ProfileWithOnlyAutocomplete))
    | OnSelect ProfileBase
    | SelectMsg (Select.Msg ProfileBase)
    | ClearSelect
    | SetDatePicker DatePicker.Msg
    | ClickedCalendar
    | ClearDatePicker
    | ShowMore
    | GotPayerProfileSummaryMsg Profile.Summary.Msg


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        ShowMore ->
            [ "ShowMore" ]

        RecipientProfileWithTransfersLoaded r ->
            [ "RecipientProfileWithTransfersLoaded", UR.remoteDataToString r ]

        AutocompleteProfilesLoaded r ->
            [ "AutocompleteProfilesLoaded", UR.remoteDataToString r ]

        ClearSelect ->
            [ "ClearSelect" ]

        OnSelect _ ->
            [ "OnSelect" ]

        SelectMsg _ ->
            [ "SelectMsg" ]

        SetDatePicker _ ->
            [ "SetDatePicker" ]

        ClickedCalendar ->
            [ "ClickedCalendar" ]

        ClearDatePicker ->
            [ "ClearDatePicker" ]

        GotPayerProfileSummaryMsg subMsg ->
            "GotPayerProfileSummaryMsg" :: Profile.Summary.msgToString subMsg



-- MODEL


type alias Model =
    { queryStatus : QueryStatus (Maybe ProfileWithTransfers) ProfileWithTransfers
    , recipientProfile : ProfileBase
    , incomingTransfers : Maybe (List Transfer)
    , incomingTransfersPageInfo : Maybe Api.Relay.PageInfo
    , autocompleteProfiles : List ProfileBase
    , autocompleteState : Select.State
    , autocompleteSelectedProfile : Maybe ProfileBase
    , datePicker : DatePicker.DatePicker
    , selectedDate : Maybe Date
    , payerProfileSummary : Profile.Summary.Model
    }


type QueryStatus err resp
    = Loading
    | Loaded resp
    | Failed (Graphql.Http.Error err)


type alias ProfileBase =
    { name : Maybe String
    , account : Eos.Account.Name
    , avatar : Avatar
    , email : Maybe String
    , bio : Maybe Markdown
    , contacts : List Profile.Contact.Normalized
    }


type alias ProfileWithTransfers =
    { name : Maybe String
    , account : Eos.Account.Name
    , avatar : Avatar
    , email : Maybe String
    , bio : Maybe Markdown
    , contacts : List Profile.Contact.Normalized
    , transfers : Maybe ConnectionTransfer
    }


type alias ProfileWithOnlyAutocomplete =
    { getPayersByAccount : Maybe (List (Maybe ProfileBase))
    }


profileWithTransfersSelectionSet : Community.Model -> Model -> SelectionSet ProfileWithTransfers Cambiatus.Object.User
profileWithTransfersSelectionSet community model =
    let
        endCursor =
            Maybe.andThen .endCursor model.incomingTransfersPageInfo

        afterOption =
            case endCursor of
                Just ec ->
                    Present ec

                Nothing ->
                    Absent

        optionalDate =
            case model.selectedDate of
                Just d ->
                    Present (Cambiatus.Scalar.Date (Date.toIsoString d))

                Nothing ->
                    Absent

        optionalFromAccount =
            case model.autocompleteSelectedProfile of
                Just p ->
                    Present (Eos.Account.nameToString p.account)

                Nothing ->
                    Absent

        optionalArgsFn =
            \r ->
                { r
                    | first = Present 16
                    , after = afterOption
                    , filter =
                        Present
                            { communityId = Present (Eos.symbolToString community.symbol)
                            , date = optionalDate
                            , direction =
                                Present
                                    { direction = Present TransferDirectionValue.Receiving
                                    , otherAccount = optionalFromAccount
                                    }
                            }
                }
    in
    SelectionSet.succeed ProfileWithTransfers
        |> SelectionSet.with User.name
        |> SelectionSet.with (Eos.Account.nameSelectionSet User.account)
        |> SelectionSet.with (Avatar.selectionSet User.avatar)
        |> SelectionSet.with User.email
        |> SelectionSet.with (Markdown.maybeSelectionSet User.bio)
        |> SelectionSet.with Profile.userContactSelectionSet
        |> SelectionSet.with
            (User.transfers
                optionalArgsFn
                Transfer.transferConnectionSelectionSet
            )


fetchProfileWithTransfers : Shared -> Community.Model -> Model -> String -> Cmd Msg
fetchProfileWithTransfers shared community model authToken =
    let
        accountName =
            Eos.Account.nameToString model.recipientProfile.account
    in
    Api.Graphql.query shared
        (Just authToken)
        (Cambiatus.Query.user
            { account = accountName }
            (profileWithTransfersSelectionSet community model)
        )
        RecipientProfileWithTransfersLoaded


fetchProfilesForAutocomplete : Shared -> Model -> String -> String -> Cmd Msg
fetchProfilesForAutocomplete shared model payerAccount authToken =
    let
        autocompleteSelectionSet : SelectionSet ProfileBase Cambiatus.Object.User
        autocompleteSelectionSet =
            SelectionSet.succeed ProfileBase
                |> SelectionSet.with User.name
                |> SelectionSet.with (Eos.Account.nameSelectionSet User.account)
                |> SelectionSet.with (Avatar.selectionSet User.avatar)
                |> SelectionSet.with User.email
                |> SelectionSet.with (Markdown.maybeSelectionSet User.bio)
                |> SelectionSet.with Profile.userContactSelectionSet

        selectionSet : SelectionSet ProfileWithOnlyAutocomplete Cambiatus.Object.User
        selectionSet =
            SelectionSet.map ProfileWithOnlyAutocomplete
                (User.getPayersByAccount { account = payerAccount } autocompleteSelectionSet)

        accountName =
            Eos.Account.nameToString model.recipientProfile.account
    in
    Api.Graphql.query shared
        (Just authToken)
        (Cambiatus.Query.user
            { account = accountName
            }
            selectionSet
        )
        AutocompleteProfilesLoaded


datePickerSettings : Shared -> DatePicker.Settings
datePickerSettings shared =
    { defaultSettings
        | changeYear = off
        , placeholder = shared.translators.t "payment_history.pick_date"
        , dateFormatter = Date.format "E, d MMM y"
        , firstDayOfWeek = Mon
        , inputId = Just "date-picker-input"
        , inputAttributes =
            [ Attrs.required False
            , Attrs.readonly True
            , class "input w-full"
            ]
    }


init : Eos.Account.Name -> LoggedIn.Model -> ( Model, Cmd Msg )
init recipientAccountName loggedIn =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init

        recipientProfile : Profile.Basic {}
        recipientProfile =
            { name = Nothing
            , account = recipientAccountName
            , avatar = Avatar.empty
            , email = Nothing
            , bio = Nothing
            , contacts = []
            }

        initModel =
            { queryStatus = Loading
            , recipientProfile = recipientProfile
            , incomingTransfers = Nothing
            , incomingTransfersPageInfo = Nothing
            , autocompleteProfiles = []
            , autocompleteState = Select.newState ""
            , autocompleteSelectedProfile = Nothing
            , datePicker = datePicker
            , selectedDate = Nothing
            , payerProfileSummary = Profile.Summary.init False
            }
    in
    ( initModel
    , Cmd.batch
        [ Cmd.map SetDatePicker datePickerCmd
        , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
        ]
    )


getTransfers : Maybe ConnectionTransfer -> List Transfer
getTransfers maybeObj =
    let
        toMaybeEdges : Maybe ConnectionTransfer -> Maybe (List (Maybe Transfer.EdgeTransfer))
        toMaybeEdges maybeConn =
            Maybe.andThen
                (\a -> a.edges)
                maybeConn

        toEdges : Maybe (List (Maybe Transfer.EdgeTransfer)) -> List (Maybe Transfer.EdgeTransfer)
        toEdges maybeEdges =
            Maybe.withDefault
                []
                maybeEdges

        toMaybeNodes : List (Maybe Transfer.EdgeTransfer) -> List (Maybe Transfer)
        toMaybeNodes edges =
            List.map
                (\a ->
                    Maybe.andThen
                        (\b ->
                            b.node
                        )
                        a
                )
                edges

        toNodes : List (Maybe Transfer) -> List Transfer
        toNodes maybeNodes =
            List.filterMap
                identity
                maybeNodes
    in
    maybeObj
        |> toMaybeEdges
        |> toEdges
        |> toMaybeNodes
        |> toNodes



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UR.UpdateResult Model Msg extMsg
update msg model ({ shared, authToken } as loggedIn) =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            model
                |> UR.init
                |> UR.addCmd (fetchProfileWithTransfers shared community model authToken)

        AutocompleteProfilesLoaded (RemoteData.Success maybeProfileWithPayers) ->
            case maybeProfileWithPayers of
                Just profileWithPayers ->
                    let
                        payers : List (Maybe ProfileBase)
                        payers =
                            Maybe.withDefault [] profileWithPayers.getPayersByAccount

                        toList : Maybe ProfileBase -> List ProfileBase
                        toList p =
                            case p of
                                Just val ->
                                    [ val ]

                                Nothing ->
                                    []

                        profiles : List ProfileBase
                        profiles =
                            payers
                                |> List.map toList
                                |> List.concat
                    in
                    { model | autocompleteProfiles = profiles }
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init

        AutocompleteProfilesLoaded (RemoteData.Failure err) ->
            model
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading profiles for auto complete"
                    { moduleName = "Page.PaymentHistory", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        AutocompleteProfilesLoaded _ ->
            UR.init model

        RecipientProfileWithTransfersLoaded (RemoteData.Success maybeProfile) ->
            case maybeProfile of
                Just profile ->
                    let
                        pageInfo =
                            Maybe.map .pageInfo profile.transfers

                        recipientProfile : ProfileBase
                        recipientProfile =
                            { name = profile.name
                            , account = profile.account
                            , avatar = profile.avatar
                            , email = profile.email
                            , bio = profile.bio
                            , contacts = profile.contacts
                            }

                        newIncomingTransfers =
                            case model.incomingTransfers of
                                Just transfers ->
                                    transfers ++ getTransfers profile.transfers

                                Nothing ->
                                    getTransfers profile.transfers

                        newModel =
                            { model
                                | queryStatus = Loaded profile
                                , recipientProfile = recipientProfile
                                , incomingTransfers = Just newIncomingTransfers
                                , incomingTransfersPageInfo = pageInfo
                            }
                    in
                    newModel
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init

        RecipientProfileWithTransfersLoaded (RemoteData.Failure err) ->
            { model | queryStatus = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading recipient profile with transfers"
                    { moduleName = "Page.PaymentHistory", function = "update" }
                    []
                    err

        RecipientProfileWithTransfersLoaded _ ->
            UR.init model

        ShowMore ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    model
                        |> UR.init
                        |> UR.addCmd (fetchProfileWithTransfers shared community model authToken)

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Clicked show more, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.PaymentHistory", function = "update" }
                            []

        OnSelect profile ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        newModel =
                            { model
                                | incomingTransfers = Nothing
                                , incomingTransfersPageInfo = Nothing
                                , autocompleteSelectedProfile = Just profile
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchProfileWithTransfers shared community newModel authToken)

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Selected user, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.PaymentHistory", function = "update" }
                            []

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration shared False) subMsg model.autocompleteState
            in
            case Select.queryFromState model.autocompleteState of
                Just payer ->
                    { model | autocompleteState = updated }
                        |> UR.init
                        |> UR.addCmd (fetchProfilesForAutocomplete shared model payer authToken)
                        |> UR.addCmd cmd

                Nothing ->
                    { model | autocompleteState = updated }
                        |> UR.init
                        |> UR.addCmd cmd

        ClearSelect ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        newModel =
                            { model
                                | incomingTransfers = Nothing
                                , incomingTransfersPageInfo = Nothing
                                , autocompleteSelectedProfile = Nothing
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchProfileWithTransfers shared community newModel authToken)

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Cleared selected user, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.PaymentHistory", function = "update" }
                            []

        SetDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (datePickerSettings shared) subMsg model.datePicker
            in
            case dateEvent of
                Picked newDate ->
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            let
                                newModel =
                                    { model
                                        | selectedDate = Just newDate
                                        , incomingTransfersPageInfo = Nothing
                                        , datePicker = newDatePicker
                                        , incomingTransfers = Nothing
                                    }
                            in
                            newModel
                                |> UR.init
                                |> UR.addCmd (fetchProfileWithTransfers shared community newModel authToken)

                        _ ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Picked a date, but community wasn't loaded"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.PaymentHistory", function = "update" }
                                    []

                _ ->
                    { model | datePicker = newDatePicker }
                        |> UR.init

        ClickedCalendar ->
            model
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.focus "date-picker-input"
                        |> Task.attempt (\_ -> NoOp)
                    )

        ClearDatePicker ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        newModel =
                            { model
                                | incomingTransfers = Nothing
                                , selectedDate = Nothing
                                , incomingTransfersPageInfo = Nothing
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchProfileWithTransfers shared community newModel authToken)

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried clearing date picker, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.PaymentHistory", function = "update" }
                            []

        GotPayerProfileSummaryMsg subMsg ->
            { model | payerProfileSummary = Profile.Summary.update subMsg model.payerProfileSummary }
                |> UR.init



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        pageTitle =
            shared.translators.t "payment_history.title"

        content =
            case model.queryStatus of
                Loaded profile ->
                    div [ class "bg-white" ]
                        [ viewSplash profile
                        , div [ class "mx-4 max-w-md md:m-auto" ]
                            [ h2 [ class "text-center text-black text-2xl" ]
                                [ text (shared.translators.t "payment_history.title") ]
                            , div []
                                [ viewUserAutocomplete loggedIn model
                                , viewDatePicker shared model
                                ]
                            , viewTransfers shared model
                            ]
                        ]

                Failed err ->
                    div [] [ Page.fullPageGraphQLError (shared.translators.t "error.accountNotFound") err ]

                Loading ->
                    Page.fullPageLoading shared
    in
    { title = pageTitle
    , content = content
    }


viewSplash : ProfileWithTransfers -> Html msg
viewSplash p =
    let
        name =
            Maybe.withDefault (Eos.Account.nameToString p.account) p.name
    in
    div
        [ class "bg-purple-500 bg-contain bg-center bg-no-repeat h-56 mb-6"
        , style "background-image" "url(/images/cover_pic_payment_history.svg)"
        ]
        [ h1 [ class "text-white text-center text-2xl pt-6" ] [ text name ]
        ]


viewUserAutocomplete : LoggedIn.Model -> Model -> Html Msg
viewUserAutocomplete loggedIn model =
    div [ class "my-4" ]
        [ View.Components.label []
            { targetId = "elm-select-input"
            , labelText = loggedIn.shared.translators.t "payment_history.user"
            }
        , viewPayerAutocomplete loggedIn model False
        ]


viewPayerAutocomplete : LoggedIn.Model -> Model -> Bool -> Html Msg
viewPayerAutocomplete loggedIn model isDisabled =
    let
        selectedPayers =
            Maybe.map (\v -> [ v ]) model.autocompleteSelectedProfile
                |> Maybe.withDefault []
    in
    div []
        [ Html.map SelectMsg
            (Select.view
                (selectConfiguration loggedIn.shared isDisabled)
                model.autocompleteState
                model.autocompleteProfiles
                selectedPayers
            )
        , viewSelectedPayers model loggedIn selectedPayers
        ]


viewSelectedPayers : Model -> LoggedIn.Model -> List ProfileBase -> Html Msg
viewSelectedPayers model loggedIn selectedPayers =
    div [ class "flex flex-row mt-3 mb-10 flex-wrap" ]
        (selectedPayers
            |> List.map
                (\p ->
                    div
                        [ class "flex justify-between flex-col m-3 items-center" ]
                        [ viewSelectedPayer loggedIn model p
                        , div
                            [ onClick ClearSelect
                            , class "h-6 w-6 flex items-center mt-4"
                            ]
                            [ Icons.trash "" ]
                        ]
                )
        )


viewSelectedPayer : LoggedIn.Model -> Model -> ProfileBase -> Html Msg
viewSelectedPayer loggedIn model profile =
    Profile.Summary.view loggedIn.shared
        loggedIn.accountName
        profile
        model.payerProfileSummary
        |> Html.map GotPayerProfileSummaryMsg


selectConfiguration : Shared -> Bool -> Select.Config Msg ProfileBase
selectConfiguration shared isDisabled =
    Profile.selectConfig
        (Select.newConfig
            { onSelect = OnSelect
            , toLabel = \p -> Eos.Account.nameToString p.account
            , filter = selectFilter 2 (\p -> Eos.Account.nameToString p.account)
            , onFocusItem = NoOp
            }
            |> Select.withInputClass "form-input"
        )
        shared
        isDisabled


selectFilter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
selectFilter minChars toLabel q items =
    if String.length q < minChars then
        Nothing

    else
        items
            |> Simple.Fuzzy.filter toLabel q
            |> Just


viewDatePicker : Shared -> Model -> Html Msg
viewDatePicker shared model =
    div [ class "my-4" ]
        [ View.Components.label []
            { targetId = "date-picker"
            , labelText = shared.translators.t "payment_history.date"
            }
        , div [ class "relative" ]
            [ DatePicker.view
                model.selectedDate
                (datePickerSettings shared)
                model.datePicker
                |> Html.map SetDatePicker
            , img
                [ class "absolute right-0 top-0 h-full cursor-pointer"
                , src "/icons/calendar.svg"
                , tabindex -1
                , onClick ClickedCalendar
                ]
                []
            , case model.selectedDate of
                Just _ ->
                    button
                        [ onClick ClearDatePicker
                        , class "absolute right-0 mr-12 top-0 mt-3"
                        ]
                        [ Icons.trash "" ]

                Nothing ->
                    text ""
            ]
        ]


viewTransfer : Shared -> Transfer -> Html Msg
viewTransfer shared payment =
    let
        payer =
            payment.from

        userName =
            Eos.Account.nameToString payer.account

        time =
            Utils.fromMaybeDateTime (Just payment.blockTime)
                |> Strftime.format "%d %b %Y, %H:%M" shared.timezone

        avatarImg =
            Avatar.view payer.avatar "max-w-full max-h-full"

        amount =
            String.concat
                [ String.fromFloat payment.value
                , " "
                , Eos.symbolToSymbolCodeString payment.community.symbol
                ]
    in
    div
        [ class "bg-gray-100 text-center py-6 my-6 rounded-lg" ]
        [ div [ class "rounded-full m-auto overflow-hidden border-white border-2 bg-grey w-14 h-14" ]
            [ avatarImg ]
        , p [ class "text-black mt-2" ]
            [ text userName ]
        , p [ class "uppercase text-gray-900 text-sm my-1" ]
            [ text time ]
        , p [ class "text-green text-4xl my-3" ]
            [ text amount ]
        , p [ class "tracking-widest text-2xl" ]
            [ text (Emoji.encode payment.createdTx)
            ]
        ]


viewTransfers : Shared -> Model -> Html Msg
viewTransfers shared model =
    case model.incomingTransfers of
        Just transfers ->
            if List.isEmpty transfers then
                div [ class "text-center my-6" ]
                    [ text (shared.translators.t "payment_history.no_transfers_found") ]

            else
                div []
                    [ ul [ class "pb-6" ]
                        (List.map (viewTransfer shared) transfers)
                    , viewPagination shared model
                    ]

        Nothing ->
            div [ class "text-center h-48" ]
                [ div [ class "m-auto spinner" ] []
                , text (shared.translators.t "menu.loading")
                ]


viewPagination : Shared -> Model -> Html Msg
viewPagination shared { incomingTransfersPageInfo } =
    case incomingTransfersPageInfo of
        Just pi ->
            if pi.hasNextPage then
                div [ class "pb-8" ]
                    [ button
                        [ class "button m-auto button-primary w-full sm:w-40"
                        , onClick ShowMore
                        ]
                        [ text (shared.translators.t "payment_history.more") ]
                    ]

            else
                text ""

        Nothing ->
            text ""
