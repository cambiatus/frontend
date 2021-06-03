module Page.PaymentHistory exposing
    ( Model
    , Msg
    , init
    , msgToString
    , update
    , view
    )

import Api.Graphql
import Api.Relay
import Avatar exposing (Avatar)
import Cambiatus.Enum.TransferDirection exposing (TransferDirection(..))
import Cambiatus.Object
import Cambiatus.Object.User as User
import Cambiatus.Query
import Cambiatus.Scalar
import Date exposing (Date)
import DatePicker exposing (DateEvent(..), defaultSettings, off)
import Emoji
import Eos
import Eos.Account
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, h1, h2, label, p, span, text, ul)
import Html.Attributes as Attrs exposing (class, href, style)
import Html.Events exposing (onClick)
import Icons
import List.Extra as LE
import Page
import RemoteData exposing (RemoteData)
import Select
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import Strftime
import Time exposing (Weekday(..))
import Transfer exposing (ConnectionTransfer, Transfer)
import UpdateResult as UR
import Utils



-- MSG


type Msg
    = RecipientProfileWithTransfersLoaded (RemoteData (Graphql.Http.Error (Maybe ProfileWithTransfers)) (Maybe ProfileWithTransfers))
    | AutocompleteProfilesLoaded (RemoteData (Graphql.Http.Error (Maybe ProfileWithOnlyAutocomplete)) (Maybe ProfileWithOnlyAutocomplete))
    | OnSelect (Maybe ProfileBase)
    | SelectMsg (Select.Msg ProfileBase)
    | ClearSelect
    | SetDatePicker DatePicker.Msg
    | ClearDatePicker
    | ShowMore


msgToString : Msg -> List String
msgToString msg =
    case msg of
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

        ClearDatePicker ->
            [ "ClearDatePicker" ]



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
    }


type QueryStatus err resp
    = Loading
    | Loaded resp
    | Failed (Graphql.Http.Error err)


type alias ProfileBase =
    { userName : Maybe String
    , account : Eos.Account.Name
    , avatar : Avatar
    }


type alias ProfileWithTransfers =
    { userName : Maybe String
    , account : Eos.Account.Name
    , avatar : Avatar
    , transfers : Maybe ConnectionTransfer
    }


type alias ProfileWithOnlyAutocomplete =
    { getPayersByAccount : Maybe (List (Maybe ProfileBase))
    }


profileWithTransfersSelectionSet : Model -> SelectionSet ProfileWithTransfers Cambiatus.Object.User
profileWithTransfersSelectionSet model =
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
                    , date = optionalDate
                    , direction = Present Incoming
                    , secondPartyAccount = optionalFromAccount
                }
    in
    SelectionSet.map4 ProfileWithTransfers
        User.name
        (Eos.Account.nameSelectionSet User.account)
        (Avatar.selectionSet User.avatar)
        (User.transfers
            optionalArgsFn
            Transfer.transferConnectionSelectionSet
        )


fetchProfileWithTransfers : Shared -> Model -> String -> Cmd Msg
fetchProfileWithTransfers shared model authToken =
    let
        accountName =
            Eos.Account.nameToString model.recipientProfile.account
    in
    Api.Graphql.query shared
        (Just authToken)
        (Cambiatus.Query.user
            { account = accountName }
            (profileWithTransfersSelectionSet model)
        )
        RecipientProfileWithTransfersLoaded


fetchProfilesForAutocomplete : Shared -> Model -> String -> String -> Cmd Msg
fetchProfilesForAutocomplete shared model payerAccount authToken =
    let
        autocompleteSelectionSet : SelectionSet ProfileBase Cambiatus.Object.User
        autocompleteSelectionSet =
            SelectionSet.map3 ProfileBase
                User.name
                (Eos.Account.nameSelectionSet User.account)
                (Avatar.selectionSet User.avatar)

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
        , inputClassList =
            [ ( "input", True )
            , ( "w-full", True )
            ]
        , dateFormatter = Date.format "E, d MMM y"
        , firstDayOfWeek = Mon
        , inputAttributes =
            [ Attrs.required False
            , Attrs.readonly True
            ]
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init { shared, authToken } =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init

        recipientAccountName =
            let
                uriLastPart =
                    String.split "/" shared.url.path
                        |> LE.last
            in
            Eos.Account.stringToName <|
                Maybe.withDefault "" uriLastPart

        recipientProfile : ProfileBase
        recipientProfile =
            { userName = Nothing, account = recipientAccountName, avatar = Avatar.empty }

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
            }
    in
    ( initModel
    , Cmd.batch
        [ Cmd.map SetDatePicker datePickerCmd
        , fetchProfileWithTransfers shared initModel authToken
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
update msg model { shared, authToken } =
    case msg of
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
                |> UR.logGraphqlError msg err

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
                            { userName = profile.userName
                            , account = profile.account
                            , avatar = profile.avatar
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
                |> UR.logGraphqlError msg err

        RecipientProfileWithTransfersLoaded _ ->
            UR.init model

        ShowMore ->
            model
                |> UR.init
                |> UR.addCmd (fetchProfileWithTransfers shared model authToken)

        OnSelect maybeProfile ->
            let
                newModel =
                    { model
                        | incomingTransfers = Nothing
                        , incomingTransfersPageInfo = Nothing
                        , autocompleteSelectedProfile = maybeProfile
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd (fetchProfileWithTransfers shared newModel authToken)

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
                |> UR.addCmd (fetchProfileWithTransfers shared newModel authToken)

        SetDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (datePickerSettings shared) subMsg model.datePicker
            in
            case dateEvent of
                Picked newDate ->
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
                        |> UR.addCmd (fetchProfileWithTransfers shared newModel authToken)

                _ ->
                    { model | datePicker = newDatePicker }
                        |> UR.init

        ClearDatePicker ->
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
                |> UR.addCmd (fetchProfileWithTransfers shared newModel authToken)



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view { shared } model =
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
                                [ viewUserAutocomplete shared model
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
            Maybe.withDefault (Eos.Account.nameToString p.account) p.userName
    in
    div
        [ class "bg-purple-500 bg-contain bg-center bg-no-repeat h-56 mb-6"
        , style "background-image" "url(/images/cover_pic_payment_history.svg)"
        ]
        [ h1 [ class "text-white text-center text-2xl pt-6" ] [ text name ]
        ]


viewUserAutocomplete : Shared -> Model -> Html Msg
viewUserAutocomplete shared model =
    div [ class "my-4" ]
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text (shared.translators.t "payment_history.user") ]
            ]
        , viewPayerAutocomplete shared model False
        ]


viewPayerAutocomplete : Shared -> Model -> Bool -> Html Msg
viewPayerAutocomplete shared model isDisabled =
    let
        selectedPayers =
            Maybe.map (\v -> [ v ]) model.autocompleteSelectedProfile
                |> Maybe.withDefault []
    in
    div []
        [ Html.map SelectMsg
            (Select.view
                (selectConfiguration shared isDisabled)
                model.autocompleteState
                model.autocompleteProfiles
                selectedPayers
            )
        , viewSelectedPayers model shared selectedPayers
        ]


viewSelectedPayers : Model -> Shared -> List ProfileBase -> Html Msg
viewSelectedPayers model shared selectedPayers =
    div [ class "flex flex-row mt-3 mb-10 flex-wrap" ]
        (selectedPayers
            |> List.map
                (\p ->
                    div
                        [ class "flex justify-between flex-col m-3 items-center" ]
                        [ viewSelectedPayer shared model p
                        , div
                            [ onClick ClearSelect
                            , class "h-6 w-6 flex items-center mt-4"
                            ]
                            [ Icons.trash "" ]
                        ]
                )
        )


viewSelectedPayer : Shared -> Model -> ProfileBase -> Html msg
viewSelectedPayer shared model profile =
    let
        accountName =
            if profile.account == model.recipientProfile.account then
                text (shared.translators.t "transfer_result.you")

            else
                case profile.userName of
                    Just u ->
                        text u

                    Nothing ->
                        Eos.Account.viewName profile.account

        accountNameContainer =
            div [ class "flex items-center bg-black rounded-label p-1" ]
                [ p [ class "mx-2 pt-caption uppercase font-bold text-white text-caption" ]
                    [ accountName ]
                ]
    in
    a
        [ class "flex flex-col items-center"
        , href ("/profile/" ++ Eos.Account.nameToString profile.account)
        ]
        [ div [ class "w-10 h-10 rounded-full" ]
            [ Avatar.view profile.avatar "w-10 h-10"
            ]
        , div [ class "mt-2" ]
            [ accountNameContainer ]
        ]


selectConfig : Select.Config msg ProfileBase -> Shared -> Bool -> Select.Config msg ProfileBase
selectConfig select shared isDisabled =
    select
        |> Select.withInputClass "form-input h-12 w-full font-sans placeholder-gray-900"
        |> Select.withClear False
        |> Select.withMultiInputItemContainerClass "hidden h-0"
        |> Select.withNotFound "No matches"
        |> Select.withNotFoundClass "text-red  border-solid border-gray-100 border rounded z-30 bg-white w-select"
        |> Select.withNotFoundStyles [ ( "padding", "0 2rem" ) ]
        |> Select.withDisabled isDisabled
        |> Select.withHighlightedItemClass "autocomplete-item-highlight"
        |> Select.withPrompt (shared.translators.t "community.actions.form.verifier_placeholder")
        |> Select.withItemHtml (viewAutoCompleteItem shared)
        |> Select.withMenuClass "border-t-none border-solid border-gray-100 border rounded-b z-30 bg-white"


viewAutoCompleteItem : Shared -> ProfileBase -> Html Never
viewAutoCompleteItem shared profile =
    div [ class "pt-3 pl-3 flex flex-row items-center w-select z-30" ]
        [ div [ class "pr-3" ] [ Avatar.view profile.avatar "h-7 w-7" ]
        , div [ class "flex flex-col font-sans border-b border-gray-500 pb-3 w-full" ]
            [ span [ class "text-black text-body leading-loose" ]
                [ text (Eos.Account.nameToString profile.account) ]
            , span [ class "leading-caption uppercase text-green text-caption" ]
                [ case profile.userName of
                    Just name ->
                        text name

                    Nothing ->
                        text ""
                ]
            ]
        ]


selectConfiguration : Shared -> Bool -> Select.Config Msg ProfileBase
selectConfiguration shared isDisabled =
    selectConfig
        (Select.newConfig
            { onSelect = OnSelect
            , toLabel = \p -> Eos.Account.nameToString p.account
            , filter = selectFilter 2 (\p -> Eos.Account.nameToString p.account)
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
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text (shared.translators.t "payment_history.date") ]
            ]
        , div [ class "relative" ]
            [ DatePicker.view
                model.selectedDate
                (datePickerSettings shared)
                model.datePicker
                |> Html.map SetDatePicker
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
            Utils.posixDateTime (Just payment.blockTime)
                |> Strftime.format "%d %b %Y, %H:%M" Time.utc

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
        , p [ class "uppercase text-gray-900 text-xs my-1" ]
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
            div [ class "text-center leading-10 h-48" ]
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
