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
import Cambiatus.Object.Profile as User
import Cambiatus.Query
import Cambiatus.Scalar
import Date exposing (Date)
import DatePicker exposing (DateEvent(..), defaultSettings, off)
import Dict exposing (Dict)
import Eos
import Eos.Account
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Hashids
import Html exposing (Html, a, button, div, h1, h2, label, p, span, text, ul)
import Html.Attributes as Attrs exposing (class, href, style)
import Html.Events exposing (onClick)
import I18Next exposing (t)
import Icons
import List.Extra as LE
import Murmur3
import Page
import Profile exposing (Profile, selectionSet)
import Select
import Session.Shared as Shared exposing (Shared)
import Simple.Fuzzy
import Strftime
import Time exposing (Month(..), Weekday(..))
import Transfer exposing (ConnectionTransfer, Transfer)
import UpdateResult as UR
import Utils



-- MSG


type Msg
    = RecipientProfileLoaded (Result (Graphql.Http.Error (Maybe ProfileWithTransfers)) (Maybe ProfileWithTransfers))
    | PayersLoaded (Result (Graphql.Http.Error (Maybe ProfileWithOnlyAutocomplete)) (Maybe ProfileWithOnlyAutocomplete))
    | OnSelect (Maybe ProfileBase)
    | SelectMsg (Select.Msg ProfileBase)
    | SetDatePicker DatePicker.Msg
    | ClearDatePicker
    | ClearSelectSelection
    | ShowMore


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ShowMore ->
            [ "ShowMore" ]

        RecipientProfileLoaded r ->
            [ "RecipientProfileLoaded", UR.resultToString r ]

        PayersLoaded r ->
            [ "PayersFetched", UR.resultToString r ]

        ClearSelectSelection ->
            [ "ClearSelectSelection" ]

        OnSelect _ ->
            [ "OnSelect" ]

        SelectMsg _ ->
            [ "SelectMsg" ]

        SetDatePicker _ ->
            [ "ToDatePicker" ]

        ClearDatePicker ->
            [ "ClearDatePicker" ]



-- MODEL


type alias Model =
    { payerAutocompleteState : Select.State
    , selectedPayer : Maybe ProfileBase
    , queryStatus : QueryStatus (Maybe ProfileWithTransfers) ProfileWithTransfers
    , recipientProfile : Maybe ProfileBase
    , incomingTransfers : Maybe (List Transfer)
    , fetchedPayers : List ProfileBase
    , pageInfo : Maybe Api.Relay.PageInfo
    , selectedDate : Maybe Date
    , datePicker : DatePicker.DatePicker
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


profileWithTransfersSelectionSet : Model -> SelectionSet ProfileWithTransfers Cambiatus.Object.Profile
profileWithTransfersSelectionSet model =
    SelectionSet.map4 ProfileWithTransfers
        User.name
        (Eos.Account.nameSelectionSet User.account)
        (Avatar.selectionSet User.avatar)
        (incomingTransfersSelectionSet model)


{-| Fetch recipient profile details with incoming transfers
-}
fetchProfileWithTransfers : Shared -> Model -> Eos.Account.Name -> Cmd Msg
fetchProfileWithTransfers shared model accountName =
    Api.Graphql.query shared
        (Cambiatus.Query.profile
            { input = { account = Present (Eos.Account.nameToString accountName) } }
            (profileWithTransfersSelectionSet model)
        )
        RecipientProfileLoaded


fetchPayersForAutocomplete : Shared -> Model -> String -> Cmd Msg
fetchPayersForAutocomplete shared model payerAccount =
    let
        autocompleteSelectionSet : SelectionSet ProfileBase Cambiatus.Object.Profile
        autocompleteSelectionSet =
            SelectionSet.map3 ProfileBase
                User.name
                (Eos.Account.nameSelectionSet User.account)
                (Avatar.selectionSet User.avatar)

        selectionSet : SelectionSet ProfileWithOnlyAutocomplete Cambiatus.Object.Profile
        selectionSet =
            SelectionSet.map ProfileWithOnlyAutocomplete
                (User.getPayersByAccount { account = payerAccount } autocompleteSelectionSet)
    in
    case model.recipientProfile of
        Just p ->
            Api.Graphql.query shared
                (Cambiatus.Query.profile
                    { input =
                        { account = Present (Eos.Account.nameToString p.account)
                        }
                    }
                    selectionSet
                )
                PayersLoaded

        Nothing ->
            Cmd.none


fetchIncomingTransfers : Shared -> Model -> Cmd Msg
fetchIncomingTransfers shared model =
    case model.recipientProfile of
        Just p ->
            let
                input =
                    { input =
                        { account = Present (Eos.Account.nameToString p.account)
                        }
                    }
            in
            Api.Graphql.query shared
                (Cambiatus.Query.profile
                    input
                    (profileWithTransfersSelectionSet model)
                )
                RecipientProfileLoaded

        Nothing ->
            Cmd.none


incomingTransfersSelectionSet model =
    let
        endCursor =
            Maybe.andThen .endCursor model.pageInfo

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
            case model.selectedPayer of
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
    User.transfers
        optionalArgsFn
        Transfer.transferConnectionSelectionSet


datePickerSettings : Shared -> DatePicker.Settings
datePickerSettings shared =
    { defaultSettings
        | changeYear = off
        , placeholder = I18Next.t shared.translations "payment_history.pick_date"
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


{-| A generic representation of `Guest.Model` and `LoggedIn.Model`. We need this since the Payment History page
works for guests and for logged-in users and `init`, `update`, and `view` functions have to accept both of these models.
-}
type alias SharedModel m =
    { m | shared : Shared }


init : SharedModel m -> ( Model, Cmd Msg )
init { shared } =
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

        initModel =
            { payerAutocompleteState = Select.newState ""
            , selectedPayer = Nothing
            , selectedDate = Nothing
            , queryStatus = Loading
            , recipientProfile = Nothing
            , incomingTransfers = Nothing
            , fetchedPayers = []
            , pageInfo = Nothing
            , datePicker = datePicker
            }
    in
    ( initModel
    , Cmd.batch
        [ Cmd.map SetDatePicker datePickerCmd
        , fetchProfileWithTransfers shared initModel recipientAccountName
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


update : Msg -> Model -> SharedModel m -> UR.UpdateResult Model Msg extMsg
update msg model { shared } =
    case msg of
        PayersLoaded (Ok maybeProfileWithAutocomplete) ->
            case maybeProfileWithAutocomplete of
                Just profile ->
                    let
                        payers =
                            case profile.getPayersByAccount of
                                Just l ->
                                    l

                                Nothing ->
                                    []

                        toList p =
                            case p of
                                Just val ->
                                    [ val ]

                                Nothing ->
                                    []

                        newModel =
                            { model | fetchedPayers = List.concat (List.map toList payers) }
                    in
                    newModel
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init

        PayersLoaded (Err err) ->
            model
                |> UR.init
                |> UR.logGraphqlError msg err

        RecipientProfileLoaded (Ok maybeProfile) ->
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
                                , recipientProfile = Just recipientProfile
                                , incomingTransfers = Just newIncomingTransfers
                                , pageInfo = pageInfo
                            }
                    in
                    newModel
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init

        RecipientProfileLoaded (Err err) ->
            { model
                | queryStatus = Failed err
            }
                |> UR.init
                |> UR.logGraphqlError msg err

        ShowMore ->
            model
                |> UR.init
                |> UR.addCmd (fetchIncomingTransfers shared model)

        OnSelect maybeProfile ->
            let
                newModel =
                    { model
                        | incomingTransfers = Nothing
                        , pageInfo = Nothing
                        , selectedPayer = maybeProfile
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd (fetchIncomingTransfers shared newModel)

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration shared False) subMsg model.payerAutocompleteState
            in
            case Select.queryFromState model.payerAutocompleteState of
                Just payer ->
                    { model | payerAutocompleteState = updated }
                        |> UR.init
                        |> UR.addCmd (fetchPayersForAutocomplete shared model payer)
                        |> UR.addCmd cmd

                Nothing ->
                    { model | payerAutocompleteState = updated }
                        |> UR.init
                        |> UR.addCmd cmd

        ClearSelectSelection ->
            let
                newModel =
                    { model
                        | incomingTransfers = Nothing
                        , pageInfo = Nothing
                        , selectedPayer = Nothing
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd (fetchIncomingTransfers shared newModel)

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
                                , pageInfo = Nothing
                                , datePicker = newDatePicker
                                , incomingTransfers = Nothing
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchIncomingTransfers shared newModel)

                _ ->
                    { model | datePicker = newDatePicker }
                        |> UR.init

        ClearDatePicker ->
            let
                newModel =
                    { model
                        | incomingTransfers = Nothing
                        , selectedDate = Nothing
                        , pageInfo = Nothing
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd (fetchIncomingTransfers shared newModel)


{-| Convert Transfer identifier (64 symbols) to emoji sequence (8 symbols)
-}
transferIdToEmojis : String -> String
transferIdToEmojis transferId =
    transferId
        |> Murmur3.hashString salt
        -- make 32 bit number
        |> Hashids.encode hashidsCtx
        -- make short has from the given alphabet
        |> String.split ""
        |> List.map (\n -> Maybe.withDefault "" (Dict.get n alphabetEmojiMapper))
        -- map hash symbols to emojis
        |> String.join " "


salt : Int
salt =
    123456


alphabet : String
alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"


hashidsCtx : Hashids.Context
hashidsCtx =
    Hashids.createHashidsContext (String.fromInt salt) 8 alphabet


alphabetEmojiMapper : Dict String String
alphabetEmojiMapper =
    let
        emojis =
            -- Must be separated by the space
            "😄 😘 😺 🚀 🚗 🚙 🚚 🌀 🌟 🌴 🌵 🌷 🌸 🌹 🌺 🌻 🌼 🌽 🌿 🍁 🍄 🍅 🍆 🍇 🍈 🍉 🍊 🍌 🍍 🍎 🍏 🍑 🍒 🍓 🍭 🍯 🎀 🎃 🎈 🎨 🎲 🎸 🏡 🐌 🐒 🐚 🐞 🐬 🐱 🐲 🐳 🐴 🐵 🐶 🐸 🐹 💜 😎 🚘 🌳 🍋 🍐"
    in
    List.map2 Tuple.pair (String.split "" alphabet) (String.split " " emojis)
        |> Dict.fromList



-- VIEW


view : SharedModel m -> Model -> Html Msg
view { shared } model =
    case model.queryStatus of
        Loaded profile ->
            div [ class "bg-white" ]
                [ viewSplash profile
                , div [ class "mx-4 max-w-md md:m-auto" ]
                    [ h2 [ class "text-center text-black text-2xl" ]
                        [ text (I18Next.t shared.translations "payment_history.title") ]
                    , div []
                        [ viewUserAutocomplete shared model
                        , viewDatePicker shared model
                        ]
                    , viewTransfers shared model
                    ]
                ]

        Failed err ->
            div [] [ Page.fullPageGraphQLError (I18Next.t shared.translations "error.accountNotFound") err ]

        Loading ->
            Page.fullPageLoading


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
                [ text (I18Next.t shared.translations "payment_history.user") ]
            ]
        , viewPayerAutocomplete shared model False
        ]


viewPayerAutocomplete : Shared -> Model -> Bool -> Html Msg
viewPayerAutocomplete shared model isDisabled =
    let
        selectedPayers =
            Maybe.map (\v -> [ v ]) model.selectedPayer
                |> Maybe.withDefault []
    in
    div []
        [ Html.map SelectMsg
            (Select.view
                (selectConfiguration shared isDisabled)
                model.payerAutocompleteState
                model.fetchedPayers
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
                            [ onClick ClearSelectSelection
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
            case model.recipientProfile of
                Just p ->
                    if profile.account == p.account then
                        text (I18Next.t shared.translations "transfer_result.you")

                    else
                        case profile.userName of
                            Just u ->
                                text u

                            Nothing ->
                                Eos.Account.viewName profile.account

                Nothing ->
                    text ""

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
            [ Avatar.view shared.endpoints.ipfs profile.avatar "w-10 h-10"
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
        |> Select.withPrompt (t shared.translations "community.actions.form.verifier_placeholder")
        |> Select.withItemHtml (viewAutoCompleteItem shared)
        |> Select.withMenuClass "border-t-none border-solid border-gray-100 border rounded-b z-30 bg-white"


viewAutoCompleteItem : Shared -> ProfileBase -> Html Never
viewAutoCompleteItem shared profile =
    let
        ipfsUrl =
            shared.endpoints.ipfs
    in
    div [ class "pt-3 pl-3 flex flex-row items-center w-select z-30" ]
        [ div [ class "pr-3" ] [ Avatar.view ipfsUrl profile.avatar "h-7 w-7" ]
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


selectConfiguration : Shared.Shared -> Bool -> Select.Config Msg ProfileBase
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
                [ text (I18Next.t shared.translations "payment_history.date") ]
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
                        [ onClick <| ClearDatePicker
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

        ipfsUrl =
            shared.endpoints.ipfs

        avatarImg =
            Avatar.view ipfsUrl payer.avatar "max-w-full max-h-full"

        amount =
            String.concat
                [ String.fromFloat payment.value
                , " "
                , Eos.symbolToString payment.symbol
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
            [ text (transferIdToEmojis payment.createdTx)
            ]
        ]


viewTransfers : Shared -> Model -> Html Msg
viewTransfers shared model =
    case model.incomingTransfers of
        Just transfers ->
            if List.isEmpty transfers then
                div [ class "text-center my-6" ] [ text (I18Next.t shared.translations "payment_history.no_transfers_found") ]

            else
                div []
                    [ ul [ class "" ]
                        (List.map (viewTransfer shared) transfers)
                    , viewPagination shared model
                    ]

        Nothing ->
            div [ class "text-center leading-10 h-48" ]
                [ div [ class "m-auto spinner" ] []
                , text (I18Next.t shared.translations "menu.loading")
                ]


viewPagination : Shared -> Model -> Html Msg
viewPagination shared { pageInfo } =
    case pageInfo of
        Just pi ->
            if pi.hasNextPage then
                div [ class "pb-8" ]
                    [ button
                        [ class "button m-auto button-primary w-full sm:w-40"
                        , onClick ShowMore
                        ]
                        [ text (I18Next.t shared.translations "payment_history.more") ]
                    ]

            else
                text ""

        Nothing ->
            text ""
