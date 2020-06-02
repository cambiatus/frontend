module Page.PaymentHistory exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Api.Relay exposing (Edge)
import Avatar exposing (Avatar)
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
import I18Next exposing (Translations, t)
import Icons
import List.Extra as LE
import Murmur3
import Page
import Profile exposing (Profile)
import Select
import Session.Guest as Guest exposing (External(..))
import Session.Shared as Shared exposing (Shared)
import Strftime
import Time exposing (Month(..), Weekday(..))
import Transfer exposing (ConnectionTransfer, Transfer)
import UpdateResult as UR
import Utils



-- MSG


type Msg
    = NoOp
    | TransfersLoaded (Result (Graphql.Http.Error (Maybe ConnectionTransfer)) (Maybe ConnectionTransfer))
    | ProfileLoaded (Result (Graphql.Http.Error (Maybe ShortProfile)) (Maybe ShortProfile))
    | OnSelect (Maybe ShortProfile)
    | SelectMsg (Select.Msg ShortProfile)
    | PayersFetched (Result (Graphql.Http.Error (Maybe (List (Maybe ShortProfile)))) (Maybe (List (Maybe ShortProfile))))
    | SetDatePicker DatePicker.Msg
    | ClearSelectedDate
    | ClearSelectSelection
    | ShowMore


msgToString : Msg -> List String
msgToString msg =
    let
        resultToString ss r =
            case r of
                Ok _ ->
                    ss ++ [ "Ok" ]

                Err _ ->
                    ss ++ [ "Err" ]
    in
    case msg of
        NoOp ->
            [ "NoOp" ]

        ShowMore ->
            [ "ShowMore" ]

        TransfersLoaded result ->
            resultToString [ "CompletedLoadUserTransfers" ] result

        ProfileLoaded r ->
            [ "CompletedProfileLoad", UR.resultToString r ]

        PayersFetched r ->
            [ "PayersFetched", UR.resultToString r ]

        ClearSelectSelection ->
            [ "ClearSelectSelection" ]

        OnSelect _ ->
            [ "OnSelect" ]

        SelectMsg _ ->
            [ "SelectMsg" ]

        SetDatePicker _ ->
            [ "ToDatePicker" ]

        ClearSelectedDate ->
            [ "ClearSelectedDate" ]



-- MODEL


type alias Model =
    { payerAutocompleteState : Select.State
    , selectedPayer : Maybe ShortProfile
    , recipientProfile : QueryStatus (Maybe ShortProfile) ShortProfile
    , incomingTransfers : QueryStatus (Maybe ConnectionTransfer) (List Transfer)
    , fetchedPayers : List ShortProfile
    , pageInfo : Maybe Api.Relay.PageInfo
    , selectedDate : Maybe Date
    , datePicker : DatePicker.DatePicker
    }


type QueryStatus err resp
    = Loading
    | Loaded resp
    | Failed (Graphql.Http.Error err)


type alias ShortProfile =
    { userName : Maybe String
    , account : Eos.Account.Name
    , avatar : Avatar
    }


fetchRecipientProfile : Shared -> Eos.Account.Name -> Cmd Msg
fetchRecipientProfile shared accountName =
    Api.Graphql.query shared
        (Cambiatus.Query.profile
            { input = { account = Present (Eos.Account.nameToString accountName) } }
            shortProfileSelectionSet
        )
        ProfileLoaded


fetchPayersAutocomplete : Shared -> String -> String -> Cmd Msg
fetchPayersAutocomplete shared recipient payer =
    Api.Graphql.query shared
        (Cambiatus.Query.getPayers
            { payer = payer
            , recipient = recipient
            }
            shortProfileSelectionSet
        )
        PayersFetched


shortProfileSelectionSet : SelectionSet ShortProfile Cambiatus.Object.Profile
shortProfileSelectionSet =
    SelectionSet.map3 ShortProfile
        User.name
        (Eos.Account.nameSelectionSet User.account)
        (Avatar.selectionSet User.avatar)


fetchTransfers : Shared -> Model -> Cmd Msg
fetchTransfers shared model =
    let
        endCursor =
            Maybe.andThen .endCursor model.pageInfo

        afterOption =
            case endCursor of
                Just ec ->
                    Present ec

                Nothing ->
                    Absent

        pagingFn =
            \r ->
                { r
                    | first = Present 16
                    , after = afterOption
                }

        optionalDate =
            case model.selectedDate of
                Just d ->
                    Present (Cambiatus.Scalar.Date (Date.toIsoString d))

                Nothing ->
                    Absent

        optionalPayer =
            case model.selectedPayer of
                Just p ->
                    Present (Eos.Account.nameToString p.account)

                Nothing ->
                    Absent
    in
    case model.recipientProfile of
        Loaded rp ->
            let
                input =
                    { input =
                        { recipient = Eos.Account.nameToString rp.account
                        , date = optionalDate
                        , payer = optionalPayer
                        }
                    }
            in
            Api.Graphql.query shared
                (incomingTransfersSelectionSet pagingFn input)
                TransfersLoaded

        _ ->
            Cmd.none


incomingTransfersSelectionSet optionalArgsFn input =
    Cambiatus.Query.paymentHistory
        optionalArgsFn
        input
        Transfer.transferConnectionSelectionSet


datePickerSettings : DatePicker.Settings
datePickerSettings =
    { defaultSettings
        | changeYear = off
        , placeholder = "Pick a date..."
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


init : Guest.Model -> ( Model, Cmd Msg )
init guest =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init

        recipientAccountName =
            let
                uriLastPart =
                    String.split "/" guest.shared.url.path
                        |> LE.last
            in
            Eos.Account.stringToName <|
                Maybe.withDefault "" uriLastPart
    in
    ( { payerAutocompleteState = Select.newState ""
      , selectedPayer = Nothing
      , selectedDate = Nothing
      , recipientProfile = Loading
      , incomingTransfers = Loading
      , fetchedPayers = []
      , pageInfo = Nothing
      , datePicker = datePicker
      }
    , Cmd.batch
        [ Cmd.map SetDatePicker datePickerCmd
        , fetchRecipientProfile guest.shared recipientAccountName
        ]
    )


type alias EdgeTransfer =
    Edge Transfer


getTransfers : Maybe ConnectionTransfer -> List Transfer
getTransfers maybeObj =
    let
        toMaybeEdges : Maybe ConnectionTransfer -> Maybe (List (Maybe EdgeTransfer))
        toMaybeEdges maybeConn =
            Maybe.andThen
                (\a -> a.edges)
                maybeConn

        toEdges : Maybe (List (Maybe EdgeTransfer)) -> List (Maybe EdgeTransfer)
        toEdges maybeEdges =
            Maybe.withDefault
                []
                maybeEdges

        toMaybeNodes : List (Maybe EdgeTransfer) -> List (Maybe Transfer)
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


update : Msg -> Model -> Guest.Model -> UpdateResult
update msg model guest =
    case msg of
        PayersFetched (Ok maybePayers) ->
            case maybePayers of
                Just payers ->
                    let
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

        PayersFetched (Err err) ->
            model
                |> UR.init
                |> UR.logGraphqlError msg err

        ProfileLoaded (Ok maybeProfile) ->
            case maybeProfile of
                Just profile ->
                    let
                        newModel =
                            { model | recipientProfile = Loaded profile }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchTransfers guest.shared newModel)

                Nothing ->
                    model
                        |> UR.init

        ProfileLoaded (Err err) ->
            { model | recipientProfile = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        TransfersLoaded (Ok maybeTransfers) ->
            let
                pageInfo =
                    Maybe.map .pageInfo maybeTransfers

                newIncomingTransfers =
                    case model.incomingTransfers of
                        Loaded it ->
                            it ++ getTransfers maybeTransfers

                        _ ->
                            getTransfers maybeTransfers
            in
            { model
                | incomingTransfers = Loaded newIncomingTransfers
                , pageInfo = pageInfo
            }
                |> UR.init

        TransfersLoaded (Err err) ->
            { model | incomingTransfers = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        ShowMore ->
            model
                |> UR.init
                |> UR.addCmd (fetchTransfers guest.shared model)

        OnSelect maybeProfile ->
            let
                newModel =
                    { model
                        | incomingTransfers = Loading
                        , pageInfo = Nothing
                        , selectedPayer = maybeProfile
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd (fetchTransfers guest.shared newModel)

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration guest.shared False) subMsg model.payerAutocompleteState
            in
            case ( model.recipientProfile, Select.queryFromState model.payerAutocompleteState ) of
                ( Loaded recipient, Just payer ) ->
                    { model | payerAutocompleteState = updated }
                        |> UR.init
                        |> UR.addCmd (fetchPayersAutocomplete guest.shared (Eos.Account.nameToString recipient.account) payer)
                        |> UR.addCmd cmd

                _ ->
                    { model | payerAutocompleteState = updated }
                        |> UR.init
                        |> UR.addCmd cmd

        ClearSelectSelection ->
            let
                newModel =
                    { model
                        | incomingTransfers = Loading
                        , pageInfo = Nothing
                        , selectedPayer = Nothing
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd (fetchTransfers guest.shared newModel)

        SetDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update datePickerSettings subMsg model.datePicker
            in
            case dateEvent of
                Picked newDate ->
                    let
                        newModel =
                            { model
                                | selectedDate = Just newDate
                                , pageInfo = Nothing
                                , datePicker = newDatePicker
                                , incomingTransfers = Loading
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchTransfers guest.shared newModel)

                _ ->
                    { model | datePicker = newDatePicker }
                        |> UR.init

        ClearSelectedDate ->
            let
                newModel =
                    { model
                        | incomingTransfers = Loading
                        , selectedDate = Nothing
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd (fetchTransfers guest.shared newModel)

        NoOp ->
            model
                |> UR.init


type alias UpdateResult =
    UR.UpdateResult Model Msg External


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


txToEmoji : String -> String
txToEmoji tx =
    tx
        |> Murmur3.hashString salt
        -- make 32 bit number
        |> Hashids.encode hashidsCtx
        -- make short has from the given alphabet
        |> String.split ""
        |> List.map (\n -> Maybe.withDefault "" (Dict.get n alphabetEmojiMapper))
        -- map hash symbols to emojis
        |> String.join " "



-- VIEW


view : Guest.Model -> Model -> Html Msg
view guest model =
    case model.recipientProfile of
        Loaded profile ->
            div [ class "bg-white" ]
                [ viewSplash profile
                , div [ class "mx-4 max-w-md md:m-auto" ]
                    [ h2 [ class "text-center text-black text-2xl" ]
                        [ text "Payment History" ]
                    , div []
                        [ viewUserAutocomplete guest model
                        , viewDatePicker model
                        ]
                    , viewTransfers guest model
                    ]
                ]

        Failed err ->
            div [] [ Page.fullPageGraphQLError "Account not found" err ]

        Loading ->
            Page.fullPageLoading


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


viewUserAutocomplete guest model =
    div [ class "my-4" ]
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text "Payer" ]
            ]
        , viewPayerAutocomplete guest model False
        ]


viewPayerAutocomplete : Guest.Model -> Model -> Bool -> Html Msg
viewPayerAutocomplete guest model isDisabled =
    let
        selectedPayers =
            Maybe.map (\v -> [ v ]) model.selectedPayer
                |> Maybe.withDefault []
    in
    div []
        [ Html.map SelectMsg
            (Select.view
                (selectConfiguration guest.shared isDisabled)
                model.payerAutocompleteState
                model.fetchedPayers
                selectedPayers
            )
        , viewSelectedPayers model guest selectedPayers
        ]


viewSelectedPayers : Model -> Guest.Model -> List ShortProfile -> Html Msg
viewSelectedPayers model guest selectedPayers =
    div [ class "flex flex-row mt-3 mb-10 flex-wrap" ]
        (selectedPayers
            |> List.map
                (\p ->
                    div
                        [ class "flex justify-between flex-col m-3 items-center" ]
                        [ viewSelectedPayer guest.shared model p
                        , div
                            [ onClick ClearSelectSelection
                            , class "h-6 w-6 flex items-center mt-4"
                            ]
                            [ Icons.trash "" ]
                        ]
                )
        )


viewSelectedPayer : Shared -> Model -> ShortProfile -> Html msg
viewSelectedPayer shared model profile =
    let
        accountNameContainer =
            div [ class "flex items-center bg-black rounded-label p-1" ]
                [ p [ class "mx-2 pt-caption uppercase font-bold text-white text-caption" ]
                    [ accountName ]
                ]

        accountName =
            case model.recipientProfile of
                Loaded p ->
                    if profile.account == p.account then
                        text (I18Next.t shared.translations "transfer_result.you")

                    else
                        case profile.userName of
                            Just u ->
                                text u

                            Nothing ->
                                Eos.Account.viewName profile.account

                _ ->
                    text ""
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


selectConfig : Select.Config msg ShortProfile -> Shared -> Bool -> Select.Config msg ShortProfile
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


viewAutoCompleteItem : Shared -> ShortProfile -> Html Never
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


selectConfiguration : Shared.Shared -> Bool -> Select.Config Msg ShortProfile
selectConfiguration shared isDisabled =
    selectConfig
        (Select.newConfig
            { onSelect = OnSelect
            , toLabel = \p -> Eos.Account.nameToString p.account
            , filter = Profile.selectFilter 2 (\p -> Eos.Account.nameToString p.account)
            }
            |> Select.withInputClass "form-input"
        )
        shared
        isDisabled


viewDatePicker model =
    div [ class "my-4" ]
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text "Date" ]
            ]
        , div [ class "relative" ]
            [ DatePicker.view
                model.selectedDate
                datePickerSettings
                model.datePicker
                |> Html.map SetDatePicker
            , case model.selectedDate of
                Just _ ->
                    button
                        [ onClick <| ClearSelectedDate
                        , class "absolute right-0 mr-12 top-0 mt-3"
                        ]
                        [ Icons.trash "" ]

                Nothing ->
                    text ""
            ]
        ]


viewTransfer guest payment =
    let
        payer =
            payment.from

        userName =
            Eos.Account.nameToString payer.account

        time =
            Utils.posixDateTime (Just payment.blockTime)
                |> Strftime.format "%d %b %Y, %H:%M" Time.utc

        ipfsUrl =
            guest.shared.endpoints.ipfs

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
            [ text (txToEmoji payment.createdTx)
            ]
        ]


viewTransfers : Guest.Model -> Model -> Html Msg
viewTransfers guest model =
    case model.incomingTransfers of
        Loaded transfers ->
            if List.isEmpty transfers then
                div [ class "text-center my-6" ] [ text "No transfers were found according your criteria." ]

            else
                div []
                    [ ul [ class "" ]
                        (List.map (viewTransfer guest) transfers)
                    , viewPagination model
                    ]

        Loading ->
            div [ class "text-center leading-10 h-48" ]
                [ div [ class "m-auto spinner" ] []
                , text "Loading transfers"
                ]

        Failed err ->
            div [] [ Page.fullPageGraphQLError "Sorry, something wrong with loading transfers" err ]


viewPagination { pageInfo } =
    case pageInfo of
        Just pi ->
            if pi.hasNextPage then
                div [ class "pb-8" ]
                    [ button
                        [ class "button m-auto button-primary w-full sm:w-40"
                        , onClick ShowMore
                        ]
                        [ text "Show More" ]
                    ]

            else
                text ""

        Nothing ->
            text ""
