module Page.PaymentHistory exposing
    ( Model
    , Msg
    , init
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api.Relay
import Avatar exposing (Avatar)
import Cambiatus.Enum.TransferDirectionValue as TransferDirectionValue
import Cambiatus.Object
import Cambiatus.Object.User as User
import Cambiatus.Query
import Cambiatus.Scalar
import Community
import Date exposing (Date)
import Emoji
import Eos
import Eos.Account
import Form.DatePicker
import Form.UserPicker
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, button, div, h1, h2, p, text, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Log
import Markdown exposing (Markdown)
import Page
import Profile
import Profile.Contact
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Strftime
import Transfer exposing (ConnectionTransfer, Transfer)
import UpdateResult as UR
import Utils



-- MSG


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | RecipientProfileWithTransfersLoaded (RemoteData (Graphql.Http.Error (Maybe ProfileWithTransfers)) (Maybe ProfileWithTransfers))
    | AutocompleteProfilesLoaded (RemoteData (Graphql.Http.Error (Maybe ProfileWithOnlyAutocomplete)) (Maybe ProfileWithOnlyAutocomplete))
    | GotUserPickerMsg Form.UserPicker.Msg
    | GotDatePickerMsg Form.DatePicker.Msg
    | ShowMore


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

        GotUserPickerMsg subMsg ->
            "GotUserPickerMsg" :: Form.UserPicker.msgToString subMsg

        GotDatePickerMsg subMsg ->
            "GotDatePickerMsg" :: Form.DatePicker.msgToString subMsg



-- MODEL


type alias Model =
    { queryStatus : QueryStatus (Maybe ProfileWithTransfers) ProfileWithTransfers
    , recipientProfile : Profile.Minimal
    , incomingTransfers : Maybe (List Transfer)
    , incomingTransfersPageInfo : Maybe Api.Relay.PageInfo
    , autocompleteProfiles : List Profile.Minimal
    , autocompleteSelectedProfile : Maybe Profile.Minimal
    , userPicker : Form.UserPicker.SinglePickerModel
    , datePicker : Form.DatePicker.Model
    , selectedDate : Maybe Date
    }


type QueryStatus err resp
    = Loading
    | Loaded resp
    | Failed (Graphql.Http.Error err)


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
    { getPayersByAccount : Maybe (List (Maybe Profile.Minimal))
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


fetchProfileWithTransfers : LoggedIn.Model -> Community.Model -> Model -> LoggedIn.External Msg
fetchProfileWithTransfers loggedIn community model =
    let
        accountName =
            Eos.Account.nameToString model.recipientProfile.account
    in
    LoggedIn.query loggedIn
        (Cambiatus.Query.user
            { account = accountName }
            (profileWithTransfersSelectionSet community model)
        )
        RecipientProfileWithTransfersLoaded


fetchProfilesForAutocomplete : LoggedIn.Model -> Model -> String -> LoggedIn.External Msg
fetchProfilesForAutocomplete loggedIn model payerAccount =
    let
        autocompleteSelectionSet : SelectionSet Profile.Minimal Cambiatus.Object.User
        autocompleteSelectionSet =
            SelectionSet.succeed Profile.Minimal
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
    LoggedIn.query loggedIn
        (Cambiatus.Query.user
            { account = accountName
            }
            selectionSet
        )
        AutocompleteProfilesLoaded


init : Eos.Account.Name -> LoggedIn.Model -> ( Model, Cmd Msg )
init recipientAccountName loggedIn =
    let
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
            , autocompleteSelectedProfile = Nothing
            , userPicker = Form.UserPicker.initSingle { id = "user-picker" }
            , datePicker = Form.DatePicker.initModel (Date.fromPosix loggedIn.shared.timezone loggedIn.shared.now)
            , selectedDate = Nothing
            }
    in
    ( initModel
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
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


update : Msg -> Model -> LoggedIn.Model -> UR.UpdateResult Model Msg (LoggedIn.External Msg)
update msg model ({ shared } as loggedIn) =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            model
                |> UR.init
                |> UR.addExt (fetchProfileWithTransfers loggedIn community model)

        AutocompleteProfilesLoaded (RemoteData.Success maybeProfileWithPayers) ->
            case maybeProfileWithPayers of
                Just profileWithPayers ->
                    let
                        payers : List (Maybe Profile.Minimal)
                        payers =
                            Maybe.withDefault [] profileWithPayers.getPayersByAccount

                        toList : Maybe Profile.Minimal -> List Profile.Minimal
                        toList p =
                            case p of
                                Just val ->
                                    [ val ]

                                Nothing ->
                                    []

                        profiles : List Profile.Minimal
                        profiles =
                            List.concatMap toList payers
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

                        recipientProfile : Profile.Minimal
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
                        |> UR.addExt (fetchProfileWithTransfers loggedIn community model)

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Clicked show more, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.PaymentHistory", function = "update" }
                            []

        GotUserPickerMsg subMsg ->
            let
                ( newUserPicker, userPickerCmd, maybeMsg ) =
                    Form.UserPicker.update
                        (userPickerOptions loggedIn model.autocompleteProfiles)
                        (userPickerViewConfig loggedIn.shared model)
                        subMsg
                        (Form.UserPicker.fromSinglePicker model.userPicker)
                        |> (\( userPicker, cmd, maybeMsg_ ) ->
                                ( Form.UserPicker.toSinglePicker userPicker, cmd, maybeMsg_ )
                           )

                modelWithUserPicker =
                    { model
                        | userPicker = newUserPicker
                        , autocompleteSelectedProfile = Form.UserPicker.getSingleProfile newUserPicker
                    }

                ( newModel, fetchProfiles ) =
                    if Form.UserPicker.getSingleProfile newUserPicker == Form.UserPicker.getSingleProfile model.userPicker then
                        ( modelWithUserPicker, identity )

                    else
                        case loggedIn.selectedCommunity of
                            RemoteData.Success community ->
                                let
                                    newModel_ =
                                        { modelWithUserPicker
                                            | incomingTransfers = Nothing
                                            , incomingTransfersPageInfo = Nothing
                                        }
                                in
                                ( newModel_
                                , UR.addExt (fetchProfileWithTransfers loggedIn community newModel_)
                                )

                            _ ->
                                ( modelWithUserPicker
                                , UR.logImpossible msg
                                    "Picked a user, but community wasn't loaded"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.PaymentHistory", function = "update" }
                                    []
                                )

                fetchProfileInfo =
                    if Form.UserPicker.getCurrentQuery (Form.UserPicker.fromSinglePicker newUserPicker) == Form.UserPicker.getCurrentQuery (Form.UserPicker.fromSinglePicker model.userPicker) then
                        identity

                    else
                        UR.addExt
                            (fetchProfilesForAutocomplete loggedIn
                                model
                                (Form.UserPicker.getCurrentQuery
                                    (Form.UserPicker.fromSinglePicker newUserPicker)
                                )
                            )
            in
            newModel
                |> UR.init
                |> UR.addCmd (Cmd.map GotUserPickerMsg userPickerCmd)
                |> UR.addMsg (Maybe.withDefault NoOp maybeMsg)
                |> fetchProfiles
                |> fetchProfileInfo

        GotDatePickerMsg subMsg ->
            let
                ( newDatePicker, datePickerCmd ) =
                    Form.DatePicker.update (datePickerOptions shared)
                        (datePickerViewConfig shared model)
                        subMsg
                        model.datePicker

                modelWithDatePicker =
                    { model
                        | datePicker = newDatePicker
                        , selectedDate = Form.DatePicker.getDate newDatePicker
                    }

                ( newModel, fetchProfile ) =
                    if modelWithDatePicker.selectedDate == model.selectedDate then
                        ( modelWithDatePicker, identity )

                    else
                        case loggedIn.selectedCommunity of
                            RemoteData.Success community ->
                                let
                                    newModel_ =
                                        { modelWithDatePicker
                                            | incomingTransfers = Nothing
                                            , incomingTransfersPageInfo = Nothing
                                        }
                                in
                                ( newModel_
                                , UR.addExt (fetchProfileWithTransfers loggedIn community newModel_)
                                )

                            _ ->
                                ( modelWithDatePicker
                                , UR.logImpossible msg
                                    "Picked a date, but community wasn't loaded"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.PaymentHistory", function = "update" }
                                    []
                                )
            in
            newModel
                |> UR.init
                |> UR.addCmd (Cmd.map GotDatePickerMsg datePickerCmd)
                |> fetchProfile



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
    Form.UserPicker.view (userPickerOptions loggedIn model.autocompleteProfiles)
        (userPickerViewConfig loggedIn.shared model)
        GotUserPickerMsg


userPickerOptions : LoggedIn.Model -> List Profile.Minimal -> Form.UserPicker.Options msg
userPickerOptions loggedIn profiles =
    Form.UserPicker.init
        { label = loggedIn.shared.translators.t "payment_history.user"
        , currentUser = loggedIn.accountName
        , profiles = profiles
        }
        |> Form.UserPicker.withContainerAttrs [ class "my-4" ]


userPickerViewConfig : Shared -> Model -> Form.UserPicker.ViewConfig Msg
userPickerViewConfig { translators } model =
    { onBlur = NoOp
    , value = Form.UserPicker.fromSinglePicker model.userPicker
    , error = text ""
    , hasError = False
    , translators = translators
    }


viewDatePicker : Shared -> Model -> Html Msg
viewDatePicker shared model =
    Form.DatePicker.view (datePickerOptions shared)
        (datePickerViewConfig shared model)
        GotDatePickerMsg


datePickerOptions : Shared -> Form.DatePicker.Options msg
datePickerOptions { translators } =
    Form.DatePicker.init
        { label = translators.t "payment_history.date"
        , id = "date-picker"
        }
        |> Form.DatePicker.withContainerAttrs [ class "my-4" ]


datePickerViewConfig : Shared -> Model -> Form.DatePicker.ViewConfig msg
datePickerViewConfig { translators } model =
    { value = model.datePicker
    , error = text ""
    , hasError = False
    , isRequired = False
    , translators = translators
    }


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
