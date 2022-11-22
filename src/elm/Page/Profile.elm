module Page.Profile exposing
    ( Model
    , Msg
    , init
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api
import Api.Relay
import Avatar
import Cambiatus.Enum.ContactType as ContactType
import Cambiatus.Enum.CurrencyType
import Cambiatus.Object
import Cambiatus.Object.Claim
import Cambiatus.Object.Network
import Cambiatus.Object.Product
import Cambiatus.Object.TransferConnection
import Cambiatus.Object.User as User
import Cambiatus.Query
import Community
import Eos
import Eos.Account as Eos
import Eos.Explorer
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, br, div, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, href, id, tabindex, target)
import Http
import Icons
import Kyc
import List.Extra as List
import Log
import Markdown
import Page
import Profile
import Profile.Address
import Profile.Contact as Contact
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared, Translators)
import Time
import Transfer exposing (QueryTransfers)
import UpdateResult as UR
import Utils
import View.Components



-- MODEL


type alias Model =
    { profileName : Eos.Name
    , profile : RemoteData (QueryError (Graphql.Http.Error (Maybe Profile.Model))) Profile.Model
    , balance : RemoteData (QueryError Http.Error) Community.Balance
    , graphqlInfo : RemoteData (QueryError (Graphql.Http.Error (Maybe GraphqlInfo))) GraphqlInfo
    , contributionInfo : Maybe ContributionInfo
    , transfersStatus : TransfersStatus
    }



-- INIT


init : LoggedIn.Model -> Eos.Name -> UpdateResult
init loggedIn profileName =
    let
        fetchProfile =
            if loggedIn.accountName == profileName then
                UR.addCmd
                    (LoggedIn.maybeInitWith
                        (Just >> RemoteData.Success >> CompletedLoadProfile)
                        .profile
                        loggedIn
                    )

            else
                UR.addExt
                    (LoggedIn.query loggedIn
                        (Profile.query profileName)
                        CompletedLoadProfile
                    )

        model =
            { profileName = profileName
            , profile = RemoteData.Loading
            , balance = RemoteData.Loading
            , graphqlInfo = RemoteData.Loading
            , contributionInfo = Nothing
            , transfersStatus = Loading []
            }
    in
    model
        |> UR.init
        |> fetchProfile
        |> UR.addCmd (LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn)
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.ContributionsField)



-- TYPES


type QueryError error
    = ResultNotFound
    | ResultWithError error


type TransfersStatus
    = Loaded (List ( Transfer.Transfer, Profile.Summary.Model )) (Maybe Api.Relay.PageInfo)
    | Loading (List ( Transfer.Transfer, Profile.Summary.Model ))
    | FailedLoading


type alias GraphqlInfo =
    { totalTransfers : Int
    , totalClaims : Int
    , totalProducts : Int
    , createdDate : Maybe Time.Posix
    }


type alias ContributionInfo =
    { amount : Float
    , currency : Cambiatus.Enum.CurrencyType.CurrencyType
    }


type VerticalAlign
    = Top
    | Center


type alias Network =
    { symbol : Eos.Symbol
    , createdAt : Time.Posix
    }


type Msg
    = CompletedLoadProfile (RemoteData (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))
    | CompletedLoadCommunity Community.Model
    | CompletedLoadContributions (List Community.Contribution)
    | CompletedLoadBalance (Result (QueryError Http.Error) Community.Balance)
    | CompletedLoadGraphqlInfo (RemoteData (Graphql.Http.Error (Maybe GraphqlInfo)) (Maybe GraphqlInfo))
    | CompletedLoadUserTransfers (RemoteData (Graphql.Http.Error (Maybe QueryTransfers)) (Maybe QueryTransfers))
    | GotTransferCardProfileSummaryMsg Int Profile.Summary.Msg
    | ClickedTransferCard Int
    | RequestedMoreTransfers


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadProfile (RemoteData.Success (Just profile)) ->
            if profile.account == model.profileName then
                { model | profile = RemoteData.Success profile }
                    |> UR.init

            else
                UR.init model

        CompletedLoadProfile (RemoteData.Success Nothing) ->
            { model | profile = RemoteData.Failure ResultNotFound }
                |> UR.init
                |> UR.logImpossible msg
                    "Profile was not found"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Profile", function = "update" }
                    []

        CompletedLoadProfile (RemoteData.Failure error) ->
            { model | profile = RemoteData.Failure (ResultWithError error) }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got a graphql error when loading profile page"
                    { moduleName = "Page.Profile", function = "update" }
                    []
                    error

        CompletedLoadProfile RemoteData.Loading ->
            UR.init model

        CompletedLoadProfile RemoteData.NotAsked ->
            UR.init model

        CompletedLoadCommunity community ->
            let
                fetchBalance =
                    Api.getBalances loggedIn.shared
                        model.profileName
                        (\balancesResult ->
                            case balancesResult of
                                Ok balances ->
                                    balances
                                        |> List.find (\balance -> balance.asset.symbol == community.symbol)
                                        |> Result.fromMaybe ResultNotFound
                                        |> CompletedLoadBalance

                                Err error ->
                                    CompletedLoadBalance (Err (ResultWithError error))
                        )
            in
            model
                |> UR.init
                |> UR.addCmd fetchBalance
                |> UR.addExt
                    (LoggedIn.query loggedIn
                        (graphqlInfoQuery model.profileName community.symbol)
                        CompletedLoadGraphqlInfo
                    )
                |> UR.addExt (fetchTransfers loggedIn community Nothing)

        CompletedLoadContributions contributions ->
            let
                userContributions =
                    contributions
                        |> List.filter (\contribution -> contribution.user.account == loggedIn.accountName)

                contributionInfo : Maybe ContributionInfo
                contributionInfo =
                    case userContributions of
                        [] ->
                            Nothing

                        first :: rest ->
                            Just
                                { amount = List.map .amount (first :: rest) |> List.sum
                                , currency = first.currency
                                }
            in
            { model | contributionInfo = contributionInfo }
                |> UR.init

        CompletedLoadBalance (Ok balance) ->
            { model
                | balance =
                    { balance
                        | lastActivity =
                            -- Blockchain divides posix values by 1000, so
                            -- we need to multiply it by 1000
                            balance.lastActivity
                                |> Time.posixToMillis
                                |> (*) 1000
                                |> Time.millisToPosix
                    }
                        |> RemoteData.Success
            }
                |> UR.init

        CompletedLoadBalance (Err err) ->
            let
                logError =
                    case err of
                        ResultNotFound ->
                            UR.logImpossible msg
                                "Balance was not found on profile page"
                                (Just loggedIn.accountName)
                                { moduleName = "Page.Profile", function = "update" }
                                []

                        ResultWithError httpError ->
                            UR.logHttpError msg
                                (Just loggedIn.accountName)
                                "Got an HTTP error when loading balance on profile page"
                                { moduleName = "Page.Profile", function = "update" }
                                []
                                httpError
            in
            { model | balance = RemoteData.Failure err }
                |> UR.init
                |> logError

        CompletedLoadGraphqlInfo (RemoteData.Success (Just graphqlInfo)) ->
            let
                reportCreatedDateNotFound =
                    case graphqlInfo.createdDate of
                        Nothing ->
                            UR.logImpossible msg
                                "Profile's createdDate is null"
                                (Just loggedIn.accountName)
                                { moduleName = "Page.Profile", function = "update" }
                                []

                        Just _ ->
                            identity
            in
            { model | graphqlInfo = RemoteData.Success graphqlInfo }
                |> UR.init
                |> reportCreatedDateNotFound

        CompletedLoadGraphqlInfo (RemoteData.Success Nothing) ->
            { model | graphqlInfo = RemoteData.Failure ResultNotFound }
                |> UR.init
                |> UR.logImpossible msg
                    "Profile's graphql info not found"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Profile", function = "update" }
                    []

        CompletedLoadGraphqlInfo (RemoteData.Failure error) ->
            { model | graphqlInfo = RemoteData.Failure (ResultWithError error) }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading profile's graphql info"
                    { moduleName = "Page.Profile", function = "update" }
                    []
                    error

        CompletedLoadGraphqlInfo RemoteData.Loading ->
            UR.init model

        CompletedLoadGraphqlInfo RemoteData.NotAsked ->
            UR.init model

        CompletedLoadUserTransfers (RemoteData.Success maybeTransfers) ->
            let
                maybePageInfo : Maybe Api.Relay.PageInfo
                maybePageInfo =
                    maybeTransfers
                        |> Maybe.andThen .transfers
                        |> Maybe.map .pageInfo

                previousTransfers : List ( Transfer.Transfer, Profile.Summary.Model )
                previousTransfers =
                    case model.transfersStatus of
                        Loaded previousTransfers_ _ ->
                            previousTransfers_

                        Loading previousTransfers_ ->
                            previousTransfers_

                        FailedLoading ->
                            []
            in
            { model
                | transfersStatus =
                    Transfer.getTransfers maybeTransfers
                        |> List.map (\transfer -> ( transfer, Profile.Summary.init False ))
                        |> (\transfers -> Loaded (previousTransfers ++ transfers) maybePageInfo)
            }
                |> UR.init

        CompletedLoadUserTransfers (RemoteData.Failure err) ->
            { model | transfersStatus = FailedLoading }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to load user's transfers on profile"
                    { moduleName = "Page.Profile", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        CompletedLoadUserTransfers RemoteData.Loading ->
            UR.init model

        CompletedLoadUserTransfers RemoteData.NotAsked ->
            UR.init model

        GotTransferCardProfileSummaryMsg transferId subMsg ->
            let
                updateTransfers transfers =
                    transfers
                        |> List.updateIf
                            (\( transfer, _ ) -> transfer.id == transferId)
                            (\( transfer, profileSummary ) ->
                                ( transfer, Profile.Summary.update subMsg profileSummary )
                            )
            in
            case model.transfersStatus of
                Loaded transfers pageInfo ->
                    { model | transfersStatus = Loaded (updateTransfers transfers) pageInfo }
                        |> UR.init

                Loading transfers ->
                    { model | transfersStatus = Loading (updateTransfers transfers) }
                        |> UR.init

                FailedLoading ->
                    model
                        |> UR.init

        ClickedTransferCard transferId ->
            UR.init model
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey (Route.ViewTransfer transferId))

        RequestedMoreTransfers ->
            case ( model.transfersStatus, loggedIn.selectedCommunity ) of
                ( Loaded transfers maybePageInfo, RemoteData.Success community ) ->
                    let
                        maybeCursor =
                            Maybe.andThen .endCursor maybePageInfo
                    in
                    { model | transfersStatus = Loading transfers }
                        |> UR.init
                        |> UR.addExt (fetchTransfers loggedIn community maybeCursor)

                _ ->
                    model
                        |> UR.init



-- VIEW


type CombinedErrors
    = ProfileError (QueryError (Graphql.Http.Error (Maybe Profile.Model)))
    | BalanceError (QueryError Http.Error)
    | GraphqlError (QueryError (Graphql.Http.Error (Maybe GraphqlInfo)))


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            case model.profile of
                RemoteData.Success profile ->
                    profile.name
                        |> Maybe.withDefault (Eos.nameToString model.profileName)

                _ ->
                    Eos.nameToString model.profileName

        combinedRemoteData =
            RemoteData.map3
                (\profile balance graphqlInfo ->
                    { profile = profile
                    , balance = balance
                    , graphqlInfo = graphqlInfo
                    }
                )
                (RemoteData.mapError ProfileError model.profile)
                (RemoteData.mapError BalanceError model.balance)
                (RemoteData.mapError GraphqlError model.graphqlInfo)

        content =
            case combinedRemoteData of
                RemoteData.Success { profile, balance, graphqlInfo } ->
                    div [ class "flex flex-grow flex-col" ]
                        [ Page.viewHeader loggedIn (t "menu.profile")
                        , view_ loggedIn model profile balance graphqlInfo
                        ]

                RemoteData.Failure error ->
                    case error of
                        ProfileError (ResultWithError graphqlError) ->
                            Page.fullPageGraphQLError (t "error.unknown") graphqlError

                        BalanceError (ResultWithError httpError) ->
                            Page.fullPageError (t "error.unknown") httpError

                        GraphqlError (ResultWithError graphqlError) ->
                            Page.fullPageGraphQLError (t "error.unknown") graphqlError

                        _ ->
                            Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared
    in
    { title = title
    , content = content
    }


view_ : LoggedIn.Model -> Model -> Profile.Model -> Community.Balance -> GraphqlInfo -> Html Msg
view_ loggedIn model profile balance graphqlInfo =
    div [ class "flex flex-grow bg-gray-100 relative", id "transfer-relative-container" ]
        [ div
            [ class "z-10 w-full bg-grey-100 grid md:grid-cols-2 md:container md:mx-auto" ]
            [ viewProfile loggedIn profile
            , viewDetails loggedIn profile balance graphqlInfo model
            ]
        , div [ class "z-0 absolute w-full h-full md:w-1/2 md:bg-white" ] []
        ]


viewProfile : LoggedIn.Model -> Profile.Model -> Html msg
viewProfile loggedIn profile =
    let
        email =
            Maybe.withDefault "" profile.email

        isProfileOwner =
            loggedIn.accountName == profile.account

        text_ =
            text << loggedIn.shared.translators.t

        blockExplorerButton =
            a
                [ class "button w-full mt-4"
                , classList
                    [ ( "button-primary", isProfileOwner )
                    , ( "button-secondary", not isProfileOwner )
                    ]
                , target "_blank"
                , profile.account
                    |> Eos.Explorer.Profile
                    |> Eos.Explorer.link loggedIn.shared.environment
                    |> href
                ]
                [ text_ "block_explorer.see" ]
    in
    div [ class "bg-white border-white border-r w-full flex md:border-gray-500" ]
        [ div [ class "w-full p-4 max-w-screen container mx-auto self-center md:max-w-lg" ]
            (div [ class "pb-4 w-full" ]
                [ div [ class "flex flex-wrap items-center justify-center mb-4" ]
                    [ Avatar.view profile.avatar "w-20 h-20 mr-6 xs-max:w-16 xs-max:h-16 xs-max:mr-3"
                    , div [ class "flex flex-grow items-center justify-between" ]
                        [ ul [ class "text-sm text-gray-900" ]
                            [ li [ class "font-semibold text-body-black text-2xl xs-max:text-xl" ]
                                [ text (Maybe.withDefault "" profile.name) ]
                            , li [] [ a [ href <| "mailto:" ++ email ] [ text email ] ]
                            , li [] [ text (Eos.nameToString profile.account) ]
                            ]
                        ]
                    , if isProfileOwner then
                        a
                            [ class "ml-2 fill-current text-orange-300 hover:text-orange-100"
                            , Route.href Route.ProfileEditor
                            ]
                            [ Icons.edit "" ]

                      else
                        text ""
                    ]
                , case profile.bio of
                    Nothing ->
                        text ""

                    Just bio ->
                        Markdown.view [ class "text-sm text-gray-900" ] bio
                ]
                :: (if isProfileOwner then
                        [ blockExplorerButton ]

                    else
                        [ a
                            [ class "button button-primary w-full mt-4"
                            , Route.href (Route.Transfer (Just profile.account))
                            ]
                            [ text_ "transfer.title" ]
                        , blockExplorerButton
                        , profile.contacts
                            |> List.map
                                (\contact ->
                                    let
                                        { contactType } =
                                            Contact.unwrap contact

                                        contactClass =
                                            case contactType of
                                                ContactType.Whatsapp ->
                                                    "fill-current text-green"

                                                _ ->
                                                    ""
                                    in
                                    a
                                        [ class ("button-secondary uppercase bg-gray-100 py-2 flex items-center justify-center border-none hover:bg-gray-200 " ++ Contact.contactTypeTextColor contactType)
                                        , Contact.toHref contact
                                        , target "_blank"
                                        ]
                                        [ Contact.contactTypeToIcon ("mr-2 w-6 h-6 " ++ contactClass)
                                            True
                                            contactType
                                        , text (Contact.contactTypeToString loggedIn.shared.translators contactType)
                                        ]
                                )
                            |> div [ class "flex flex-col space-y-4 mt-4 mb-2" ]
                        ]
                   )
            )
        ]


viewDetails : LoggedIn.Model -> Profile.Model -> Community.Balance -> GraphqlInfo -> Model -> Html Msg
viewDetails loggedIn profile balance graphqlInfo model =
    let
        text_ =
            text << loggedIn.shared.translators.t

        isProfileOwner =
            loggedIn.accountName == profile.account

        isCommunityAdmin =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    community.creator == loggedIn.accountName

                _ ->
                    False
    in
    div
        [ class "bg-gray-100 w-full md:flex md:flex-col" ]
        [ div
            [ class "md:flex-basis-0 md:flex-grow-1 md:overflow-y-auto"
            , id "transfer-scroll-container"
            , tabindex -1
            ]
            [ div [ class "w-full bg-white md:bg-gray-100" ]
                [ div [ class "px-4" ]
                    [ ul [ class "container mx-auto divide-y divide-gray-500 w-full mb-4 bg-white md:bg-gray-100" ]
                        [ viewDetailsItem
                            (text_ "profile.locations")
                            (text (profile.localization |> Maybe.withDefault ""))
                            Center
                        , case profile.address of
                            Just address ->
                                viewAddress loggedIn.shared.translators address

                            Nothing ->
                                text ""
                        , viewDetailsItem (text_ "profile.interests")
                            (text (String.join ", " profile.interests))
                            Top
                        , if isProfileOwner then
                            viewDetailsItem (text_ "contact_form.options")
                                (a
                                    [ class "button-secondary button-sm uppercase cursor-pointer"
                                    , Route.href Route.ProfileAddContact
                                    ]
                                    [ if List.isEmpty profile.contacts then
                                        text_ "menu.add"

                                      else
                                        text_ "menu.edit"
                                    ]
                                )
                                Center

                          else
                            text ""
                        , if isProfileOwner then
                            case profile.kyc of
                                Just kyc ->
                                    viewKycInfo kyc

                                Nothing ->
                                    text ""

                          else
                            text ""
                        ]
                    ]
                ]
            , if isProfileOwner || isCommunityAdmin then
                viewHistory loggedIn.shared balance graphqlInfo model.contributionInfo

              else
                text ""
            , if isProfileOwner || isCommunityAdmin then
                viewLatestTransactions loggedIn model

              else
                text ""
            ]
        ]


viewDetailsItem : Html msg -> Html msg -> VerticalAlign -> Html msg
viewDetailsItem label content verticalAlign =
    li [ class "py-4" ]
        [ div
            [ class "flex justify-between"
            , classList
                [ ( "items-start", verticalAlign == Top )
                , ( "items-center", verticalAlign == Center )
                ]
            ]
            [ span [ class "text-sm mr-4" ]
                [ label ]
            , span [ class "text-indigo-500 font-semibold text-sm text-right" ]
                [ content ]
            ]
        ]


viewAddress : Translators -> Profile.Address.Address -> Html msg
viewAddress { t } address =
    let
        fullAddress =
            span []
                [ [ address.country
                  , address.state
                  , address.city
                  , address.neighborhood
                  , address.street
                  , address.number
                  ]
                    |> List.filter (not << String.isEmpty)
                    |> String.join ", "
                    |> text
                , br [] []
                , text address.zip
                ]
    in
    viewDetailsItem (text <| t "Address")
        fullAddress
        Top


viewKycInfo : Kyc.ProfileKyc -> Html msg
viewKycInfo kyc =
    let
        documentLabel =
            case kyc.documentType of
                "cedula_de_identidad" ->
                    "Cédula de identidad"

                "dimex" ->
                    "DIMEX"

                "nite" ->
                    "NITE"

                "gran_empresa" ->
                    "Gran Empresa"

                "mipyme" ->
                    "MIPYME"

                _ ->
                    "Unknown Document"
    in
    viewDetailsItem (text documentLabel)
        (text kyc.document)
        Center


viewLatestTransactions : LoggedIn.Model -> Model -> Html Msg
viewLatestTransactions loggedIn model =
    let
        text_ =
            text << loggedIn.shared.translators.t

        viewInfiniteList : Maybe Api.Relay.PageInfo -> List (Html Msg) -> Html Msg
        viewInfiniteList maybePageInfo children =
            let
                viewList isMobile =
                    View.Components.infiniteList
                        { onRequestedItems =
                            maybePageInfo
                                |> Maybe.andThen
                                    (\pageInfo ->
                                        if pageInfo.hasNextPage then
                                            Just RequestedMoreTransfers

                                        else
                                            Nothing
                                    )
                        , distanceToRequest = 1000
                        , elementToTrack =
                            if isMobile then
                                View.Components.TrackWindow

                            else
                                View.Components.TrackSelector "#transfer-scroll-container"
                        }
                        [ class "w-full mt-2 divide-y divide-gray-500"
                        , classList
                            [ ( "hidden md:block", not isMobile )
                            , ( "md:hidden", isMobile )
                            ]
                        ]
                        children
            in
            div []
                [ viewList True
                , viewList False
                ]
    in
    div [ class "p-4 bg-white max-w-screen md:px-3 md:bg-transparent" ]
        [ div [ class "container mx-auto w-full" ]
            [ p [ class "text-lg" ]
                [ span [ class "text-gray-900 font-light" ] [ text_ "transfer.transfers_latest" ]
                , text " "
                , span [ class "text-indigo-500 font-semibold" ] [ text_ "transfer.transfers" ]
                ]
            , case model.transfersStatus of
                FailedLoading ->
                    Page.viewCardEmpty
                        [ div [ class "text-gray-900 text-sm" ]
                            [ text_ "transfer.loading_error" ]
                        ]

                Loading transfers ->
                    viewInfiniteList Nothing
                        (viewTransactionList loggedIn transfers
                            ++ [ View.Components.loadingLogoAnimated loggedIn.shared.translators "" ]
                        )

                Loaded transfers maybePageInfo ->
                    viewTransactionList loggedIn transfers
                        |> viewInfiniteList maybePageInfo
            ]
        ]


viewTransactionList : LoggedIn.Model -> List ( Transfer.Transfer, Profile.Summary.Model ) -> List (Html Msg)
viewTransactionList loggedIn transfers =
    transfers
        |> List.groupWhile
            (\( t1, _ ) ( t2, _ ) ->
                Utils.areSameDay loggedIn.shared.timezone
                    (Utils.fromDateTime t1.blockTime)
                    (Utils.fromDateTime t2.blockTime)
            )
        |> List.map
            (\( ( t1, _ ) as first, rest ) ->
                div []
                    [ div [ class "mt-4" ]
                        [ View.Components.dateViewer
                            [ class "uppercase text-sm text-black tracking-wide" ]
                            identity
                            loggedIn.shared
                            (Utils.fromDateTime t1.blockTime)
                        ]
                    , first
                        :: rest
                        |> List.map
                            (\( transfer, profileSummary ) ->
                                Transfer.view loggedIn
                                    transfer
                                    (profileSummary
                                        |> Profile.Summary.withRelativeSelector "#transfer-relative-container"
                                        |> Profile.Summary.withScrollSelector "#transfer-scroll-container"
                                    )
                                    (GotTransferCardProfileSummaryMsg transfer.id)
                                    (ClickedTransferCard transfer.id)
                                    [ class "md:hover:bg-gray-200" ]
                            )
                        |> div [ class "divide-y divide-gray-500" ]
                    ]
            )


viewHistory : Shared -> Community.Balance -> GraphqlInfo -> Maybe ContributionInfo -> Html msg
viewHistory shared balance graphqlInfo maybeContributionInfo =
    let
        { t } =
            shared.translators

        text_ =
            text << t

        viewHistoryItem title number translation =
            li [ class "flex items-center py-4 text-sm" ]
                [ div [ class "flex items-center" ]
                    title
                , div [ class "ml-auto" ]
                    [ span [ class "mr-1 text-indigo-500 font-bold" ]
                        [ number ]
                    , span [ class "text-sm text-gray-900 uppercase" ]
                        [ translation ]
                    ]
                ]
    in
    div [ class "bg-white px-4 mb-6 md:bg-gray-100" ]
        [ ul [ class "container mx-auto w-full self-center divide-y divide-gray-500" ]
            [ viewHistoryItem
                [ Icons.coin "mr-2"
                , span [] [ text_ "profile.history.balance" ]
                ]
                (span [ class "text-3xl mr-1" ]
                    [ balance.asset.amount
                        |> Eos.formatSymbolAmount shared.translators balance.asset.symbol
                        |> text
                    ]
                )
                (balance.asset.symbol
                    |> Eos.symbolToSymbolCodeString
                    |> text
                )
            , case maybeContributionInfo of
                Nothing ->
                    text ""

                Just contributionInfo ->
                    viewHistoryItem [ text_ "dashboard.my_contributions" ]
                        (Utils.formatFloat (Just shared.translators) 2 contributionInfo.amount
                            |> text
                        )
                        (Community.currencyTranslationKey contributionInfo
                            |> text_
                        )
            , viewHistoryItem [ text_ "profile.history.transfers_number" ]
                (graphqlInfo.totalTransfers
                    |> String.fromInt
                    |> text
                )
                (text_ "profile.history.transfers")
            , viewHistoryItem [ text_ "profile.history.claims_number" ]
                (graphqlInfo.totalClaims
                    |> String.fromInt
                    |> text
                )
                (text_ "profile.history.claims")
            , viewHistoryItem [ text_ "profile.history.products" ]
                (graphqlInfo.totalProducts
                    |> String.fromInt
                    |> text
                )
                (text_ "profile.history.items")
            , case graphqlInfo.createdDate of
                Just createdDate ->
                    viewHistoryItem [ text_ "profile.history.registration_date" ]
                        (View.Components.dateViewer [ class "capitalize -mr-1" ] identity shared createdDate)
                        (text "")

                Nothing ->
                    text ""
            , viewHistoryItem [ text_ "profile.history.last_transaction" ]
                (View.Components.dateViewer [ class "capitalize -mr-1" ] identity shared balance.lastActivity)
                (text "")
            ]
        ]



-- GRAPHQL


graphqlInfoQuery : Eos.Name -> Eos.Symbol -> SelectionSet (Maybe GraphqlInfo) RootQuery
graphqlInfoQuery profileName communitySymbol =
    Cambiatus.Query.user
        { account = Eos.nameToString profileName }
        (graphqlInfoSelectionSet communitySymbol)


graphqlInfoSelectionSet : Eos.Symbol -> SelectionSet GraphqlInfo Cambiatus.Object.User
graphqlInfoSelectionSet communitySymbol =
    SelectionSet.succeed GraphqlInfo
        |> SelectionSet.with
            (User.transfers
                (\optionals ->
                    { optionals
                        | first = Present 0
                        , filter =
                            Present
                                { communityId = communitySymbol |> Eos.symbolToString |> Present
                                , date = Absent
                                , direction = Absent
                                }
                    }
                )
                transfersSelectionSet
                |> SelectionSet.map (Maybe.withDefault 0)
            )
        |> SelectionSet.with
            (User.claims identity claimSelectionSet
                |> SelectionSet.map List.length
            )
        |> SelectionSet.with
            (User.products productSelectionSet
                |> SelectionSet.map
                    (\products ->
                        products
                            |> List.filterMap identity
                            |> List.filter (\{ symbol } -> symbol == communitySymbol)
                            |> List.length
                    )
            )
        |> SelectionSet.with
            (User.network networkSelectionSet
                |> SelectionSet.map
                    (\networks ->
                        networks
                            |> Maybe.withDefault []
                            |> List.filterMap identity
                            |> List.find (\network -> network.symbol == communitySymbol)
                            |> Maybe.map .createdAt
                    )
            )


transfersSelectionSet : SelectionSet Int Cambiatus.Object.TransferConnection
transfersSelectionSet =
    SelectionSet.succeed identity
        |> SelectionSet.with
            (Cambiatus.Object.TransferConnection.count
                |> SelectionSet.map (Maybe.withDefault 0)
            )


claimSelectionSet : SelectionSet { id : Int } Cambiatus.Object.Claim
claimSelectionSet =
    SelectionSet.succeed (\id -> { id = id })
        |> SelectionSet.with Cambiatus.Object.Claim.id


productSelectionSet : SelectionSet { id : Int, symbol : Eos.Symbol } Cambiatus.Object.Product
productSelectionSet =
    SelectionSet.succeed (\id symbol -> { id = id, symbol = symbol })
        |> SelectionSet.with Cambiatus.Object.Product.id
        |> SelectionSet.with (Eos.symbolSelectionSet Cambiatus.Object.Product.communityId)


networkSelectionSet : SelectionSet Network Cambiatus.Object.Network
networkSelectionSet =
    SelectionSet.succeed Network
        |> SelectionSet.with (Eos.symbolSelectionSet Cambiatus.Object.Network.communityId)
        |> SelectionSet.with
            (Cambiatus.Object.Network.createdAt
                |> SelectionSet.map Utils.fromDateTime
            )


fetchTransfers : LoggedIn.Model -> Community.Model -> Maybe String -> LoggedIn.External Msg
fetchTransfers loggedIn community maybeCursor =
    LoggedIn.query loggedIn
        (Transfer.transfersUserQuery
            loggedIn.accountName
            (\args ->
                { args
                    | first = Present 10
                    , after = OptionalArgument.fromMaybe maybeCursor
                    , filter =
                        Present
                            { communityId = Present (Eos.symbolToString community.symbol)
                            , date = Absent
                            , direction = Absent
                            }
                }
            )
        )
        CompletedLoadUserTransfers



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.ProfileLoaded profile ->
            Just profile
                |> RemoteData.Success
                |> CompletedLoadProfile
                |> Just

        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        LoggedIn.CommunityFieldLoaded _ (Community.ContributionsValue contributions) ->
            Just (CompletedLoadContributions contributions)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadProfile r ->
            [ "CompletedLoadProfile", UR.remoteDataToString r ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        CompletedLoadContributions _ ->
            [ "CompletedLoadContributions" ]

        CompletedLoadBalance r ->
            [ "CompletedLoadBalance", UR.resultToString r ]

        CompletedLoadGraphqlInfo r ->
            [ "CompletedLoadGraphqlInfo", UR.remoteDataToString r ]

        CompletedLoadUserTransfers r ->
            [ "CompletedLoadUserTransfers", UR.remoteDataToString r ]

        GotTransferCardProfileSummaryMsg _ subMsg ->
            "GotTransferCardProfileSummaryMsg" :: Profile.Summary.msgToString subMsg

        ClickedTransferCard _ ->
            [ "ClickedTransferCard" ]

        RequestedMoreTransfers ->
            [ "RequestedMoreTransfers" ]
