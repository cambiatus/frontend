module Page exposing
    ( External(..)
    , ExternalMsg(..)
    , Msg(..)
    , Session(..)
    , fullPageError
    , fullPageGraphQLError
    , fullPageLoading
    , fullPageNotFound
    , init
    , jsAddressToMsg
    , logout
    , maybeAccountName
    , msgToString
    , subscriptions
    , toShared
    , update
    , viewCardEmpty
    , viewGuest
    , viewHeader
    , viewLoggedIn
    , viewTitle
    )

import Api.Graphql
import Browser.Navigation as Nav
import Dict
import Eos.Account
import Flags exposing (Flags)
import Graphql.Http
import Html exposing (Html, a, div, h1, img, nav, p, span, text)
import Html.Attributes exposing (class, id, src)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabelledby)
import Http
import I18Next exposing (Translations)
import Icons
import Json.Encode exposing (Value)
import Log
import Ports
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared exposing (Shared)
import Task
import Time
import Translation
import UpdateResult as UR
import Url exposing (Url)
import Version exposing (Version)
import View.Components



-- INIT


init : Flags -> Nav.Key -> Url -> UpdateResult externalMsg
init flags navKey url =
    let
        ( shared, sharedCmd ) =
            Shared.init flags navKey url

        initialCmds =
            Cmd.batch
                [ fetchTranslations shared.version shared.language
                , fetchTimezone
                , sharedCmd
                ]
    in
    case shared.maybeAccount of
        Just accountName ->
            let
                ( model, cmd ) =
                    LoggedIn.init shared accountName flags.authToken
            in
            LoggedIn model
                |> UR.init
                |> UR.addCmd initialCmds
                |> UR.addCmd (Cmd.map GotLoggedInMsg cmd)
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = Ignored
                    , message = "Started Elm app with loggedIn user"
                    , data = Dict.empty
                    , level = Log.DebugLevel
                    }

        Nothing ->
            let
                ( model, cmd ) =
                    Guest.init shared
            in
            Guest model
                |> UR.init
                |> UR.addCmd initialCmds
                |> UR.addCmd (Cmd.map GotGuestMsg cmd)
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = Ignored
                    , message = "Started Elm app with guest user"
                    , data = Dict.empty
                    , level = Log.DebugLevel
                    }


fetchTranslations : Version -> Translation.Language -> Cmd (Msg externalMsg)
fetchTranslations version language =
    CompletedLoadTranslation language
        |> Translation.get version language


fetchTimezone : Cmd (Msg externalMsg)
fetchTimezone =
    Time.here
        |> Task.attempt GotTimezone



-- SUBSCRIPTIONS


subscriptions : Session -> Sub (Msg externalMsg)
subscriptions session =
    case session of
        Guest guest ->
            Guest.subscriptions guest
                |> Sub.map GotGuestMsg

        LoggedIn loggedIn ->
            LoggedIn.subscriptions loggedIn
                |> Sub.map GotLoggedInMsg



-- MODEL


type Session
    = Guest Guest.Model
    | LoggedIn LoggedIn.Model



-- VIEW


viewGuest : (Msg msg -> msg) -> Guest.Page -> Guest.Model -> Html msg -> Html msg
viewGuest thisMsg page model content =
    Guest.view (thisMsg << GotGuestMsg) page model content


viewLoggedIn : (Msg msg -> msg) -> LoggedIn.Page -> LoggedIn.Model -> Html msg -> Html msg
viewLoggedIn thisMsg page model content =
    LoggedIn.view (thisMsg << GotLoggedInMsg) page model content



-- VIEW >> HELPERS


viewCardEmpty : List (Html msg) -> Html msg
viewCardEmpty content =
    div [ class "rounded-lg bg-white mt-5 p-4" ]
        [ div
            [ class "bg-white-smoke flex items-center justify-center p-8" ]
            content
        ]


viewTitle : String -> Html msg
viewTitle text_ =
    p
        [ class "text-lg font-bold my-3" ]
        [ text text_ ]


viewHeader : LoggedIn.Model -> String -> Html msg
viewHeader { shared, routeHistory } title =
    div [ class "w-full h-16 flex items-center bg-indigo-500" ]
        [ nav [ class "flex items-center container mx-auto px-4 relative" ]
            [ a
                [ class "flex items-center sm:w-full"
                , routeHistory
                    |> List.drop 1
                    |> List.head
                    |> Maybe.withDefault Route.Dashboard
                    |> Route.href
                , ariaLabelledby "previous-page-button"
                ]
                [ Icons.back ""
                , p
                    [ class "ml-2 text-white text-sm hidden md:visible md:flex"
                    , id "previous-page-button"
                    ]
                    [ text (shared.translators.t "back") ]
                ]
            , h1 [ class "text-white truncate w-full text-center ml-1 mr-8 sm:ml-0 sm:mr-0" ]
                [ text title ]
            , span
                [ class "w-full hidden sm:inline"
                , ariaHidden True
                ]
                []
            ]
        ]


fullPageLoading : Shared -> Html msg
fullPageLoading { translators } =
    View.Components.loadingLogoAnimated translators ""


fullPageError : String -> Http.Error -> Html msg
fullPageError title_ _ =
    div []
        [ viewTitle title_
        , div [ class "card" ] [ text "Something wrong happened." ]
        ]


fullPageGraphQLError : String -> Graphql.Http.Error a -> Html msg
fullPageGraphQLError title_ e =
    div [ class "mx-auto container p-16 flex flex-wrap" ]
        [ div [ class "w-full" ]
            [ p [ class "text-2xl font-bold text-center" ] [ text title_ ]
            , p [ class "text-center" ] [ text (Api.Graphql.errorToString e) ]
            ]
        , img [ class "w-full", src "/images/error.svg" ] []
        ]


fullPageNotFound : String -> String -> Html msg
fullPageNotFound title subTitle =
    div [ class "mx-auto container p-8 md:p-24 flex flex-wrap" ]
        [ div [ class "w-full" ]
            [ p [ class "text-2xl font-bold text-center" ] [ text title ]
            , p [ class "text-center" ] [ text subTitle ]
            ]
        , img [ class "w-full md:w-auto md:mx-auto mt-4", src "/images/not_found.svg" ] []
        ]



-- UPDATE


type alias UpdateResult msg =
    UR.UpdateResult Session (Msg msg) (ExternalMsg msg)


{-| External msg for the `UpdateResult` produced by `Page.update`
-}
type ExternalMsg msg
    = LoggedInExternalMsg (LoggedIn.ExternalMsg msg)
    | GuestBroadcastMsg Guest.BroadcastMsg


{-| External msg for pages to produce when they can be viewed by a logged in
user and by a guest user
-}
type External msg
    = LoggedInExternal (LoggedIn.External msg)
    | GuestExternal Guest.External


type Msg msg
    = Ignored
    | CompletedLoadTranslation Translation.Language (Result Http.Error Translations)
    | GotTimezone (Result () Time.Zone)
    | GotGuestMsg Guest.Msg
    | GotLoggedInMsg (LoggedIn.Msg msg)


update : Msg externalMsg -> Session -> UpdateResult externalMsg
update msg session =
    case ( msg, session ) of
        ( Ignored, _ ) ->
            UR.init session

        ( CompletedLoadTranslation lang (Ok transl), _ ) ->
            Shared.loadTranslation (Ok ( lang, transl ))
                |> updateShared session
                |> UR.init
                |> UR.addCmd (Ports.storeLanguage (Translation.languageToLocale lang))
                |> UR.addExt
                    (LoggedIn.TranslationsLoaded
                        |> LoggedIn.Broadcast
                        |> LoggedInExternalMsg
                    )

        ( CompletedLoadTranslation _ (Err err), _ ) ->
            Shared.loadTranslation (Err err)
                |> updateShared session
                |> UR.init
                |> UR.logHttpError msg
                    (maybeAccountName session)
                    "Got an error when loading translations"
                    { moduleName = "Page", function = "update" }
                    []
                    err

        ( GotTimezone (Ok zone), _ ) ->
            (\shared -> { shared | timezone = zone })
                |> updateShared session
                |> UR.init

        ( GotTimezone (Err ()), _ ) ->
            UR.init session

        ( GotGuestMsg subMsg, Guest subModel ) ->
            Guest.update subMsg subModel
                |> UR.map Guest
                    GotGuestMsg
                    (\extMsg uR ->
                        UR.addExt (GuestBroadcastMsg extMsg) uR
                    )

        ( GotLoggedInMsg subMsg, LoggedIn subModel ) ->
            LoggedIn.update subMsg subModel
                |> UR.map
                    LoggedIn
                    GotLoggedInMsg
                    (\extMsg uR ->
                        UR.addExt (LoggedInExternalMsg extMsg) uR
                    )

        ( _, _ ) ->
            UR.init session
                |> UR.logIncompatibleMsg msg
                    (maybeAccountName session)
                    { moduleName = "Page"
                    , function = "update"
                    }
                    []


updateShared : Session -> (Shared -> Shared) -> Session
updateShared session transform =
    case session of
        Guest subModel ->
            Guest { subModel | shared = transform subModel.shared }

        LoggedIn subModel ->
            LoggedIn { subModel | shared = transform subModel.shared }



-- TRANSFORM


logout : LoggedIn.Model -> ( Session, Cmd (Msg externalMsg) )
logout { shared } =
    let
        ( guest, guestCmd ) =
            Guest.init shared
    in
    ( Guest { guest | shared = { shared | maybeAccount = Nothing } }
    , Cmd.batch
        [ Route.replaceUrl shared.navKey (Route.Login Nothing Nothing)
        , Cmd.map GotGuestMsg guestCmd
        ]
    )



-- INFO


toShared : Session -> Shared
toShared session =
    case session of
        LoggedIn loggedIn ->
            loggedIn.shared

        Guest guest ->
            guest.shared


maybeAccountName : Session -> Maybe Eos.Account.Name
maybeAccountName session =
    case session of
        LoggedIn loggedIn ->
            Just loggedIn.accountName

        Guest _ ->
            Nothing


jsAddressToMsg : List String -> Value -> Maybe (Msg externalMsg)
jsAddressToMsg addr val =
    case addr of
        "GotLoggedInMsg" :: remainAddress ->
            LoggedIn.jsAddressToMsg remainAddress val
                |> Maybe.map GotLoggedInMsg

        _ ->
            Nothing


msgToString : Msg externalMsg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        CompletedLoadTranslation _ r ->
            [ "CompletedLoadTranslation", UR.resultToString r ]

        GotTimezone r ->
            [ "GotTimezone", UR.resultToString r ]

        GotGuestMsg subMsg ->
            "GotGuestMsg" :: Guest.msgToString subMsg

        GotLoggedInMsg subMsg ->
            "GotLoggedInMsg" :: LoggedIn.msgToString subMsg
