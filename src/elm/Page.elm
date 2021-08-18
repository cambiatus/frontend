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

import Auth
import Browser.Navigation as Nav
import Dict
import Eos.Account
import Flags exposing (Flags)
import Graphql.Http
import Html exposing (Html, a, div, img, p, text)
import Html.Attributes exposing (class, src, title)
import Http
import I18Next exposing (Delims(..), Translations)
import Icons
import Json.Encode exposing (Value)
import Log
import Ports
import RemoteData exposing (RemoteData)
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared exposing (Shared)
import Task
import Time
import Translation
import UpdateResult as UR
import Url exposing (Url)
import Utils
import View.Components



-- INIT


init : Flags -> Nav.Key -> Url -> UpdateResult
init flags navKey url =
    let
        shared =
            Shared.init flags navKey url
    in
    case ( shared.maybeAccount, flags.authToken ) of
        ( Just ( accountName, _ ), Just authToken ) ->
            let
                ( model, cmd ) =
                    LoggedIn.init shared accountName authToken
            in
            UR.init (LoggedIn model)
                |> UR.addCmd (Cmd.map GotLoggedInMsg cmd)
                |> UR.addCmd (fetchTranslations shared.language)
                |> UR.addCmd fetchTimezone
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = Ignored
                    , message = "Started Elm app with loggedIn user"
                    , data = Dict.empty
                    , level = Log.DebugLevel
                    }

        ( Just ( accountName, _ ), Nothing ) ->
            let
                ( model, cmd, signedInCmd ) =
                    Guest.initLoggingIn shared accountName SignedIn
            in
            Guest model
                |> UR.init
                |> UR.addCmd (Cmd.map GotGuestMsg cmd)
                |> UR.addCmd (fetchTranslations shared.language)
                |> UR.addCmd fetchTimezone
                |> UR.addCmd signedInCmd
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = Ignored
                    , message = "Started Elm app with guest logging in"
                    , data = Dict.empty
                    , level = Log.DebugLevel
                    }

        ( Nothing, _ ) ->
            let
                ( model, cmd ) =
                    Guest.init shared
            in
            UR.init (Guest model)
                |> UR.addCmd (Cmd.map GotGuestMsg cmd)
                |> UR.addCmd (fetchTranslations shared.language)
                |> UR.addCmd fetchTimezone
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = Ignored
                    , message = "Started Elm app with regular guest user"
                    , data = Dict.empty
                    , level = Log.DebugLevel
                    }


fetchTranslations : Translation.Language -> Cmd Msg
fetchTranslations language =
    CompletedLoadTranslation language
        |> Translation.get language


fetchTimezone : Cmd Msg
fetchTimezone =
    Time.here
        |> Task.attempt GotTimezone



-- SUBSCRIPTIONS


subscriptions : Session -> Sub Msg
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


viewGuest : (Msg -> msg) -> Guest.Page -> Guest.Model -> Html msg -> Html msg
viewGuest thisMsg page model content =
    Guest.view (thisMsg << GotGuestMsg) page model content


viewLoggedIn : (Msg -> msg) -> LoggedIn.Page -> LoggedIn.Model -> Html msg -> Html msg
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
    div [ class "w-full h-16 flex px-4 items-center bg-indigo-500" ]
        [ div [ class "flex container mx-auto" ]
            [ a
                [ class "flex items-center mr-4"
                , routeHistory
                    |> List.drop 1
                    |> List.head
                    |> Maybe.withDefault Route.Dashboard
                    |> Route.href
                ]
                [ Icons.back ""
                , p [ class "ml-2 text-white text-sm hidden md:visible md:flex" ]
                    [ text (shared.translators.t "back") ]
                ]
            , p [ class "mx-auto text-white truncate ..." ] [ text title ]
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
            , p [ class "text-center" ] [ text (Utils.errorToString e) ]
            ]
        , img [ class "w-full", src "/images/error.svg" ] []
        ]


fullPageNotFound : String -> String -> Html msg
fullPageNotFound title subTitle =
    div [ class "mx-auto container p-24 flex flex-wrap" ]
        [ div [ class "w-full" ]
            [ p [ class "text-2xl font-bold text-center" ] [ text title ]
            , p [ class "text-center" ] [ text subTitle ]
            ]
        , img [ class "w-full", src "/images/not_found.svg" ] []
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Session Msg ExternalMsg


{-| External msg for the `UpdateResult` produced by `Page.update`
-}
type ExternalMsg
    = LoggedInExternalMsg LoggedIn.ExternalMsg
    | GuestBroadcastMsg Guest.BroadcastMsg


{-| External msg for pages to produce when they can be viewed by a logged in
user and by a guest user
-}
type External msg
    = LoggedInExternal (LoggedIn.External msg)
    | GuestExternal Guest.External


type Msg
    = Ignored
    | CompletedLoadTranslation Translation.Language (Result Http.Error Translations)
    | GotTimezone (Result () Time.Zone)
    | SignedIn (RemoteData (Graphql.Http.Error (Maybe Auth.SignInResponse)) (Maybe Auth.SignInResponse))
    | GotGuestMsg Guest.Msg
    | GotLoggedInMsg LoggedIn.Msg


update : Msg -> Session -> UpdateResult
update msg session =
    case ( msg, session ) of
        ( Ignored, _ ) ->
            UR.init session

        ( CompletedLoadTranslation lang (Ok transl), _ ) ->
            Shared.loadTranslation (Ok ( lang, transl ))
                |> updateShared session
                |> UR.init
                |> UR.addCmd (Ports.storeLanguage (Translation.languageToLocale lang))

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

        ( SignedIn (RemoteData.Success (Just { user, token })), Guest guest ) ->
            let
                shared =
                    guest.shared

                ( loggedIn, cmd ) =
                    LoggedIn.initLogin shared Nothing user token
            in
            LoggedIn loggedIn
                |> UR.init
                |> UR.addCmd (Cmd.map GotLoggedInMsg cmd)
                |> UR.addCmd (Ports.storeAuthToken token)
                |> UR.addCmd
                    (guest.afterLoginRedirect
                        |> Maybe.withDefault Route.Dashboard
                        |> Route.replaceUrl shared.navKey
                    )
                |> UR.addBreadcrumb
                    { type_ = Log.InfoBreadcrumb
                    , category = msg
                    , message = "User logged in"
                    , data = Dict.fromList [ ( "username", Eos.Account.encodeName user.account ) ]
                    , level = Log.Info
                    }

        ( SignedIn (RemoteData.Failure error), Guest guest ) ->
            UR.init session
                |> UR.addCmd (Route.replaceUrl guest.shared.navKey (Route.Login guest.maybeInvitation guest.afterLoginRedirect))
                |> UR.logGraphqlError msg
                    (maybeAccountName session)
                    "Got an error when trying to sign in"
                    { moduleName = "Page", function = "update" }
                    []
                    error

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


logout : LoggedIn.Model -> ( Session, Cmd Msg )
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


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "GotLoggedInMsg" :: remainAddress ->
            LoggedIn.jsAddressToMsg remainAddress val
                |> Maybe.map GotLoggedInMsg

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        CompletedLoadTranslation _ r ->
            [ "CompletedLoadTranslation", UR.resultToString r ]

        GotTimezone r ->
            [ "GotTimezone", UR.resultToString r ]

        SignedIn r ->
            [ "SignedIn", UR.remoteDataToString r ]

        GotGuestMsg subMsg ->
            "GotGuestMsg" :: Guest.msgToString subMsg

        GotLoggedInMsg subMsg ->
            "GotLoggedInMsg" :: LoggedIn.msgToString subMsg
