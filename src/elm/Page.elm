module Page exposing
    ( ExternalMsg(..)
    , Msg(..)
    , Session(..)
    , fullPageError
    , fullPageGraphQLError
    , fullPageLoading
    , fullPageNotFound
    , init
    , jsAddressToMsg
    , logout
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
import Flags exposing (Flags)
import Graphql.Http
import Html exposing (Html, a, div, img, p, text)
import Html.Attributes exposing (class, src, title)
import Http
import I18Next exposing (Delims(..), Translations)
import Icons
import Json.Encode exposing (Value)
import Ports
import RemoteData exposing (RemoteData)
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared exposing (Shared)
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
                |> UR.addCmd (fetchTranslations shared shared.language)

        ( Just ( accountName, _ ), Nothing ) ->
            let
                ( model, cmd, signedInCmd ) =
                    Guest.initLoggingIn shared accountName SignedIn
            in
            Guest model
                |> UR.init
                |> UR.addCmd (Cmd.map GotGuestMsg cmd)
                |> UR.addCmd (fetchTranslations shared shared.language)
                |> UR.addCmd signedInCmd

        ( Nothing, _ ) ->
            let
                ( model, cmd ) =
                    Guest.init shared
            in
            UR.init (Guest model)
                |> UR.addCmd (Cmd.map GotGuestMsg cmd)
                |> UR.addCmd (fetchTranslations shared shared.language)


fetchTranslations : Shared -> String -> Cmd Msg
fetchTranslations _ language =
    CompletedLoadTranslation language
        |> Translation.get language



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
        [ class "heading-bold" ]
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


type ExternalMsg
    = LoggedInExternalMsg LoggedIn.ExternalMsg
    | GuestBroadcastMsg Guest.BroadcastMsg


type Msg
    = CompletedLoadTranslation String (Result Http.Error Translations)
    | SignedIn (RemoteData (Graphql.Http.Error (Maybe Auth.SignInResponse)) (Maybe Auth.SignInResponse))
    | GotGuestMsg Guest.Msg
    | GotLoggedInMsg LoggedIn.Msg


update : Msg -> Session -> UpdateResult
update msg session =
    case ( msg, session ) of
        ( CompletedLoadTranslation lang (Ok transl), _ ) ->
            Shared.loadTranslation (Ok ( lang, transl ))
                |> updateShared session
                |> UR.init
                |> UR.addCmd (Ports.storeLanguage lang)

        ( CompletedLoadTranslation _ (Err err), _ ) ->
            Shared.loadTranslation (Err err)
                |> updateShared session
                |> UR.init
                |> UR.logHttpError msg err

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

        ( SignedIn (RemoteData.Failure error), Guest guest ) ->
            UR.init session
                |> UR.addCmd (Route.replaceUrl guest.shared.navKey (Route.Login guest.maybeInvitation guest.afterLoginRedirect))
                |> UR.logGraphqlError msg error

        ( _, _ ) ->
            UR.init session
                |> UR.logImpossible msg []


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
        CompletedLoadTranslation _ r ->
            [ "CompletedLoadTranslation", UR.resultToString r ]

        SignedIn r ->
            [ "SignedIn", UR.remoteDataToString r ]

        GotGuestMsg subMsg ->
            "GotGuestMsg" :: Guest.msgToString subMsg

        GotLoggedInMsg subMsg ->
            "GotLoggedInMsg" :: LoggedIn.msgToString subMsg
