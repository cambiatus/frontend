module Page exposing
    ( ExternalMsg(..)
    , Msg(..)
    , Session(..)
    , errorToString
    , fullPageError
    , fullPageGraphQLError
    , fullPageLoading
    , fullPageNotFound
    , init
    , isLoggedIn
    , jsAddressToMsg
    , labelWithTooltip
    , loading
    , login
    , logout
    , msgToString
    , onFileChange
    , subscriptions
    , toShared
    , update
    , viewButtonNew
    , viewCardEmpty
    , viewCardList
    , viewDateDistance
    , viewGuest
    , viewHeader
    , viewLoggedIn
    , viewMaxTwoColumn
    , viewMenuFilter
    , viewMenuFilterButton
    , viewMenuFilterTabButton
    , viewMenuTab
    , viewTitle
    )

import Api.Graphql
import Asset.Icon as Icon
import Auth
import Browser.Navigation as Nav
import DateDistance
import File exposing (File)
import Flags exposing (Flags)
import Graphql.Http
import Graphql.Http.GraphqlError
import Html exposing (Attribute, Html, a, br, button, div, img, label, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, for, src, title, type_, value)
import Html.Events exposing (on)
import Http
import I18Next exposing (Delims(..), Translations)
import Icons
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Ports
import Profile
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared exposing (Shared)
import Time exposing (Posix)
import Translation
import UpdateResult as UR
import Url exposing (Url)
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
                    LoggedIn.init shared accountName flags authToken
            in
            UR.init (LoggedIn model)
                |> UR.addCmd (Cmd.map GotLoggedInMsg cmd)
                |> UR.addCmd (fetchTranslations shared shared.language)

        ( Just ( accountName, _ ), Nothing ) ->
            let
                ( model, cmd ) =
                    Guest.initLoggingIn shared
            in
            Guest model
                |> UR.init
                |> UR.addCmd (Cmd.map GotGuestMsg cmd)
                |> UR.addCmd (fetchTranslations shared shared.language)
                |> UR.addCmd
                    (Api.Graphql.mutation shared
                        Nothing
                        (Auth.signIn accountName shared Nothing)
                        SignedIn
                    )

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


onClick : (a -> msg) -> Decoder a -> Html.Attribute msg
onClick toMsg decoder =
    on "click" (Decode.map toMsg decoder)


onFileChange : (List File -> msg) -> Attribute msg
onFileChange toMsg =
    Decode.list File.decoder
        |> Decode.at [ "target", "files" ]
        |> Decode.map toMsg
        |> on "change"


viewMenuFilter : List (Html msg) -> Html msg
viewMenuFilter buttons =
    div
        [ class "menu-filter__buttons sm:hidden md:flex" ]
        buttons


viewMenuFilterButton : Bool -> String -> Route -> Html msg
viewMenuFilterButton isActive text_ route =
    a
        [ classList
            [ ( "menu-filter__button"
              , True
              )
            , ( "menu-filter__button-active"
              , isActive
              )
            ]
        , Route.href route
        , title text_
        ]
        [ text text_ ]


viewMenuTab : List (Html msg) -> Html msg
viewMenuTab buttons =
    div
        [ class "flex justify-center" ]
        buttons


viewMenuFilterTabButton : Bool -> (a -> msg) -> Decoder a -> String -> Html msg
viewMenuFilterTabButton isActive toMsg decoder text_ =
    if isActive then
        if String.startsWith "All offers" text_ then
            button [ class "bg-purple-500 border border-purple-500 rounded-l px-12 py-2 text-white", value text_, onClick toMsg decoder ]
                [ text text_ ]

        else
            button [ class "bg-purple-500 border border-purple-500 rounded-r px-12 py-2 text-white", value text_, onClick toMsg decoder ]
                [ text text_ ]

    else if String.startsWith "All offers" text_ then
        button [ class "border border-purple-500 rounded-l px-16 py-2 text-gray", value text_, onClick toMsg decoder ]
            [ text text_ ]

    else
        button [ class "border border-purple-500 rounded-r px-16 py-2 text-gray", value text_, onClick toMsg decoder ]
            [ text text_ ]


viewCardList : List ( List (Html msg), Posix, Maybe Posix ) -> Html msg
viewCardList items =
    let
        items_ =
            List.map
                (\( content, date, maybeNow ) ->
                    li []
                        [ span [ class "card__list-text" ] content
                        , span [ class "card__list-date" ]
                            (viewDateDistance date maybeNow)
                        ]
                )
                items
    in
    div [ class "shadow-md rounded-lg bg-white" ]
        [ ul [] items_
        , div [ class "card__button-row" ]
            [ button [ class "btn btn--primary" ]
                [ text "See More" ]
            ]
        ]


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


viewHeader : LoggedIn.Model -> String -> Route -> Html msg
viewHeader { shared } title route =
    div [ class "w-full h-16 flex px-4 items-center bg-indigo-500" ]
        [ div [ class "flex container mx-auto" ]
            [ a
                [ class "flex items-center mr-4"
                , Route.href route
                ]
                [ Icons.back ""
                , p [ class "ml-2 text-white text-sm hidden md:visible md:flex" ]
                    [ text (shared.translators.t "back") ]
                ]
            , p [ class "mx-auto text-white truncate ..." ] [ text title ]
            ]
        ]


viewButtonNew : String -> Route -> Html msg
viewButtonNew title_ route =
    a
        [ class "btn create-button my-3"
        , title title_
        , Route.href route
        ]
        [ text title_ ]


viewMaxTwoColumn : List (Html msg) -> List (Html msg) -> Html msg
viewMaxTwoColumn firstColContent secColContent =
    div [ class "section-grid mt-4" ]
        [ div [ class "section-grid__section" ] firstColContent
        , div [ class "section-grid__section" ] secColContent
        ]


labelWithTooltip : String -> String -> String -> Html msg
labelWithTooltip for_ text_ tooltipText =
    label [ for for_ ]
        [ div [ class "tooltip__text" ]
            [ text text_
            , button
                [ class "tooltip"
                , type_ "button"
                , attribute "tooltip" tooltipText
                ]
                [ Icon.helpCircle "" ]
            ]
        ]


viewDateDistance : Posix -> Maybe Posix -> List (Html msg)
viewDateDistance date maybeNow =
    case maybeNow of
        Just now ->
            [ text (DateDistance.viewDateDistance date now)
            , br [] []
            , text "ago"
            ]

        Nothing ->
            []


fullPageLoading : Shared.Shared -> Html msg
fullPageLoading { translators } =
    View.Components.loadingLogoAnimated translators


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
            , p [ class "text-center" ] [ text (errorToString e) ]
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


loading : Html msg
loading =
    div [ class "spinner spinner--delay" ] []


errorToString : Graphql.Http.Error parsedData -> String
errorToString errorData =
    case errorData of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            graphqlErrors
                |> List.map graphqlErrorToString
                |> String.join "\n"

        Graphql.Http.HttpError _ ->
            "Http Error"


graphqlErrorToString : Graphql.Http.GraphqlError.GraphqlError -> String
graphqlErrorToString error =
    error.message



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Session Msg ExternalMsg


type ExternalMsg
    = LoggedInExternalMsg LoggedIn.ExternalMsg


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
                |> UR.map Guest GotGuestMsg (\() uR -> uR)

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
                    LoggedIn.initLogin shared (Auth.init shared) user token
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
                |> UR.addCmd (Route.replaceUrl guest.shared.navKey (Route.Login guest.afterLoginRedirect))
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


login : Auth.Model -> Profile.Model -> Guest.Model -> String -> ( LoggedIn.Model, Cmd Msg )
login auth profile guest authToken =
    let
        ( loggedIn, cmd ) =
            LoggedIn.initLogin guest.shared auth profile authToken
    in
    ( loggedIn
    , Cmd.map GotLoggedInMsg cmd
    )


logout : LoggedIn.Model -> ( Session, Cmd Msg )
logout { shared } =
    ( Guest (Guest.initModel { shared | maybeAccount = Nothing })
    , Route.replaceUrl shared.navKey (Route.Login Nothing)
    )



-- INFO


isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        LoggedIn _ ->
            True

        Guest _ ->
            False


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
