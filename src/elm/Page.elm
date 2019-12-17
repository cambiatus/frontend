module Page exposing (ExternalMsg(..), Msg, Session(..), errorToString, fullPageError, fullPageGraphQLError, fullPageLoading, fullPageNotFound, init, isLoggedIn, jsAddressToMsg, labelWithTooltip, loading, login, logout, msgToString, onFileChange, subscriptions, toShared, update, viewButtonNew, viewCardEmpty, viewCardList, viewDateDistance, viewGuest, viewLoggedIn, viewMaxTwoColumn, viewMenuFilter, viewMenuFilterButton, viewMenuFilterDropdown, viewMenuFilterDropdownOption, viewTitle, viewMenuTab, viewMenuFilterTabButton)

import Account exposing (Profile)
import Asset.Icon as Icon
import Auth
import Browser.Navigation as Nav
import Community exposing (ActionVerification)
import DateDistance
import File exposing (File)
import Flags exposing (Flags)
import Graphql.Http
import Graphql.Http.GraphqlError
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Http
import I18Next exposing (Delims(..), Translations)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Ports
import Route exposing (Route)
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared exposing (Shared)
import Time exposing (Posix)
import Translation
import UpdateResult as UR



-- INIT


init : Flags -> Nav.Key -> UpdateResult
init flags navKey =
    let
        shared =
            Shared.init flags navKey
    in
    case shared.maybeAccount of
        Nothing ->
            let
                ( model, cmd ) =
                    Guest.init shared
            in
            UR.init (Guest model)
                |> UR.addCmd (Cmd.map GotGuestMsg cmd)
                |> UR.addCmd (fetchTranslations shared shared.language)
                |> UR.addPort
                    { responseAddress = GotScatterAvailability False
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "checkScatterAvailability" ) ]
                    }

        Just ( accountName, _ ) ->
            let
                ( model, cmd ) =
                    LoggedIn.init shared accountName
            in
            UR.init (LoggedIn model)
                |> UR.addCmd (Cmd.map GotLoggedInMsg cmd)
                |> UR.addCmd (fetchTranslations shared shared.language)
                |> UR.addPort
                    { responseAddress = GotScatterAvailability False
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "checkScatterAvailability" ) ]
                    }


fetchTranslations : Shared -> String -> Cmd Msg
fetchTranslations shared language =
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


onChange : (a -> msg) -> Decoder a -> Html.Attribute msg
onChange toMsg decoder =
    on "change" (Decode.map toMsg decoder)

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


viewMenuFilterDropdown : (a -> msg) -> Decoder a -> List (Html msg) -> Html msg
viewMenuFilterDropdown toMsg decoder options =
    div
        [ class "flex flex-row justify-between" ]
        [ select
            [ class "form-select sm:w-full md:w-64 select border-gray-500"
            , onChange toMsg decoder
            ]
            options
        ]


viewMenuTab : List (Html msg) -> Html msg
viewMenuTab buttons =
    div
        [ class "filter-tab" ]
        buttons


viewMenuFilterTabButton : Bool -> (a -> msg) -> Decoder a -> String -> Html msg
viewMenuFilterTabButton isActive toMsg decoder text_ =
    case isActive of
        True ->
            button [ class "filter-tab--button filter-tab--button__active", value text_, onClick toMsg decoder ]
                [ text text_ ]

        False ->
            button [ class "filter-tab--button", value text_, onClick toMsg decoder ]
                [ text text_ ]

viewMenuFilterDropdownOption : Bool -> String -> Html msg
viewMenuFilterDropdownOption isSelected text_ =
    option
        [ class "menu-filter__dropdown-option"
        , selected isSelected
        ]
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
    div [ class "rounded-lg bg-white p-4" ]
        [ span [] content ]


viewTitle : String -> Html msg
viewTitle text_ =
    p
        [ class "heading-bold" ]
        [ text text_ ]


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


fullPageLoading : Html msg
fullPageLoading =
    div [ class "full-spinner-container h-full" ]
        [ div [ class "spinner spinner--delay" ] [] ]


fullPageError : String -> Http.Error -> Html msg
fullPageError title_ e =
    div []
        [ viewTitle title_
        , div [ class "card" ] [ text "Something wrong happened." ]
        ]


fullPageGraphQLError : String -> Graphql.Http.Error a -> Html msg
fullPageGraphQLError title_ e =
    div [ class "mx-auto container p-24 flex flex-wrap" ]
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

        Graphql.Http.HttpError httpError ->
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
    = GotScatterAvailability Bool
    | CompletedLoadTranslation String (Result Http.Error Translations)
    | GotGuestMsg Guest.Msg
    | GotLoggedInMsg LoggedIn.Msg


update : Msg -> Session -> UpdateResult
update msg session =
    case ( msg, session ) of
        ( GotScatterAvailability isAvailable, _ ) ->
            Shared.gotScatterAvailability isAvailable
                |> updateShared session
                |> UR.init

        ( CompletedLoadTranslation lang (Ok transl), _ ) ->
            Shared.loadTranslation (Ok ( lang, transl ))
                |> updateShared session
                |> UR.init
                |> UR.addCmd (Ports.storeLanguage lang)

        ( CompletedLoadTranslation lang (Err err), _ ) ->
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


login : Auth.Model -> Profile -> Guest.Model -> ( LoggedIn.Model, Cmd Msg )
login auth profile guest =
    let
        ( loggedIn, cmd ) =
            LoggedIn.initLogin guest.shared auth profile
    in
    ( loggedIn
    , Cmd.map GotLoggedInMsg cmd
    )


logout : LoggedIn.Model -> ( Session, Cmd Msg )
logout ({ shared } as loggedIn) =
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
        "GotScatterAvailability" :: [] ->
            Decode.decodeValue
                (Decode.field "isAvailable" Decode.bool)
                val
                |> Result.map GotScatterAvailability
                |> Result.toMaybe

        "GotLoggedInMsg" :: remainAddress ->
            LoggedIn.jsAddressToMsg remainAddress val
                |> Maybe.map GotLoggedInMsg

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        GotScatterAvailability _ ->
            [ "GotScatterAvailability" ]

        CompletedLoadTranslation _ r ->
            [ "CompletedLoadTranslation", UR.resultToString r ]

        GotGuestMsg subMsg ->
            "GotGuestMsg" :: Guest.msgToString subMsg

        GotLoggedInMsg subMsg ->
            "GotLoggedInMsg" :: LoggedIn.msgToString subMsg
