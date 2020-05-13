module Page.PublicProfile exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Api.Graphql
import Avatar
import Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import I18Next exposing (t)
import Icons
import Json.Decode exposing (Value)
import Page
import Profile exposing (Profile)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Session.Shared exposing (Shared)
import UpdateResult as UR


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn accountName =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Profile.query (Eos.stringToName accountName))
                CompletedProfileLoad
    in
    ( initModel loggedIn
    , profileQuery
    )


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))


type alias Model =
    Status


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Profile))
    | Loaded Profile
    | NotFound


initModel : LoggedIn.Model -> Model
initModel _ =
    Loading


view : LoggedIn.Model -> Model -> Html msg
view loggedIn status =
    let
        t =
            I18Next.t loggedIn.shared.translations
    in
    case status of
        Loading ->
            Page.fullPageLoading

        Loaded profile ->
            view_ loggedIn profile status

        NotFound ->
            Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

        LoadingFailed err ->
            Page.fullPageNotFound (t "error.unknown") (Page.errorToString err)


view_ : LoggedIn.Model -> Profile -> Model -> Html msg
view_ loggedIn profile status =
    let
        userName =
            Maybe.withDefault "" profile.userName

        email =
            Maybe.withDefault "" profile.email

        description =
            Maybe.withDefault "" profile.bio

        location =
            Maybe.withDefault "" profile.localization

        account =
            Eos.nameToString profile.account

        ipfsUrl =
            loggedIn.shared.endpoints.ipfs
    in
    div []
        [ Page.viewHeader loggedIn "Profile" Route.Communities
        , div
            [ class "grid pt-8 bg-white gap-4 container mx-auto"
            , style "grid-template" """
                                 ". avatar  info    info ."
                                 ". desc    desc    desc ."
                                 ". extra   extra   extra ." / 1px 84px auto auto 1px
                                 """
            ]
            [ div [ style "grid-area" "avatar" ] [ Avatar.view ipfsUrl profile.avatar "w-20 h-20" ]
            , div [ style "grid-area" "info" ] [ viewUserInfo userName email account ]
            , div [ style "grid-area" "desc" ] [ viewUserDescription description ]
            , div [ style "grid-area" "extra" ]
                [ viewUserExtendedInfo
                    [ ( "Locations", [ location ] )
                    , ( "Interests", profile.interests )
                    ]
                ]
            ]
        ]


viewUserBalance : Int -> Html msg
viewUserBalance amount =
    div
        [ class "flex flex-col justify-between items-center py-4 w-full w-full bg-gray-100 h-40 rounded"
        ]
        [ span [ class "text-sm uppercase text-green" ] [ text "User Balance" ]
        , span [ class "text-indigo-500 text-3xl font-medium" ] [ text (String.fromInt amount ++ " cr") ]
        , button
            [ style "width" "calc(100% - 16px)"
            , class "bg-orange-300 uppercase text-sm font-medium text-white h-10 rounded-lg max-w-xs"
            ]
            [ text "Transfer CR" ]
        ]


viewUserExtendedInfo : List ( String, List String ) -> Html msg
viewUserExtendedInfo data =
    div
        [ class "grid divide-y"
        ]
        (List.map
            (\x ->
                div
                    [ class "grid grid-cols-2 grid-rows-1"
                    , style "grid-template-areas" "'key value'"
                    ]
                    [ span
                        [ class "text-sm py-2 leading-6"
                        , style "grid-area" "key"
                        ]
                        [ text (Tuple.first x) ]
                    , span
                        [ class "text-indigo-500 font-medium text-sm text-right py-2 leading-6"
                        , style "grid-area" "value"
                        ]
                        [ text (String.join ", " (Tuple.second x)) ]
                    ]
            )
            data
        )


viewUserDescription : String -> Html msg
viewUserDescription content =
    p
        [ class "text-sm text-gray-900" ]
        [ text content ]


viewUserInfo : String -> String -> String -> Html msg
viewUserInfo name email username =
    let
        contentClasses =
            "text-sm text-gray-900"

        headerClasses =
            "font-medium text-2xl"
    in
    ul []
        [ li [ class headerClasses ]
            [ text name ]
        , li
            [ class contentClasses ]
            [ text email ]
        , li
            [ class contentClasses ]
            [ text username ]
        ]



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedProfileLoad (Ok Nothing) ->
            UR.init NotFound

        CompletedProfileLoad (Ok (Just profile)) ->
            UR.init (Loaded profile)

        CompletedProfileLoad (Err err) ->
            UR.init (LoadingFailed err)
                |> UR.logGraphqlError msg err


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr _ =
    case addr of
        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedProfileLoad _ ->
            [ "CompletedProfileLoad" ]
