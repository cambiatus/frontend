module Page.PublicProfile exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Api.Graphql
import Avatar
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, button, div, li, p, span, text, ul)
import Html.Attributes exposing (class, style)
import I18Next
import Json.Decode exposing (Value)
import Page
import Profile exposing (Profile)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import UpdateResult as UR


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn accountName =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Profile.query (Eos.stringToName accountName))
                CompletedProfileLoad
    in
    ( initModel
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


initModel : Model
initModel =
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
            view_ loggedIn profile

        NotFound ->
            Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

        LoadingFailed err ->
            Page.fullPageNotFound (t "error.unknown") (Page.errorToString err)


view_ : LoggedIn.Model -> Profile -> Html msg
view_ loggedIn profile =
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
                                 ". transfer    transfer    transfer ."
                                 ". extra   extra   extra ." / 1px 84px auto auto 1px
                                 """
            ]
            [ div [ style "grid-area" "avatar" ] [ Avatar.view ipfsUrl profile.avatar "w-20 h-20" ]
            , div [ style "grid-area" "info" ] [ viewUserInfo userName email account ]
            , div [ style "grid-area" "transfer" ] [ viewTransferButton ]
            , div [ style "grid-area" "desc" ] [ viewUserDescription description ]
            , div [ style "grid-area" "extra" ]
                [ viewUserExtendedInfo
                    [ ( "Locations", [ location ] )
                    , ( "Interests", profile.interests )
                    ]
                ]
            ]
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


viewTransferButton : Html msg
viewTransferButton =
    div
        [ class "flex justify-center"
        ]
        [ button
            [ style "width" "calc(100% - 16px)"
            , class "bg-orange-300 uppercase text-sm font-medium text-white h-10 rounded-lg max-w-xs"
            ]
            [ text "Transfer" ]
        ]


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
update msg _ _ =
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
