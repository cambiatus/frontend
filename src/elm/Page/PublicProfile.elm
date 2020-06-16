module Page.PublicProfile exposing (Model, Msg, Status, init, initModel, jsAddressToMsg, msgToString, update, view, view_)

import Api.Graphql
import Avatar
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, button, div, li, p, span, text, ul)
import Html.Attributes exposing (class, style)
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
            view_ loggedIn profile True

        NotFound ->
            Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

        LoadingFailed err ->
            Page.fullPageNotFound (t "error.unknown") (Page.errorToString err)


view_ : LoggedIn.Model -> Profile -> Bool -> Html msg
view_ loggedIn profile shouldShowTransfer =
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
    div [ class "bg-white" ]
        [ Page.viewHeader loggedIn (t loggedIn.shared.translations "menu.profile") Route.Communities
        , div
            [ class "grid pt-4 gap-4 container mx-auto p-4"
            , style "grid-template"
                ("\". avatar  info    info .\""
                    ++ "\". desc    desc    desc .\""
                    ++ (if shouldShowTransfer then
                            "\". transfer    transfer    transfer .\""

                        else
                            ""
                       )
                    ++ "\". extra   extra   extra .\" / 1px 84px auto auto 1px"
                )
            ]
            ([ div [ style "grid-area" "avatar" ] [ Avatar.view ipfsUrl profile.avatar "w-20 h-20" ]
             , div [ style "grid-area" "info" ] [ viewUserInfo userName email account ]
             , div [ style "grid-area" "desc" ] [ viewUserDescription description ]
             , div [ style "grid-area" "extra" ]
                [ viewUserExtendedInfo
                    [ ( t loggedIn.shared.translations "profile.locations", [ location ] )
                    , ( t loggedIn.shared.translations "profile.interests", profile.interests )
                    ]
                ]
             ]
                ++ [ if shouldShowTransfer then
                        div [ style "grid-area" "transfer" ] [ viewTransferButton loggedIn.shared loggedIn.selectedCommunity account ]

                     else
                        div [] []
                   ]
            )
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


viewTransferButton : Shared -> Symbol -> String -> Html msg
viewTransferButton shared symbol user =
    let
        text_ s =
            text (t shared.translations s)
    in
    a
        [ class "flex justify-center w-full"
        , Route.href (Route.Transfer symbol (Just user))
        ]
        [ button
            [ class "button button-primary w-full"
            ]
            [ text_ "transfer.title" ]
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
    div
        [ class "grid"
        , style "grid-template-columns" "1fr 50px"
        , style "grid-template-areas" """
                                       "userInfo ."
                                       "userInfo editButton"
                                       "userInfo ."
                                      """
        ]
        [ ul [ style "grid-area" "userInfo" ]
            [ li [ class headerClasses ]
                [ text name ]
            , li
                [ class contentClasses ]
                [ text email ]
            , li
                [ class contentClasses ]
                [ text username ]
            ]
        , div [ style "grid-area" "editButton" ] [ Icons.edit "" ]
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
