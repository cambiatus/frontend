module Page.PublicProfile exposing (Model, Msg, init, jsAddressToMsg, msgToString, view)

import Api
import Api.Graphql
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import I18Next exposing (t, tr)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Profile exposing (Profile, ProfileForm, decode)
import PushSubscription exposing (PushSubscription)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Session.Shared as Shared exposing (Shared)
import Task


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Profile.query loggedIn.accountName)
                CompletedProfileLoad
    in
    ( initModel loggedIn
    , Cmd.batch
        [ profileQuery
        , Task.succeed CheckPushPref |> Task.perform identity
        ]
    )


type Msg
    = CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))
    | CheckPushPref
    | GotPushSub PushSubscription
    | GotPushPreference Bool


type alias Model =
    { status : Status
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Profile))
    | Loaded Profile


initModel : LoggedIn.Model -> Model
initModel _ =
    { status = Loading
    }


view : LoggedIn.Model -> Model -> Html msg
view loggedIn model =
    div [ class "h-full flex items-center flex-col" ]
        [ viewHeader loggedIn.shared
        , div
            [ class "w-full h-full bg-white max-w-6xl"
            , style "padding-top" "16px"
            , style "display" "grid"
            , style "grid-template" """
                                 ". avatar  info    info ."
                                 ". desc    desc    desc ."
                                 ". balance balance balance ."
                                 ". extra   extra   extra ." / 1px 84px auto auto 1px
                                 """
            , style "grid-gap" "16px 16px"
            ]
            [ img
                [ class "rounded-full"
                , style "width" "84px"
                , style "height" "84px"
                , style "grid-area" "avatar"
                , src "https://b.thumbs.redditmedia.com/4_F9NWICIq_yOAFfRqg37l3n9vFs3Li5qQMyN2QzayQ.png"
                ]
                []
            , div [ style "grid-area" "info" ] [ viewUserInfo "Clara Matos" "clara@gmail.com" "clara172983" ]
            , div [ style "grid-area" "desc" ] [ viewUserDescription "My name is Clara, i work as a Designer and can help you to learn too." ]
            , div [ style "grid-area" "balance" ] [ viewUserBalance 500 ]
            , div [ style "grid-area" "extra" ]
                [ viewUserExtendedInfo
                    [ ( "Locations", [ "Costa Rica", "Brazil" ] )
                    , ( "Interests", [ "Games", "Food", "Tech", "Lord of the Rings", "Surf", "Beach" ] )
                    ]
                ]

            -- , span [ style "grid-area" "info" ] [ text "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" ]
            ]
        ]


viewUserBalance : Int -> Html msg
viewUserBalance amount =
    div
        [ class "flex flex-col justify-between items-center py-4 w-full w-full bg-gray-100 h-40"
        , style "border-radius" "24px"
        ]
        [ span [ class "text-xs uppercase text-green" ] [ text "User Balance" ]
        , span [ class "text-indigo-500 text-3xl font-medium" ] [ text (String.fromInt amount ++ " cr") ]
        , button
            [ style "width" "311px"
            , style "height" "40px"
            , style "border-radius" "40px"
            , style "width" "calc(100% - 16px)"
            , style "max-width" "311px"
            , class "bg-orange-300 uppercase text-sm font-medium text-white"
            ]
            [ text "Transfer CR" ]
        ]


viewUserExtendedInfo : List ( String, List String ) -> Html msg
viewUserExtendedInfo data =
    div
        [ style "display" "grid"
        ]
        (List.indexedMap
            (\index x ->
                let
                    isNotLast =
                        index /= List.length data - 1

                    isNotFirst =
                        index /= 0

                    notFirstStyle =
                        if isNotFirst then
                            [ style "padding-top" "16px" ]

                        else
                            []

                    notLastPadding =
                        if isNotLast then
                            [ style "border-bottom" "1px solid #E7E7E7E7"
                            ]

                        else
                            []
                in
                div
                    [ style "display" "grid"
                    , style "grid-template-columns" "50% 50%"
                    , style "grid-template-rows" "1fr"
                    , style "grid-template-areas" "'key value'"
                    ]
                    [ span
                        ([ class "text-xs"
                         , style "line-height" "24px"
                         , style "grid-area" "key"
                         , style "padding-bottom" "16px"
                         ]
                            ++ notFirstStyle
                            ++ notLastPadding
                        )
                        [ text (Tuple.first x) ]
                    , span
                        ([ class "text-indigo-500 font-medium text-xs text-right"
                         , style "line-height" "16px"
                         , style "grid-area" "value"
                         , style "padding-bottom" "16px"
                         ]
                            ++ notFirstStyle
                            ++ notLastPadding
                        )
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
            [ "text-sm", "text-gray-900" ] |> String.join " "

        headerClasses =
            [ "font-medium", "text-2xl" ] |> String.join " "
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


viewHeader : Shared -> Html msg
viewHeader shared =
    div
        [ class "h-16 w-full bg-indigo-500 flex px-4 items-center"
        , style "grid-area" "header"
        ]
        [ a
            [ class "items-center flex absolute"
            , Route.href Route.Communities
            ]
            [ Icons.back ""
            , p [ class "text-white text-sm ml-2" ]
                [ text (t shared.translations "back")
                ]
            ]
        , p [ class "text-white mx-auto" ] [ text "Profile" ]
        ]


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "RequestPush" :: _ ->
            let
                push =
                    Decode.decodeValue (Decode.field "sub" Decode.string) val
                        |> Result.andThen (Decode.decodeString Decode.value)
                        |> Result.andThen (Decode.decodeValue PushSubscription.decode)
            in
            case push of
                Ok res ->
                    Just (GotPushSub res)

                Err err ->
                    -- TODO: Handle PushSubscription Decode error
                    Nothing

        "CompletedPushUpload" :: _ ->
            decodePushPref val

        "GotPushPreference" :: _ ->
            decodePushPref val

        _ ->
            Nothing


decodePushPref : Value -> Maybe Msg
decodePushPref val =
    Decode.decodeValue (Decode.field "isSet" Decode.bool) val
        |> Result.map GotPushPreference
        |> Result.toMaybe


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedProfileLoad r ->
            [ "CompletedProfileLoad" ]

        CheckPushPref ->
            [ "CheckPushPref" ]

        GotPushSub subMsg ->
            [ "GotPushSub" ]

        GotPushPreference subMsg ->
            [ "GotPushPreference" ]



-- type Msg
--     = CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))
--     | CheckPushPref
--     | GotPushSub PushSubscription
--     | GotPushPreference Bool
