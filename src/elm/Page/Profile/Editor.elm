module Page.Profile.Editor exposing (..)

import Api.Graphql
import Avatar
import Graphql.Http
import Html exposing (Html, button, div, form, input, span, text, textarea)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import I18Next exposing (t)
import Icons
import Json.Decode
import Page
import Profile exposing (Profile)
import Route
import Session.LoggedIn exposing (External(..), FeedbackStatus(..))
import UpdateResult as UR



-- INIT


init : Session.LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Profile.query loggedIn.accountName)
                CompletedProfileLoad
    in
    ( initModel
    , profileQuery
    )



-- MODEL


type alias Model =
    { fullName : String
    , email : String
    , about : String
    , location : String
    , interests : List String
    , interest : String
    , status : Status
    }


initModel : Model
initModel =
    { fullName = ""
    , email = ""
    , about = ""
    , location = ""
    , interests = []
    , interest = ""
    , status = Loading
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Profile))
    | Loaded Profile



-- VIEW


view : Session.LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    case model.status of
        Loading ->
            Page.fullPageLoading

        LoadingFailed _ ->
            Page.fullPageError (t loggedIn.shared.translations "profile.title") Http.Timeout

        Loaded profile ->
            view_ loggedIn model profile


view_ : Session.LoggedIn.Model -> Model -> Profile -> Html Msg
view_ loggedIn model profile =
    let
        pageHeader =
            Page.viewHeader loggedIn (t loggedIn.shared.translations "menu.profile") Route.Profile

        text_ s =
            t loggedIn.shared.translations s
    in
    Html.div [ class "bg-white" ]
        [ pageHeader
        , form
            [ class "grid pt-4 gap-4 container mx-auto p-4"
            , style "grid-template" """
                                       "avatar avatar"
                                       "fullname fullname"
                                       "email email" auto
                                       "about about"
                                       "location location"
                                       "interests interestButton"
                                       "interestList interestList" auto
                                       "save save" / 1fr 90px
                                      """
            ]
            ([ viewAvatar loggedIn profile.avatar
             , viewInput (text_ "register.form.name.label") "fullname" FullName model.fullName
             , viewInput (text_ "register.form.email.label") "email" Email model.email
             , viewTextArea (text_ "About Me") "about" About
             , viewInput (text_ "profile.edit.labels.localization") "location" Location model.location
             , viewButton "Save" Ignored "save"
             , div [ class "flex flex-wrap", style "grid-area" "interestList" ]
                (model.interests
                    |> List.map viewInterest
                )
             ]
                ++ viewInterests model.interest
            )
        ]


viewInput : String -> String -> Field -> String -> Html Msg
viewInput label area field currentValue =
    div [ style "grid-area" area ]
        [ Html.label [ class "input-label" ]
            [ text label ]
        , input
            [ class "w-full input rounded-sm"
            , onInput (OnFieldInput field)
            , value currentValue
            ]
            []
        ]


viewInterests : String -> List (Html Msg)
viewInterests interest =
    [ viewInput "Interests" "interests" Interest interest
    , button
        [ class "button-secondary h-12 mt-auto"
        , style "grid-area" "interestButton"
        , onClickPreventDefault AddInterest
        ]
        [ text "ADD" ]
    ]


viewInterest : String -> Html Msg
viewInterest interest =
    div [ class "bg-green w-24 h-8 rounded-sm text-xs mr-4 mb-1 flex" ]
        [ span [ class "m-auto leading-none text-white uppercase" ] [ text interest ]
        , button
            [ class "m-auto"
            , onClickPreventDefault (RemoveInterest interest)
            ]
            [ Icons.close "w-4 h-4 text-white fill-current" ]
        ]


onClickPreventDefault : msg -> Html.Attribute msg
onClickPreventDefault message =
    Html.Events.custom "click" (Json.Decode.succeed { message = message, stopPropagation = True, preventDefault = True })


viewButton : String -> Msg -> String -> Html Msg
viewButton label msg area =
    button
        [ class "button button-primary w-full"
        , style "grid-area" area
        , onClick msg
        ]
        [ text label
        ]


viewTextArea : String -> String -> Field -> Html Msg
viewTextArea label area field =
    div
        [ style "grid-area" area
        ]
        [ span [ class "input-label" ]
            [ text label ]
        , textarea
            [ class "w-full input"
            , onInput (OnFieldInput field)
            ]
            []
        ]


viewAvatar : Session.LoggedIn.Model -> Avatar.Avatar -> Html Msg
viewAvatar loggedIn url =
    div [ class "m-auto", style "grid-area" "avatar" ]
        [ Avatar.view loggedIn.shared.endpoints.ipfs url "w-20 h-20"
        ]



-- UPDATE


type Msg
    = Ignored
    | CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))
    | OnFieldInput Field String
    | AddInterest
    | RemoveInterest String


type Field
    = FullName
    | Email
    | About
    | Location
    | Interest


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> Session.LoggedIn.Model -> UpdateResult
update msg model _ =
    case msg of
        Ignored ->
            UR.init model

        CompletedProfileLoad (Ok Nothing) ->
            UR.init model

        CompletedProfileLoad (Ok (Just profile)) ->
            let
                nullable a =
                    Maybe.withDefault "" a
            in
            UR.init
                { model
                    | status = Loaded profile
                    , fullName = nullable profile.userName
                    , email = nullable profile.email
                    , about = nullable profile.bio
                    , location = nullable profile.localization
                    , interests = profile.interests
                }

        CompletedProfileLoad (Err err) ->
            UR.init { model | status = LoadingFailed err }
                |> UR.logGraphqlError msg err

        OnFieldInput field data ->
            let
                newModel =
                    case field of
                        FullName ->
                            { model | fullName = data }

                        Email ->
                            { model | email = data }

                        About ->
                            { model | about = data }

                        Location ->
                            { model | location = data }

                        Interest ->
                            { model | interest = data }
            in
            UR.init newModel

        AddInterest ->
            let
                newModel =
                    -- Prevent empty and duplicate interests
                    if model.interest /= "" && not (List.any (\interest -> interest == model.interest) model.interests) then
                        { model | interests = model.interest :: model.interests, interest = "" }

                    else
                        model
            in
            UR.init newModel

        RemoveInterest interest ->
            UR.init
                { model
                    | interests =
                        model.interests
                            |> List.filter (\x -> x /= interest)
                }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        CompletedProfileLoad r ->
            [ "CompletedProfileLoad" ]

        OnFieldInput a f ->
            [ "OnFieldInput" ]

        AddInterest ->
            [ "AddInterest" ]

        RemoveInterest r ->
            [ "RemoveInterest" ]
