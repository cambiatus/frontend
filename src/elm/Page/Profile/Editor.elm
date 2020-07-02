module Page.Profile.Editor exposing (Model, Msg, init, msgToString, update, view)

import Api
import Api.Graphql
import Avatar exposing (Avatar)
import File exposing (File)
import Graphql.Http
import Html exposing (Html, button, div, form, input, label, span, text, textarea)
import Html.Attributes exposing (accept, class, for, id, multiple, style, type_, value)
import Html.Events exposing (onInput)
import Http
import I18Next exposing (Translations, t)
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
    , bio : String
    , location : String
    , interests : List String
    , interest : String
    , status : Status
    , avatar : Maybe Avatar
    }


initModel : Model
initModel =
    { fullName = ""
    , email = ""
    , bio = ""
    , location = ""
    , interests = []
    , interest = ""
    , status = Loading
    , avatar = Nothing
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Profile))
    | Loaded Profile



-- VIEW


view : Session.LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            ""

        content =
            case model.status of
                Loading ->
                    Page.fullPageLoading

                LoadingFailed _ ->
                    Page.fullPageError (t loggedIn.shared.translations "profile.title") Http.Timeout

                Loaded profile ->
                    view_ loggedIn model profile
    in
    { title = title
    , content = content
    }


view_ : Session.LoggedIn.Model -> Model -> Profile -> Html Msg
view_ loggedIn model profile =
    let
        tr s =
            t loggedIn.shared.translations s

        title =
            tr "menu.edit" ++ " " ++ ("menu.profile" |> tr |> String.toLower)

        pageHeader =
            Page.viewHeader loggedIn title Route.Profile

        avatar =
            case model.avatar of
                Just avatar_ ->
                    avatar_

                Nothing ->
                    profile.avatar
    in
    Html.div [ class "bg-white" ]
        [ pageHeader
        , form
            [ class "grid pt-4 gap-4 container mx-auto p-4"
            , style "grid-template" """
                                       "avatar avatar"
                                       "fullname fullname"
                                       "email email" auto
                                       "bio bio"
                                       "location location"
                                       "interests interestButton"
                                       "interestList interestList" auto
                                       "save save" / 1fr 90px
                                      """
            ]
            ([ viewAvatar loggedIn avatar
             , viewInput (tr "profile.edit.labels.name") "fullname" FullName model.fullName
             , viewInput (tr "profile.edit.labels.email") "email" Email model.email
             , viewTextArea (tr "profile.edit.labels.bio") "bio" Bio model.bio
             , viewInput (tr "profile.edit.labels.localization") "location" Location model.location
             , viewButton (tr "profile.edit.submit") ClickedSave "save"
             , div [ class "flex flex-wrap", style "grid-area" "interestList" ]
                (model.interests
                    |> List.map viewInterest
                )
             ]
                ++ viewInterests model.interest loggedIn.shared.translations
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


viewInterests : String -> Translations -> List (Html Msg)
viewInterests interest translations =
    [ viewInput (t translations "profile.edit.labels.interests") "interests" Interest interest
    , button
        [ class "button-secondary h-12 mt-auto"
        , style "grid-area" "interestButton"
        , onClickPreventDefault AddInterest
        ]
        [ text "ADD" ]
    ]


viewInterest : String -> Html Msg
viewInterest interest =
    div [ class "bg-green px-3 h-8 rounded-sm text-xs mr-4 mb-1 flex" ]
        [ span [ class "m-auto mr-3 leading-none text-white uppercase" ] [ text interest ]
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
        , onClickPreventDefault msg
        ]
        [ text label
        ]


viewTextArea : String -> String -> Field -> String -> Html Msg
viewTextArea label gridArea field currentValue =
    div
        [ style "grid-area" gridArea
        ]
        [ span [ class "input-label" ]
            [ text label ]
        , textarea
            [ class "w-full input"
            , onInput (OnFieldInput field)
            ]
            [ text currentValue ]
        ]


viewAvatar : Session.LoggedIn.Model -> Avatar.Avatar -> Html Msg
viewAvatar loggedIn url =
    div
        [ class "m-auto relative"
        , style "grid-area" "avatar"
        ]
        [ input
            [ id "profile-upload-avatar"
            , class "profile-img-input"
            , type_ "file"
            , accept "image/*"
            , Page.onFileChange EnteredAvatar
            , multiple False
            ]
            []
        , label
            [ for "profile-upload-avatar" ]
            [ Avatar.view loggedIn.shared.endpoints.ipfs url "w-20 h-20"
            , span [ class "absolute bottom-0 right-0 bg-orange-300 w-8 h-8 p-2 rounded-full" ] [ Icons.camera ]
            ]
        ]



-- UPDATE


type Msg
    = Ignored
    | CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))
    | OnFieldInput Field String
    | AddInterest
    | RemoveInterest String
    | ClickedSave
    | EnteredAvatar (List File)
    | CompletedAvatarUpload (Result Http.Error Avatar)


type Field
    = FullName
    | Email
    | Bio
    | Location
    | Interest


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> Session.LoggedIn.Model -> UpdateResult
update msg model loggedIn =
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
                    , bio = nullable profile.bio
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

                        Bio ->
                            { model | bio = data }

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

        ClickedSave ->
            case model.status of
                Loaded profile ->
                    let
                        newProfile =
                            modelToProfile model profile
                    in
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Api.Graphql.mutation loggedIn.shared
                                (Profile.mutation profile.account (Profile.profileToForm newProfile))
                                CompletedProfileLoad
                            )
                        |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.Profile)
                        |> UR.addExt (ShowFeedback Success (t loggedIn.shared.translations "profile.edit_success"))

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        EnteredAvatar (file :: _) ->
            let
                uploadAvatar file_ =
                    Api.uploadAvatar loggedIn.shared file_ CompletedAvatarUpload
            in
            case model.status of
                Loaded profile ->
                    model
                        |> UR.init
                        |> UR.addCmd (uploadAvatar file)

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        EnteredAvatar [] ->
            UR.init model

        CompletedAvatarUpload (Ok a) ->
            UR.init { model | avatar = Just a }

        CompletedAvatarUpload (Err err) ->
            UR.init model
                |> UR.logHttpError msg err


modelToProfile : Model -> Profile -> Profile
modelToProfile model profile =
    { profile
        | userName = Just model.fullName
        , email = Just model.email
        , localization = Just model.location
        , interests = model.interests
        , bio = Just model.bio
        , avatar = Maybe.withDefault profile.avatar model.avatar
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

        ClickedSave ->
            [ "ClickedSave" ]

        CompletedAvatarUpload r ->
            [ "CompletedAvatarUpload" ]

        EnteredAvatar r ->
            [ "EnteredAvatar" ]
